#' Apply corrections to IEA data needed for Industry subsectors
#' 
#' Apply corrections to IEA data to cope with fragmentary time series and 
#' replace outputs from blast furnaces and coke ovens, that are inputs into 
#' industry subsectors, by their respective inputs.  
#' The corrections done by this function are rather rudimentary and crude. This
#' gets smoothed away in regional aggregation. But do not use the resulting 
#' country-level data without additinonal scrutiny.
#' 
#' Use regional or global averages if IEA industry data lists energy use only as
#' "non-specified". 
#' Outputs from blast furnaces (\code{BLFURGS}, \code{OGASES}) and coke ovens 
#' (\code{OVENCOKE}, \code{COKEOVGS}, \code{COALTAR}, \code{NONCRUDE}), that are
#' inputs into industry subsectors, are replaced with the respective inputs 
#' based on regional averages. 
#' Used internally in \code{\link{calcIO}} for subtype 
#' \code{output_Industry_subsectors}.
#'
#' @param data MAgPIE object containing the IEA Energy Balances data
#' 
#' @param ieamatch mapping of IEA product/flow combinations to REMIND 
#'        \code{sety}/\code{fety}/\code{te} combinations as used in 
#'        \code{\link{calcIO}}.
#'
#' @return a MAgPIE object
#' 
#' @author Michaja Pehl
#' 
#' @importFrom rlang .data
#' @importFrom readr read_delim cols col_skip col_character
#' @importFrom quitte cartesian interpolate_missing_periods overwrite
#'             character.data.frame interpolate_missing_periods_
#' @importFrom dplyr anti_join group_by inner_join left_join mutate pull rename 
#'     summarise tbl_df 
#' @importFrom assertr not_na assert
#' @importFrom tibble as_tibble
#' @importFrom tidyr complete gather nesting_ select spread
#' @importFrom magclass getRegions getYears getNames

fix_IEA_data_for_Industry_subsectors <- function(data, ieamatch, 
                                                 distribute_losses = TRUE) {
  
  # all industry subsector flows
  flows_to_fix <- c('IRONSTL', 'CHEMICAL', 'NONFERR', 'NONMET', 'TRANSEQ', 
                    'MACHINE','MINING', 'FOODPRO', 'PAPERPRO', 'WOODPRO', 
                    'CONSTRUC', 'TEXTILES')
  
  # all products associated with those flows
  products_to_fix <- ieamatch %>%
    filter(.data$iea_flows %in% flows_to_fix) %>%
    getElement('iea_product') %>%
    unique()

  region_mapping <- toolGetMapping(name = 'regionmapping_21_EU11.csv', 
                                   type = 'regional') %>% 
    as_tibble() %>% 
    select('iso3c' = .data$CountryCode, 'region' = .data$RegionCode)

  # ---- extend industry subsector time series ----
  # subset of data containing industry subsector products and flows
  data_industry <- data[,,cartesian(products_to_fix,
                                    c(flows_to_fix, 'TOTIND', 'INONSPEC'))] %>%
    as.data.frame() %>%
    select(iso3c = 'Region', year = 'Year', product = 'Data1', flow = 'Data2',
           value = 'Value') %>%
    mutate(year = as.integer(as.character(.data$year))) %>%
    filter(0 != .data$value) %>%
    inner_join(region_mapping, 'iso3c') %>%
    assert(not_na, .data$region)

  # all products that are consumed only in the non-specified subsector of
  # industry are "suspicious" and are therefore fixed
  data_to_fix <- inner_join(
    data_industry %>%
      filter('TOTIND' != .data$flow) %>%
      group_by(.data$iso3c, .data$region, .data$year, .data$product) %>%
      summarise(total = sum(.data$value, na.rm = TRUE)) %>%
      ungroup(),

    data_industry %>%
      filter(.data$flow %in% c('TOTIND', 'INONSPEC')) %>%
      spread(.data$flow, .data$value),

    c('iso3c', 'region', 'year', 'product')
  ) %>%
    filter(  abs(1 - (.data$total / .data$TOTIND)) > 1e-3 
           | .data$INONSPEC == .data$TOTIND) %>%
    select(.data$iso3c, .data$region, .data$year, .data$product, .data$TOTIND)

  # use all non-suspicious data to calculate reginal and global averages
  data_for_fixing <- anti_join(
    data_industry %>%
      filter('TOTIND' != .data$flow),

    data_to_fix %>%
      select(-.data$TOTIND),

    c('iso3c', 'region', 'year', 'product')
  ) %>% 
    tbl_df()

  data_for_fixing <- full_join(
    # compute global averages
    data_for_fixing %>%
      group_by(.data$year, .data$product, .data$flow) %>%
      summarise(value = sum(.data$value)) %>%
      ungroup() %>%   # FIXME: dplyr 0.7.4 seems unable to group again
      group_by(.data$year, .data$product) %>%
      mutate(global_share = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-.data$value) %>%
      # and expand to all regions
      mutate(region = NA_character_) %>%
      complete(nesting_(list('year', 'product', 'flow', 'global_share')),
               region = unique(region_mapping$region)),

    # compute regional averages
    data_for_fixing %>%
      group_by(.data$year, .data$region, .data$product, .data$flow) %>%
      summarise(value = sum(.data$value)) %>%
      ungroup() %>%   # FIXME: dplyr 0.7.4 seems unable to group again
      group_by(.data$year, .data$region, .data$product) %>%
      mutate(regional_share = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-.data$value),

    c('year', 'region', 'product', 'flow')
  ) %>%
    # use regional averages if available, global averages otherwise
    mutate(value = ifelse(!is.na(.data$regional_share), .data$regional_share,
                          .data$global_share)) %>%
    select(-.data$regional_share, -.data$global_share) %>%
    interpolate_missing_periods_(
      periods = list(year = sub('^y([0-9]{4})$', '\\1', getYears(data)) %>% 
                       as.integer() %>% 
                       sort()), 
      expand.values = TRUE, method = 'linear')

  # calculated fixed data
  data_industry_fixed <- left_join(
    data_to_fix,
    data_for_fixing,
    c('region', 'year', 'product')
  ) %>%
    # replace "suspicious" data with averages
    mutate(value = .data$TOTIND * .data$value) %>%
    select(.data$iso3c, .data$region, .data$year, .data$product, .data$flow, 
           .data$value) %>%
    assert(not_na, .data$value) %>%
    overwrite(data_industry) %>%
    select(COUNTRY = .data$iso3c, TIME = .data$year, PRODUCT = .data$product, 
           FLOW = .data$flow, Value = .data$value) %>%
    as.magpie()

  # replace fixed data
  data[getRegions(data_industry_fixed),
       getYears(data_industry_fixed),
       getNames(data_industry_fixed)] <- data_industry_fixed
  
  # ---- calculate factors to replace blast furnace outputs ----
  # get subset of data pertaining to blast furnaces
  flows_BLASTFUR <- grep('^(TOTAL|MRENEW)', 
                         grep('[ET]BLASTFUR', getNames(data), value = TRUE), 
                         value = TRUE, invert = TRUE)
  
  data_BLASTFUR <- data[,,flows_BLASTFUR] %>% 
    as.data.frame() %>% 
    tbl_df() %>% 
    select(iso3c = 'Region', year = 'Year', product = 'Data1', flow = 'Data2', 
           value = 'Value') %>% 
    filter(0 != .data$value) %>% 
    mutate(year = as.integer(as.character(.data$year)))
  
  # save output products from blast furnaces for replacement further upstream
  # (coke ovens) and downstream (industry)
  outputs_BLASTFUR <- data_BLASTFUR %>% 
    filter(0 < .data$value) %>% 
    pull('product') %>% 
    unique() %>% 
    as.character()
  
  # aggregate over regions
  factors_BLASTFUR <- inner_join(data_BLASTFUR, region_mapping, 'iso3c') %>% 
    group_by(.data$region, .data$year, .data$product) %>% 
    summarise(value = sum(.data$value), .groups = 'drop')
  
  # calculate the factor with which blast furnace outputs (i.e. industry inputs) 
  # are replaced by blast furnace inputs
  factors_BLASTFUR <- inner_join(
    # inputs into blast furnaces
    factors_BLASTFUR %>% 
      filter(0 > .data$value),
    
    # outputs from blast furnaces
    factors_BLASTFUR %>% 
      filter(0 < .data$value) %>% 
      group_by(.data$region, .data$year) %>% 
      summarise(output = sum(.data$value), .groups = 'drop'),
    
    c('region', 'year')
  ) %>% 
    group_by(.data$region, .data$year, .data$product) %>% 
    summarise(factor = abs(.data$value / .data$output), .groups = 'drop')
  
  # ==== replace coke oven outputs with coke oven inputs ====
  
  # get subset of data pertaining to coke ovens
  flows_COKEOVS <- grep('^(TOTAL|MRENEW)', 
                         grep('[ET]COKEOVS', getNames(data), value = TRUE), 
                         value = TRUE, invert = TRUE)

  data_COKEOVS <- data[,,flows_COKEOVS] %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    select(iso3c = 'Region', year = 'Year', product = 'Data1', flow = 'Data2',
           value = 'Value') %>% 
    filter(0 != .data$value) %>% 
    mutate(year = as.integer(as.character(.data$year)))
  
  # save output products from coke ovens for replacement downstream (industry)
  outputs_COKEOVS <- data_COKEOVS %>% 
    filter(0 < .data$value) %>% 
    pull('product') %>% 
    unique() %>% 
    as.character()
  
  # aggregate over regions
  factors_COKEOVS <- inner_join(data_COKEOVS, region_mapping, 'iso3c') %>% 
    group_by(.data$region, .data$year, .data$product) %>% 
    summarise(value = sum(.data$value), .groups = 'drop')
  
  # replace inputs into coke ovens, that are themselves outputs of blast 
  # furnaces, by the appropriate amount of blast furnace inputs
  factors_COKEOVS <- bind_rows(
    # for each Joule of blast furnace output, use factor Joules of all blast 
    # furnace inputs instead
    factors_COKEOVS %>% 
      filter(.data$product %in% outputs_BLASTFUR) %>% 
      rename(product.replace = 'product') %>% 
      inner_join(factors_BLASTFUR, c('region', 'year')) %>% 
      group_by(.data$region, .data$year, .data$product) %>% 
      summarise(value = sum(.data$value * .data$factor), .groups = 'drop'),
    
    factors_COKEOVS %>% 
      filter(!.data$product %in% outputs_BLASTFUR)
  ) %>% 
    group_by(.data$region, .data$year, .data$product) %>% 
    summarise(value = sum(.data$value), .groups = 'drop')
  
  # calculate the factor with which coke oven outputs (i.e. industry inputs) 
  # are replaced by coke oven inputs
  factors_COKEOVS <- inner_join(
    # inputs into coke ovens
    factors_COKEOVS %>% 
      filter(0 > .data$value),
    
    # outputs from coke ovens
    factors_COKEOVS %>% 
      filter(0 < .data$value) %>% 
      group_by(.data$region, .data$year) %>% 
      summarise(output = sum(.data$value), .groups = 'drop'),
    
    c('region', 'year')
  ) %>% 
    group_by(.data$region, .data$year, .data$product) %>% 
    summarise(factor = abs(.data$value / .data$output), .groups = 'drop')
  
  # ---- replace blast furnace and coke oven outputs by respective inputs ----
  
  # get data that needs to be replaced: blast furnace and coke oven outputs that
  # are inputs in industry sectors
  REMIND_industry_flows <- c(flows_to_fix, 
                             'INONSPEC', 'AGRICULT', 'FISHING',
                             'MAINELEC', 'AUTOELEC', 'MAINCHP', 'AUTOCHP', 
                             'MAINHEAT', 'AUTOHEAT',
                             'NONENUSE')
  
  replace_product.flow <- cartesian(c(outputs_BLASTFUR, outputs_COKEOVS), 
                                    REMIND_industry_flows)
  
  data_to_fix <- data[,,replace_product.flow] %>% 
    as.data.frame() %>% 
    tbl_df() %>% 
    select(iso3c = 'Region', year = 'Year', product = 'Data1', flow = 'Data2', 
           value = 'Value') %>% 
    filter(0 != .data$value) %>% 
    mutate(year = as.integer(as.character(.data$year)))
  
  # replace blast furnace outputs
  data_for_fixing <- bind_rows(
    data_to_fix %>% 
      filter(.data$product %in% outputs_BLASTFUR,
             .data$flow %in% REMIND_industry_flows,
             0 != .data$value) %>% 
      select(-.data$product) %>% 
      inner_join(region_mapping, 'iso3c') %>% 
      inner_join(factors_BLASTFUR, c('region', 'year')) %>% 
      group_by(.data$iso3c, .data$year, .data$product, .data$flow) %>% 
      summarise(value = sum(.data$value * .data$factor), .groups = 'drop') %>% 
      character.data.frame(),
    
    data_to_fix %>% 
      filter( !.data$product %in% outputs_BLASTFUR 
            | !.data$flow %in% REMIND_industry_flows) %>% 
      character.data.frame()
  )
  
  # replace coke oven outputs
  data_for_fixing <- bind_rows(
    data_for_fixing %>% 
      filter(.data$product %in% outputs_COKEOVS,
             .data$flow %in% REMIND_industry_flows,
             0 != .data$value) %>% 
      select(-.data$product) %>% 
      inner_join(region_mapping, 'iso3c') %>% 
      inner_join(factors_COKEOVS, c('region', 'year')) %>% 
      group_by(.data$iso3c, .data$year, .data$product, .data$flow) %>% 
      summarise(value = sum(.data$value * .data$factor), .groups = 'drop') %>% 
      character.data.frame(),
    
    data_for_fixing %>% 
      filter( !.data$product %in% outputs_COKEOVS 
            | !.data$flow %in% REMIND_industry_flows)
  ) %>% 
    group_by(.data$iso3c, .data$year, .data$product, .data$flow) %>% 
    summarise(value = sum(.data$value), .groups = 'drop') %>% 
    character.data.frame() %>% 
    rename(Region = 'iso3c', Year = 'year', Data1 = 'product', Data2 = 'flow',
           Value = 'value') %>%
    as.magpie(spatial = 'Region', temporal = 'Year')
  
  # replace NAs with zeros
  data_for_fixing[is.na(data_for_fixing)] <- 0
  
  # replace
  regions_keep <- getRegions(data)
  years_keep   <- getYears(data)
  names_keep   <- setdiff(getNames(data), replace_product.flow)
  
  regions_replace <- getRegions(data_for_fixing)
  years_replace   <- getYears(data_for_fixing)
  names_replace   <- getNames(data_for_fixing)
  
  data_fixed <- new.magpie(cells_and_regions = sort(regions_keep), 
                    years = sort(years_keep),
                    names = unique(c(names_keep, names_replace)),
                    fill = 0)
  
  data_fixed[regions_keep,years_keep,names_keep] <- (
    data[regions_keep,years_keep,names_keep])
  
  data_fixed[regions_replace,years_replace,names_replace] <- (
      data_fixed[regions_replace,years_replace,names_replace]
    + data_for_fixing[regions_replace,years_replace,names_replace])
  
  return(data_fixed)
}
