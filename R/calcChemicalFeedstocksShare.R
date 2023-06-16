#' Calculate Chemical Feedstock share projections
#' 
#' Calculates the share of `CHEMICAL` in `CHEMICAL = NECHEM` and converges it
#' towards the maximum value of either OECD or non-OECD countries by 2050.
#' 
#' @md
#' @return A list with a [`magpie`][magclass::magclass] object `x`, `weight`,
#'   `unit`, `description`, `min`, and `max`.
#'   
#' @author Michaja Pehl
#' 
#' @seealso [`calcOutput()`]
#' 
#' @importFrom assertr assert not_na
#' @importFrom dplyr filter group_by inner_join mutate pull select summarise
#' @importFrom quitte character.data.frame interpolate_missing_periods_ 
#' @importFrom rlang .data sym
#' @importFrom tibble as_tibble
#' @importFrom tidyr complete everything nesting 

#' @export
calcChemicalFeedstocksShare <- function() 
{
  region_mapping <- toolGetMapping(
    name = 'regionmapping_21_EU11.csv',
    type = 'regional', where = "mappingfolder") %>%
    as_tibble() %>%
    select(region = 'RegionCode', iso3c = 'CountryCode')
  
  OECD_iso3c <- toolGetMapping(name = 'regionmappingOECD.csv', type = 'regional', where = "mappingfolder") %>%
    as_tibble() %>% 
    select(iso3c = 'CountryCode', region = 'RegionCode') %>% 
    filter('OECD' == .data$region) %>% 
    pull('iso3c')
  
  # get CHEMICAL and NECHEM data ----
  CHEMICAL_NECHEM <- readSource(type = 'IEA', 'EnergyBalances') %>% 
    `[`(,,paste('TOTAL', c('CHEMICAL', 'NECHEM'), sep = '.')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    select(iso3c = 'Region', year = 'Year', flow = 'Data2', value = 'Value') %>%
    character.data.frame() %>%
    mutate(year = as.integer(.data$year)) %>% 
    filter(0 != .data$value) %>% 
    filter(max(.data$year) - 20 <= .data$year) %>% 
    inner_join(region_mapping, 'iso3c')
  
  convergence_share <- c(
    # max share in OECD countries of last 20 year
    CHEMICAL_NECHEM %>% 
      filter(.data$iso3c %in% OECD_iso3c) %>% 
      group_by(.data$year) %>% 
      summarise(share = sum(.data$value['NECHEM' == .data$flow])
                / sum(.data$value),
                .groups = 'drop') %>% 
      pull('share') %>% 
      max(na.rm = TRUE),
    
    # max share in non-OECD countries of last 20 years
    CHEMICAL_NECHEM %>% 
      filter(!.data$iso3c %in% OECD_iso3c) %>% 
      group_by(.data$year) %>% 
      summarise(share = sum(.data$value['NECHEM' == .data$flow])
                / sum(.data$value),
                .groups = 'drop') %>% 
      pull('share') %>% 
      max(na.rm = TRUE)
  ) %>% 
    max() %>% 
    round(2)
  
  x <- CHEMICAL_NECHEM %>% 
    group_by(.data$region, .data$year) %>% 
    summarise(share = sum(.data$value['NECHEM' == .data$flow])
              / sum(.data$value),
              .groups = 'drop') %>% 
    complete(nesting(!!sym('region')), year = c(unique(.data$year), 2050),
             fill = list(share = convergence_share)) %>% 
    interpolate_missing_periods_(
      periods = list(year = min(CHEMICAL_NECHEM$year):2150),
      value = 'share', expand.values = TRUE) %>%
    full_join(region_mapping, 'region') %>%
    select('iso3c', 'year', 'share') %>%
    assert(not_na, everything())
  
  weight <- calcOutput('GDP', aggregate = FALSE) %>%
    `[`(,,'gdp_SSP2') %>%
    dimSums(dim = 3)

  return(
    list(
    x = x %>%
      filter(.data$year %in% unique(quitte::remind_timesteps$period)) %>%
      as.magpie(spatial = 1, temporal = 2, tidy = TRUE) %>% 
      collapseDim(dim = 3), 
    
    weight = weight[,unique(quitte::remind_timesteps$period),],
    
    unit = 'share',
    description = 'Share of feedstocks in chemicals FE input',
    min = 0, max = 1)
  )
}

# product_mapping <- toolGetMapping(
#   name = 'structuremappingIO_outputs_Industry_subsectors.csv',
#   type = 'sectoral') %>% 
#   as_tibble() %>% 
#   select(product = 'iea_product', flow = 'iea_flows', pf = 'REMINDitems_out',
#          weight = 'Weight') %>% 
#   filter('CHEMICAL' == .data$flow) %>% 
#   right_join(
#     tribble(
#       ~pf,   ~group,
#       'feso_chemicals',   'solids',
#       'feli_chemicals',   'liquids',
#       'fega_chemicals',   'gases'),
#     
#     'pf'
#   ) %>% 
#   select('product', 'group', 'weight')

# # FIXME: emission factors from generisdata_emi.prn; replace with numbers
# # hard-coded into this (or some) package
# emission_factors <- tribble(
#   ~group,      ~emission.factor,
#   'solids',    26.1,
#   'liquids',   20,
#   'gases',     15.3) %>% 
#   # GtC/ZJ * 1000 ZJ/EJ = GtC/EJ
#   mutate(emission.factor = .data$emission.factor * 1000)
