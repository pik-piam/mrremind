#' EDGE-Industry
#' 
#' Functions for calculating industry activity trajectories.
#' 
#' @md
#' @param match.historic.values Should steel production trajectories match 
#'   historic values?
#' @param match.estimates Should steel production trajectories match exogenous
#'   estimates?  `NULL` or one of
#'   - `IEA_ETP` IEA 2017 Energy Transition Pathways steel production totals for
#'     OECD and Non-OECD countries from the _Reference Technologies Scenario_
#'     until 2060, and original growth rates after that.
#'
#' @return A list with a [`magpie`][magclass::magclass] object `x`, `weight`,
#'   `unit`, `description`, `min`, and `max`.
#'   
#' @author Michaja Pehl
#'
#' @seealso [`calcOutput()`]
#'
#' @importFrom assertr assert verify
#' @importFrom broom tidy
#' @importFrom car logit
#' @importFrom dplyr %>% case_when right_join semi_join
#' @importFrom Hmisc wtd.quantile
#' @importFrom quitte calc_mode duplicate duplicate_ list_to_data_frame 
#'   madrat_mule sum_total_
#' @importFrom stats SSlogis nls
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom zoo rollmean

# setup
# library(broom)
# library(car)
# library(Hmisc)
# library(assertr)
# library(tidyverse)
# library(quitte)
# library(mrremind)
# library(zoo)

#' @rdname EDGE-Industry
#' @export
calcSteel_Projections <- function(match.historic.values = TRUE, 
                                  match.estimates = NULL) {
  
  . <- NULL
  
  # table of units ----
  # GDPpC = $/year
  # GDPpC_history = $/year
  # population_history = people

  
  steel.match.estimates.baseline.scenario <- 'SSP2'
  # get EDGE-Industry switches ----
  # FIXME: remove before deploying
  # load('./R/sysdata.rda')
  `EDGE-Industry_scenario_switches` <- EDGE_scenario_switches %>% 
    select(
      'scenario', 
      `steel.stock.estimate` = 'EDGE-Industry_steel.stock.estimate',
      `scenario.mask.OECD` = 
        'EDGE-Industry_scenario.mask.OECD',
      `scenario.mask.non-OECD` = 
        'EDGE-Industry_scenario.mask.non-OECD',
      `steel.stock.lifetime.base.scenario` = 
        'EDGE-Industry_steel.stock.lifetime.base.scenario',
      `steel.stock.lifetime.convergence.year` = 
        'EDGE-Industry_steel.stock.lifetime.convergence.year',
      `steel.stock.lifetime.convergence.factor` = 
        'EDGE-Industry_steel.stock.lifetime.convergence.factor')
  
  # load required data ----
  ## region mapping for aggregation ----
  region_mapping <- toolGetMapping(name = 'regionmapping_21_EU11.csv',
                                   type = 'regional') %>% 
    as_tibble() %>% 
    select(region = 'RegionCode', iso3c = 'CountryCode')
  
  ### extra region mapping for Belgium-Luxembourg ----
  region_mapping__Belgium_Luxembourg <- region_mapping %>% 
    filter(.data$iso3c %in% c('BEL', 'LUX')) %>% 
    distinct(.data$region) %>% 
    verify(1 == length(.data$region)) %>% 
    mutate(iso3c = 'blx')
  
  ## country mapping for Müller data ----
  country_mapping <- readSource(type = 'Mueller', subtype = 'countries',
                                convert = FALSE) %>% 
    madrat_mule()
  
  ## steel stock lifetimes ----
  lifetime <- readSource(type = 'Pauliuk', subtype = 'lifetime', 
                         convert = FALSE) %>% 
    madrat_mule()
  
  ### add iso3c codes ----
  lifetime <- inner_join(
    lifetime,
    
    country_mapping %>%
      mutate(country = ifelse(.data$iso3c %in% c('BEL', 'FRA', 'LUX', 'NLD'),
                              'France+Benelux', .data$country)),
    
    'country'
  )
  
  ## set of OECD countries ----
  OECD_iso3c <- toolGetMapping(name = 'regionmappingOECD.csv',
                               type = 'regional') %>%
    as_tibble() %>% 
    select(iso3c = 'CountryCode', region = 'RegionCode') %>% 
    filter('OECD' == .data$region) %>% 
    pull('iso3c')
  
  
  
  ## historic per-capita steel stock estimates ----
  steel_stock_per_capita <- readSource(type = 'Mueller', subtype = 'stocks',
                                       convert = FALSE) %>% 
    madrat_mule() %>% 
    # remove Netherlands Antilles, only use Bonaire, Sint Eustatius and Saba; 
    # Curaçao; and Sint Maarten (Dutch part)
    filter('ANT' != .data$iso3c)
    
  ## historic per-capita GDP ----
  GDPpC_history <- readSource(type = 'James', subtype = 'IHME_USD05_PPP_pc', 
                              convert = FALSE) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    select(iso3c = .data$Region, year = .data$Year, GDPpC = .data$Value) %>% 
    character.data.frame() %>% 
    mutate(year = as.integer(.data$year))
    
  ## historic population ----
  population_history <- calcOutput(type = 'PopulationPast', 
                                   PopulationPast = 'UN_PopDiv', 
                                   aggregate = FALSE) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    select(iso3c = .data$Region, year = .data$Year, 
           population = .data$Value) %>% 
    character.data.frame() %>% 
    mutate(year = as.integer(.data$year),
           # million people * 1e6/million = people
           population = .data$population * 1e6)
  
  ## GDP projections ----
  GDP <- calcOutput(type = 'GDPppp', FiveYearSteps = FALSE, 
                    aggregate = FALSE) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    select(scenario = .data$Data1, iso3c = .data$Region, year = .data$Year, 
           GDP = .data$Value) %>% 
    character.data.frame() %>% 
    mutate(scenario = sub('^gdp_', '', .data$scenario),
           year = as.integer(.data$year),
           # $m * 1e6 $/$m = $
           GDP = .data$GDP * 1e6)
  
  ## population ----
  population <- calcOutput('Population', FiveYearSteps = FALSE,
                           aggregate = FALSE) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    select(scenario = .data$Data1, iso3c = .data$Region, year = .data$Year, 
           population = .data$Value) %>% 
    character.data.frame() %>% 
    mutate(scenario = sub('^pop_', '', .data$scenario),
           year = as.integer(.data$year),
           # million people * 1e6/million = people
           population = .data$population * 1e6)
  
  # estimate steel stock distribution ----
  regression_data <- steel_stock_per_capita %>% 
    inner_join(GDPpC_history, c('iso3c', 'year')) %>% 
    inner_join(population_history, c('iso3c', 'year'))
  
  regression_parameters <- tibble()
  for (.estimate in unique(regression_data$estimate)) {
    
    Asym <- regression_data %>% 
      filter(.estimate == .data$estimate) %>% 
      group_by(.data$year) %>%
      summarise(Asym = 1.1 * wtd.quantile(x = .data$steel.stock.per.capita,
                                          weights = .data$population,
                                          probs = 0.99),
                .groups = 'drop') %>% 
      pull('Asym') %>% 
      max()

    coefficients <- lm(
      formula = logit(x, adjust = 0.025) ~ y,
      data = regression_data %>%
        filter(.estimate == .data$estimate,
               between(.data$steel.stock.per.capita, 0, Asym)) %>%
        mutate(x = .data$steel.stock.per.capita  / Asym) %>%
        select(.data$x, y = .data$GDPpC)
    ) %>%
      getElement('coefficients') %>%
      setNames(NULL)
    
    xmid <- -coefficients[1] / coefficients[2]
    scal <- 1 / coefficients[2]
    
    regression_parameters <- bind_rows(
      regression_parameters,
      
      nls(formula = steel.stock.per.capita
          ~ Asym / (1 + exp((xmid - GDPpC) / scal)),
          weights = population,
          data = regression_data %>%
            filter(.estimate == .data$estimate),
          start = list(Asym = Asym, xmid = xmid, scal = scal),
          algorithm = 'port',
          trace = FALSE) %>%
        tidy() %>%
        select('term', 'estimate') %>%
        pivot_wider(names_from = 'term', values_from = 'estimate') %>% 
        mutate(estimate = .estimate)
    )
  }
  
  # estimate future steel stocks ----
  steel_stock_estimates <- full_join(
    # GDP, population to calculate per-capita GDP
    full_join(GDP, population, c('scenario', 'iso3c', 'year')) %>%
      assert(not_na, everything()),
    
    # regression parameters mapped to GDP and population scenarios
    full_join(
      regression_parameters,
      
      `EDGE-Industry_scenario_switches` %>%
        select('scenario', estimate = 'steel.stock.estimate'),

      'estimate'
    ),
    
    'scenario'
  ) %>% 
    # make sure all scenarios have associated regression parameters
    assert(
      not_na, .data$Asym, .data$scal, .data$xmid, 
      error_fun = function(errors, data) { 
        rows <- lapply(errors, function(x) { x$error_df$index }) %>% 
          unlist() %>% 
          unique()
        message <- paste0('Unmatched estimates for steel projection regression',
                          'parameters')
        stop(paste(c(message, format(head(as.data.frame(data[rows,])))),
                   collapse = '\n'),
             call. = FALSE)
      }) %>% 
    # calculate steel stock estimates using logistic function
    mutate(
      value = SSlogis(input = .data$GDP / .data$population, 
                      Asym = .data$Asym, xmid = .data$xmid, scal = .data$scal),
      source = 'computation') %>%
    select('scenario', 'iso3c', 'year', 'value', 'source') %>% 
    assert(not_na, everything())
  
  steel_stock_estimates <- bind_rows(
    steel_stock_estimates,
    
    steel_stock_per_capita %>%
      filter(.data$year >= min(steel_stock_estimates$year)) %>%
      full_join(
        `EDGE-Industry_scenario_switches` %>%
          select('scenario', estimate = 'steel.stock.estimate'),
        
        'estimate'
      ) %>%
      select(.data$scenario, .data$iso3c, .data$year, 
             value = .data$steel.stock.per.capita) %>%
      mutate(source = 'Pauliuk')
  ) %>% 
    full_join(region_mapping, 'iso3c') %>% 
    pivot_wider(names_from = 'source') %>% 
    assert(not_na, .data$computation,
           error_fun = function(errors, data) { 
             rows <- lapply(errors, function(x) { x$error_df$index }) %>% 
               unlist() %>% 
               unique()
             message <- paste('Mismatch between Pauliuk and estimation',
                              'regions')
             stop(paste(c(message, format(head(as.data.frame(data[rows,])))),
                        collapse = '\n'),
                  call. = FALSE)
           })
  
  # TODO: harmonise estimates for historic time steps between scenarios, so as
  # to having identical estimates between SSP1/2/5/... up to 2020

  ## smooth transition ----
  # from Pauliuk data to per-capita GDP-based estimates over 30 years  
  fade_end   <- max(steel_stock_per_capita$year)
  fade_start <- fade_end - 30
  
  steel_stock_estimates <- steel_stock_estimates %>% 
    mutate(
      l = pmax(0, pmin(1, (.data$year - fade_start) / (fade_end - fade_start))),
      mix = .data$l * .data$computation + (1 - .data$l) * .data$Pauliuk,
      steel.stock.per.capita = ifelse(is.na(.data$mix), 
                                      .data$computation, .data$mix)) %>%
    select('scenario', 'iso3c', 'region', 'year', 'steel.stock.per.capita') %>% 
    assert(not_na, everything())
  
  rm(list = c('fade_start', 'fade_end'))
  
  ## update SSP4 ----
  # SSP4 uses SSP2 estimates for OECD countries and SSP1 estimates for non-OECD
  # countries
  steel_stock_estimates <- bind_rows(
    # non-masked scenarios
    steel_stock_estimates %>% 
      anti_join(
        `EDGE-Industry_scenario_switches` %>% 
          select(.data$scenario, 
                 .data$scenario.mask.OECD, .data$`scenario.mask.non-OECD`) %>% 
          filter(  !is.na(.data$scenario.mask.OECD) 
                 & !is.na(.data$`scenario.mask.non-OECD`)) %>% 
          select(.data$scenario),
        
        'scenario'
      ) %>% 
      assert(not_na, everything()),
    
    # masked scenarios, OECD countries
    left_join(
      `EDGE-Industry_scenario_switches` %>% 
        select('scenario', 'scenario.mask.OECD') %>% 
        filter(!is.na(.data$scenario.mask.OECD)) %>% 
        rename(scenario.mask = 'scenario',
               scenario = 'scenario.mask.OECD'),
      
      steel_stock_estimates %>% 
        filter(.data$iso3c %in% OECD_iso3c), 
      
      'scenario'
    ) %>% 
      select(-'scenario', 'scenario' = 'scenario.mask') %>% 
      assert(not_na, everything()),
    
    # masked scenarios, non-OECD countries
    left_join(
      `EDGE-Industry_scenario_switches` %>% 
        select('scenario', 'scenario.mask.non-OECD') %>% 
        filter(!is.na(.data$`scenario.mask.non-OECD`)) %>% 
        rename(scenario.mask = 'scenario',
               scenario = 'scenario.mask.non-OECD'),
      
      steel_stock_estimates %>% 
        filter(!.data$iso3c %in% OECD_iso3c), 
      
      'scenario'
    ) %>% 
      select(-'scenario', 'scenario' = 'scenario.mask') %>% 
      assert(not_na, everything())
  ) %>% 
    assert(not_na, everything())
  
  ## calculate regional and global totals, as well as absolute stocks ----
  steel_stock_estimates <- steel_stock_estimates %>% 
    assert(not_na, everything()) %>% 
    full_join(population, c('scenario', 'iso3c', 'year')) %>% 
    group_by(.data$scenario, .data$year, .data$region) %>% 
    sum_total_(group = 'iso3c', value = 'steel.stock.per.capita', 
               weight = 'population') %>%
    ungroup(.data$region) %>% 
    sum_total_(group = 'iso3c', value = 'steel.stock.per.capita', 
               weight = 'population') %>%
    ungroup() %>%
    filter(!(.data$region == 'World' & .data$iso3c != 'Total')) %>%
    # absolute stocks
    mutate(steel.stock = .data$steel.stock.per.capita * .data$population) %>% 
    assert(not_na, everything())
  
  # calculate lifetime projections ----
  
  # steel stock lifetimes are projected to converge from regional averages in 
  # 2010 towards the global average in 2100
  lifetime_regions <- lifetime %>%
    select(.data$iso3c, .data$lifetime) %>% 
    full_join(filter(GDP, 2010 == .data$year), 'iso3c') %>% 
    inner_join(region_mapping, 'iso3c') %>% 
    filter(!is.na(.data$lifetime)) %>% 
    group_by(.data$scenario, .data$region) %>% 
    summarise(
      lifetime = round(sum(.data$lifetime * .data$GDP) / sum(.data$GDP)),
      .groups = 'drop')
  
  lifetime_global <- lifetime %>%
    select(.data$iso3c, .data$lifetime) %>% 
    full_join(filter(GDP, 2010 == .data$year), 'iso3c') %>%
    inner_join(region_mapping, 'iso3c') %>%
    filter(!is.na(lifetime)) %>%
    group_by(.data$scenario) %>%
    summarise(
      lifetime = round(sum(.data$lifetime * .data$GDP) / sum(.data$GDP)),
      .groups = 'drop')

  lifetime_projections <- inner_join(
    lifetime_regions %>%
      rename(`2010` = .data$lifetime),
    
    lifetime_global %>%
      mutate(region = 'World') %>%
      complete(nesting(!!sym('scenario'), !!sym('lifetime')),
               region = unique(region_mapping$region)) %>%
      rename(`2100` = .data$lifetime),
    
    c('scenario', 'region')
  ) %>%
    pivot_longer(c(.data$`2010`, .data$`2100`), 
                 names_to = 'year', names_transform = list(year = as.integer),
                 values_to = 'lifetime', 
                 values_transform = list(lifetime = as.numeric))
  
  # steel stock lifetimes for specific scenarios in 2100 can be defined relative
  # to the lifetime of a <base.scenario> in a <convergence.year>, times a 
  # <convergence.factor>
  lifetime_projections <- bind_rows(
    lifetime_projections %>% 
      filter(2010 == .data$year),
    
    inner_join(
      `EDGE-Industry_scenario_switches` %>% 
        select(
          .data$scenario,
          base.scenario      = .data$steel.stock.lifetime.base.scenario,
          convergence.year   = .data$steel.stock.lifetime.convergence.year,
          convergence.factor = .data$steel.stock.lifetime.convergence.factor
        ) %>% 
        mutate(convergence.factor = as.numeric(.data$convergence.factor),
               convergence.year   = as.integer(.data$convergence.year)),
      
      lifetime_projections,
      
      c('base.scenario' = 'scenario', 'convergence.year' = 'year')
    ) %>% 
      mutate(lifetime = round(.data$convergence.factor * .data$lifetime),
             year = 2100) %>% 
      select('scenario', 'region', 'year', 'lifetime')
  ) %>%
    interpolate_missing_periods_(periods = list(year = 1950:2150), 
                                 value = 'lifetime', expand.values = TRUE)
  
  # calculate steel trade ----
  steel_yearbook_data <- madrat_mule(readSource('worldsteel', convert = FALSE))
  
  ## compute historic steel values ----
  steel_historic <- bind_rows(
    # combine Belgium and Luxembourg, because apparent steel use is reported 
    # for both together
    steel_yearbook_data %>% 
      filter(!.data$iso3c %in% c('BEL', 'LUX')),
    
    steel_yearbook_data %>% 
      filter(.data$iso3c %in% c('BEL', 'LUX')) %>% 
      group_by(.data$name, .data$year) %>% 
      summarise(value = sum(.data$value, na.rm = TRUE), 
                iso3c = 'blx',
                .groups = 'drop')
  ) %>% 
    # rename to shorter variable names
    inner_join(
      tribble(
        ~name,                                           ~variable,
        'Apparent Steel Use (Crude Steel Equivalent)',   'use',
        'Total Production of Crude Steel',               'production',
        'Production in Oxygen-Blown Converters',         'prod.BOF',
        'Production in Open Hearth Furnaces',            'prod.OHF',
        'Production in Electric Arc Furnaces',           'prod.EAF',
        'Pig Iron Production',                           'prod.pig',
        'DRI Production',                                'prod.DRI'),
      
      'name'
    ) %>% 
    select('iso3c', 'variable', 'year', 'value') %>% 
    # kt/year * 1e-3 Mt/kt = Mt/year
    mutate(value = .data$value * 1e-3) %>%
    pivot_wider(names_from = 'variable') %>% 
    
    mutate(imports = pmax(0, .data$use - .data$production),
           exports = pmin(0, .data$use - .data$production)) %>% 
    pivot_longer(cols = c(-'iso3c', -'year'), names_to = 'variable',
                 values_drop_na = TRUE) %>% 
    # add region mapping
    inner_join(
      bind_rows(
        region_mapping, 
        region_mapping__Belgium_Luxembourg),
      
      'iso3c')
  
  ## compute regional/global aggregates ----
  steel_historic <- bind_rows(
    steel_historic,
    
    steel_historic %>% 
      group_by(.data$region, .data$year, .data$variable) %>% 
      summarise(value = sum(.data$value, na.rm = TRUE),
                iso3c = 'Total',
                .groups = 'drop'),
    
    steel_historic %>% 
      group_by(.data$year, .data$variable) %>% 
      summarise(value = sum(.data$value, na.rm = TRUE),
                region = 'World',
                .groups = 'drop')
  )
  
  ## compute trade shares ----
  # calculate regional trade shares
  steel_trade_shares_regional <- steel_historic %>% 
    filter('Total' != .data$iso3c, 
           .data$variable %in% c('use', 'imports', 'exports')) %>%
    # exclude regions that don't have valid import/export data
    group_by(.data$iso3c, .data$year) %>% 
    filter(3 == n()) %>% 
    group_by(.data$region, .data$year, .data$variable) %>% 
    summarise(value = sum(.data$value), .groups = 'drop') %>% 
    pivot_wider(names_from = 'variable') %>% 
    mutate(import.share = .data$imports / .data$use,
           export.share = .data$exports / .data$use) %>% 
    select('region', 'year', 'import.share', 'export.share') %>% 
    pivot_longer(c('import.share', 'export.share'), names_to = 'variable')
  
  # calculate country trade shares, defaulting to regional shares
  steel_trade_shares <- steel_historic %>% 
    filter(.data$variable %in% c('imports', 'exports', 'use'),
           !('Total' == .data$iso3c & 'use' != .data$variable)) %>% 
    pivot_wider(names_from = 'variable') %>% 
    full_join(
      steel_trade_shares_regional %>% 
        mutate(variable = paste0(.data$variable, '.regional')) %>% 
        pivot_wider(names_from = 'variable') %>% 
        inner_join(region_mapping, 'region'),
      
      c('region', 'iso3c', 'year')
    ) %>% 
    mutate(
      import.share = ifelse(!is.na(.data$imports), 
                            .data$imports / .data$use,
                            .data$import.share.regional),
      export.share = ifelse(!is.na(.data$exports), 
                            .data$exports / .data$use,
                            .data$export.share.regional),
      imports      = ifelse(!is.na(.data$imports), 
                            .data$imports, 
                            .data$use * .data$import.share),
      exports      = ifelse(!is.na(.data$exports), 
                            .data$exports, 
                            .data$use * .data$export.share),
      trade        = .data$imports + .data$exports,
      trade.share  = ifelse(!is.na(.data$use), 
                            .data$trade / .data$use, 
                            .data$import.share + .data$export.share)) %>% 
    select('iso3c', 'region', 'year', 'import.share', 'export.share', 
           'trade.share')

  # calculate steel production ----
  steel_trade_share_2015 <- steel_trade_shares %>% 
    filter('Total' != .data$iso3c, 
           2015 == .data$year) %>% 
    select('region', 'iso3c', 'trade.share')
  
  # duplicate Belgium and Luxembourg from Belgium-Luxembourg
  steel_trade_share_2015 <- bind_rows(
    steel_trade_share_2015 %>% 
      filter(!.data$iso3c %in% c('blx', 'BEL', 'LUX')),
    
    steel_trade_share_2015 %>% 
      filter('blx' == .data$iso3c) %>%
      pivot_wider(names_from = 'iso3c', values_from = 'trade.share') %>%
      mutate(LUX = .data$blx) %>% 
      rename(BEL = .data$blx) %>% 
      pivot_longer(-'region', names_to = 'iso3c', values_to = 'trade.share')
  )
  
  ## calculate primary and secondary production ----
  production_estimates <- steel_stock_estimates %>%
    filter('Total' != .data$iso3c) %>% 
    inner_join(steel_trade_share_2015 %>% select(-'region'), 'iso3c') %>% 
    left_join(lifetime_projections, c('scenario', 'region', 'year')) %>%
    select(-'steel.stock.per.capita', -'population') %>% 
    group_by(.data$scenario, .data$region, .data$iso3c) %>% 
    assert(not_na, everything()) %>% 
    mutate(
      stock.additions = rollmean(
        pmax(0, .data$steel.stock - lag(.data$steel.stock)),
        k = 5, fill = 'extend', na.rm = TRUE),
      depriciation    = lag(.data$steel.stock) / lag(.data$lifetime),
      recyclable      = 0.9 * .data$depriciation, # FIXME: pull parameter out
      new.stock       = .data$stock.additions + .data$depriciation,
      trade           = .data$new.stock * .data$trade.share) %>% 
    group_by(.data$scenario, .data$year) %>% 
    mutate(m.factor = ( sum(.data$trade, na.rm = TRUE) 
                      / sum(abs(.data$trade), na.rm = TRUE)
                      )) %>% 
    group_by(.data$scenario, .data$region, .data$iso3c) %>% 
    mutate(
      adj.trade            = ( .data$trade 
                             * ifelse(0 < .data$trade, 1 - .data$m.factor, 
                                       1 + .data$m.factor)
                             ),
      adj.trade.share      = .data$trade / .data$new.stock,
      production           = .data$new.stock - .data$adj.trade,
      secondary.production = pmin(0.9 * .data$production, .data$recyclable),
      primary.production   = .data$production - .data$secondary.production) %>% 
    ungroup() %>% 
    select('scenario', 'region', 'iso3c', 'year', 'steel.stock', 'depriciation', 
           'primary.production', 'secondary.production', 
           trade = 'adj.trade') %>% 
    filter(min(.data$year) < .data$year) %>% 
    pivot_longer(c('steel.stock', 'depriciation', 'primary.production', 
                   'secondary.production', 'trade'),
                 names_to = 'variable') %>% 
    group_by(.data$scenario, .data$region, .data$year, .data$variable) %>% 
    sum_total_('iso3c') %>%
    ungroup()
  
  ## calculate production limits of secondary steel----
  # FIXME: move to separate function
  production_limits <- production_estimates %>% 
    filter('depriciation' == .data$variable) %>% 
    select(-'variable')
  
  ## construct output ----
  x <- production_estimates %>% 
    semi_join(region_mapping, c('region', 'iso3c')) %>% 
    filter(min(steel_historic_prod$year) <= .data$year) %>% 
    right_join(
      tribble(
        ~variable,                ~pf,
        'primary.production',     'ue_steel_primary',
        'secondary.production',   'ue_steel_secondary'),
      
      'variable'
    ) %>% 
    assert(not_na, everything()) %>% 
    # t/year * 1e-6 Mt/t = Mt/year
    mutate(value = .data$value * 1e-6) %>% 
    select('scenario', 'iso3c', 'pf', 'year', 'value') %>% 
    as.magpie(spatial = 2, temporal = 4, data = 5)
  
  # modify estimates to match historic values ----
  if (match.historic.values) {
    ## aggregate primary and secondary production ----
    steel_historic_prod <- steel_historic %>% 
      filter(!is.na(.data$iso3c),
             .data$variable %in% c('production', 'prod.BOF', 'prod.OHF',
                                   'prod.EAF', 'prod.DRI')) %>% 
      pivot_wider(names_from = 'variable', values_fill = 0) %>% 
      mutate(
        primary.production   = .data$prod.BOF + .data$prod.OHF + .data$prod.DRI,
        # TODO: for VEN & IRN DRI > EAF -- figure out what is going on
        secondary.production = pmax(0, .data$prod.EAF - .data$prod.DRI),
        primary.production   = .data$primary.production 
        * .data$production
        / ( .data$primary.production 
            + .data$secondary.production),
        secondary.production = .data$secondary.production 
        * .data$production
        / ( .data$primary.production 
            + .data$secondary.production)) %>% 
      select('iso3c', 'region', 'year', 'primary.production', 
             'secondary.production') %>% 
      pivot_longer(cols = c('primary.production', 'secondary.production'),
                   names_to = 'variable') %>% 
      filter(0 != .data$value)
    
    ## split Belgium and Luxembourg by population ----
    steel_historic_prod <- bind_rows(
      steel_historic_prod %>% 
        filter('blx' != .data$iso3c),
      
      steel_historic_prod %>% 
        filter('blx' == .data$iso3c) %>% 
        select(-'region') %>% 
        left_join(
          population %>% 
            filter(.data$iso3c %in% c('BEL', 'LUX'),
                   .data$year %in% unique(steel_historic_prod$year)) %>% 
            group_by(.data$year, .data$scenario) %>% 
            summarise(population = sum(.data$population), 
                      .groups = 'drop_last') %>% 
            summarise(population = mean(.data$population), .groups = 'drop'),
          
          'year'
        ) %>% 
        mutate(value = .data$value / .data$population) %>% 
        select('year', 'variable', 'value') %>% 
        left_join(
          population %>% 
            filter(.data$iso3c %in% c('BEL', 'LUX'),
                   .data$year %in% unique(steel_historic_prod$year)) %>% 
            inner_join(region_mapping, 'iso3c') %>% 
            group_by(.data$region, .data$iso3c, .data$year) %>% 
            summarise(population = mean(.data$population), .groups = 'drop'),
          
          'year'
        ) %>% 
        mutate(value = .data$value * .data$population) %>% 
        select('region', 'iso3c', 'year', 'variable', 'value')
    ) %>% 
      assert(not_na, everything())
  
  tmp <- full_join(
    # estimates after last historic year
    production_estimates %>% 
      filter(max(unique(steel_historic_prod$year)) <= .data$year,
               .data$variable %in% c('primary.production', 
                                     'secondary.production'),
             'Total' != .data$iso3c),
    
    # estimates up to last historic year
    steel_historic_prod %>% 
      rename(historic = .data$value) %>% 
      # Mt/year * 1e6 t/Mt = t/year
      mutate(historic = .data$historic * 1e6,
             scenario = production_estimates[[1,'scenario']]) %>% 
      # duplicate for all scenarios
      complete(
          nesting(!!!syms(c('region', 'iso3c', 'year', 'variable', 
                            'historic'))),
        scenario = unique(pull(production_estimates, 'scenario'))) %>% 
      complete(nesting(!!!syms(c('scenario', 'region', 'iso3c'))),
               year = unique(.data$year),
               variable = unique(.data$variable),
               fill = list(historic = 0)) %>% 
      assert(not_na, everything()),
    
    c('scenario', 'region', 'iso3c', 'year', 'variable')
  )
  
  tmp <- bind_rows(
    # scale country production to meet historic production in the last year
    # for which data is available
    tmp %>% 
      filter(.data$year >= max(steel_historic_prod$year)) %>% 
      group_by(.data$scenario, .data$region, .data$iso3c, .data$variable) %>% 
      filter(!is.na(first(.data$historic, order_by = .data$year))) %>% 
        mutate(value = .data$value 
                     / first(.data$value / .data$historic, 
                             order_by = .data$year)) %>% 
      select(-'historic') %>% 
      ungroup(),
    
    # countries w/o historic production fade production in over 20 years
    tmp %>% 
      filter(.data$year >= max(steel_historic_prod$year)) %>% 
        group_by(!!!syms(c('scenario', 'region', 'iso3c', 'variable'))) %>% 
        filter(any(
          is.na(first(.data$historic, order_by = .data$year)),
          0 == first(.data$historic, order_by = .data$year))) %>% 
      mutate(value = .data$value 
                   * pmin(1, (.data$year - first(.data$year)) / 20)) %>% 
      select(-'historic') %>% 
      ungroup(),
    
      # data up to last historic year
    tmp %>% 
      filter(.data$year < max(steel_historic_prod$year)) %>% 
      select(-'value', value = 'historic')
  ) %>% 
      semi_join(region_mapping, c('region', 'iso3c')) %>% 
      group_by(!!!syms(c('scenario', 'region', 'iso3c', 'year', 
                         'variable'))) %>% 
      summarise(value = sum(.data$value), .groups = 'drop') %>%
      assert(not_na, everything()) %>% 
    arrange(.data$scenario, .data$region, .data$iso3c, .data$variable, 
            .data$year)
    
    # make zero values explicit ----
    tmp <- tmp %>% 
      semi_join(region_mapping, c('region', 'iso3c')) %>% 
      complete(.data$scenario, .data$variable,
               nesting(!!sym('region'), !!sym('iso3c')),
               year   = unique(!!sym('year')), 
               fill = list(value = 0))
    
    ## construct output ----
    x <- tmp %>% 
      filter(min(steel_historic_prod$year) <= .data$year) %>% 
      semi_join(region_mapping, c('region', 'iso3c')) %>% 
      right_join(
        tribble(
          ~variable,                ~pf,
          'primary.production',     'ue_steel_primary',
          'secondary.production',   'ue_steel_secondary'),
        
        'variable'
      ) %>%
      assert(not_na, everything()) %>% 
      # t/year * 1e-6 Mt/t = Mt/year
      mutate(value = .data$value * 1e-6) %>% 
      select('scenario', 'iso3c', 'pf', 'year', 'value') %>% 
      as.magpie(spatial = 2, temporal = 4, data = 5)
  }
  
  # match exogenous estimates ----
  ## IEA ETP 2017 ----
  if ('IEA_ETP' == match.estimates) {
    ### regional IEA ETP growth rates ----
    ETP_growth_rates <- readSource('IEA_ETP', 'industry_subsectors', FALSE) %>% 
      `[`(,,'IEA_ETP_RTS.Production|Industry|Steel (Mt/yr)') %>% 
      as.data.frame() %>% 
      as_tibble() %>% 
      select(region = 'Region', year = 'Year', value = 'Value') %>% 
      character.data.frame() %>% 
      mutate(year = as.integer(.data$year)) %>% 
      interpolate_missing_periods_(
        periods = list(year = seq_range(range(.$year))), method = 'spline') %>% 
      group_by(.data$region) %>% 
      mutate(factor = case_when(
        max(steel_historic_prod$year) < .data$year ~ 
          .data$value / lag(.data$value,
                            default = first(.data$value, order_by = .data$year),
                            order_by = .data$year),
        max(steel_historic_prod$year) == .data$year ~ 1,
        max(steel_historic_prod$year) > .data$year ~ 
            lag(.data$value,
                   default = first(.data$value, order_by = .data$year),
              order_by = .data$year) 
          / .data$value)) %>% 
      ungroup() %>% 
      select(-'value')
    
    ### growth rates during ETP ----
    # steel_growth_rates_during_ETP: scenario, region, iso3c, year, variable, 
    #                                share, factor
    # variable: primary.production/secondary.production
    # share:    share of primary/secondary production in total production
    # factor:   growth rate of total steel production relative to previous
    #           period
    steel_growth_rates_during_ETP <- ETP_growth_rates %>% 
      filter(max(steel_historic_prod$year) <= .data$year) %>% 
      # expand OECD and Non-OECD regions to iso3c countries
      full_join(
        bind_rows(
          region_mapping %>% 
            filter(.data$iso3c %in% OECD_iso3c) %>% 
            mutate(region = 'OECD'),
          
          region_mapping %>% 
            filter(!.data$iso3c %in% OECD_iso3c) %>% 
            mutate(region = 'Non-OECD')
        ),
        
        'region'
      ) %>% 
      select(-'region') %>% 
      # expand across scenarios, add regions 
      # FIXME: treat scenarios differently by using inter-scenario
      # growth-rates from tmp
      full_join(
        tmp %>% 
          semi_join(region_mapping, c('region', 'iso3c')) %>% 
          distinct(!!!syms(c('scenario', 'region', 'iso3c'))),
        
        'iso3c'
      ) %>% 
      # expand across primary and secondary production
      full_join(
        tmp %>% 
          semi_join(region_mapping, c('region', 'iso3c')) %>% 
          filter(between(.data$year, 
                         max(steel_historic_prod$year), 
                         max(ETP_growth_rates$year))) %>% 
          group_by(!!!syms(c('scenario', 'region', 'iso3c', 'year'))) %>% 
          filter(0 != sum(.data$value)) %>% 
          mutate(share = .data$value / sum(.data$value)) %>% 
          select(-'value') %>% 
          ungroup() %>% 
          interpolate_missing_periods_(
            periods = list(year = unique(pmax(max(steel_historic_prod$year), 
                                              ETP_growth_rates$year))),
            value = 'share',
            expand.values = TRUE),
        
        c('scenario', 'region', 'iso3c', 'year')
      )
    
    ### growth rates after ETP ----
    # steel_growth_rates_after_ETP: scenario, region, iso3c, year, variable, 
    #                               share, factor
    # variable: primary.production/secondary.production
    # share:    share of primary/secondary production in total production
    # factor:   growth rate of total steel production relative to previous 
    #           period
    steel_growth_rates_after_ETP <- tmp %>% 
      filter(max(steel_historic_prod$year) <= .data$year) %>% 
      filter(max(ETP_growth_rates$year) < .data$year) %>% 
      group_by(!!!syms(c('scenario', 'region', 'iso3c', 'variable'))) %>% 
      mutate(factor = .data$value
             / lag(.data$value, 
                          default = first(.data$value, order_by = .data$year),
                   order_by = .data$year)) %>% 
      group_by(!!!syms(c('scenario', 'region', 'iso3c', 'year'))) %>% 
      mutate(share = .data$value / sum(.data$value)) %>% 
      ungroup() %>% 
      select(-'value')
    
    ### combine data ----
    IEA_ETP_matched <- bind_rows(
      # tmp data up to the last historic year
      tmp %>% 
        semi_join(region_mapping, c('region', 'iso3c')) %>% 
        filter(max(steel_historic_prod$year) > .data$year,
               'Total' != .data$iso3c),
      
      # tmp data after historic production, driven by growth rates
      tmp %>% 
        semi_join(region_mapping, c('region', 'iso3c')) %>% 
        filter(max(steel_historic_prod$year) == .data$year) %>% 
        group_by(!!!syms(c('scenario', 'region', 'iso3c'))) %>% 
        summarise(value = sum(.data$value), .groups = 'drop') %>% 
        full_join(
          bind_rows(
            steel_growth_rates_during_ETP, 
            steel_growth_rates_after_ETP),
          
          c('scenario', 'region', 'iso3c')
        ) %>% 
        group_by(!!!syms(c('scenario', 'region', 'iso3c', 'variable'))) %>% 
        #   primary/secondary production 
        # = 2014 total production
        # * primary/secondary production share
        # * cumulated ETP/tmp production growth factor
        mutate(value = .data$value * .data$share * cumprod(.data$factor)) %>% 
        ungroup() %>% 
        select('scenario', 'region', 'iso3c', 'year', 'variable', 'value')
    )
    
    ## construct output ----
    x <- IEA_ETP_matched %>% 
      filter(min(steel_historic_prod$year) <= .data$year) %>% 
      semi_join(region_mapping, c('region', 'iso3c')) %>% 
      right_join(
        tribble(
          ~variable,                ~pf,
          'primary.production',     'ue_steel_primary',
          'secondary.production',   'ue_steel_secondary'),
        
        'variable'
      ) %>%
      assert(not_na, everything()) %>% 
      # t/year * 1e-6 Mt/t = Mt/year
      mutate(value = .data$value * 1e-6) %>% 
      select('scenario', 'iso3c', 'pf', 'year', 'value') %>% 
      as.magpie(spatial = 2, temporal = 4, data = 5)
  } else {
    stop('Unknown setting \'', match.estimates, '\' for match.estimates')
  }
    
  # return statement ----
  return(list(x = x,
              weight = NULL,
              unit = 'Mt steel/year',
              description = 'primary and secondary steel production'))
}

#' @rdname EDGE-Industry
#' @export
calcIndustry_Value_Added <- function(match.steel.historic.values = TRUE,
                                     match.steel.estimates = NULL) {
  # load required data ----
  ## region mapping for aggregation ----
  region_mapping <- toolGetMapping(name = 'regionmapping_21_EU11.csv',
                                   type = 'regional') %>% 
    as_tibble() %>% 
    select(region = 'RegionCode', iso3c = 'CountryCode')
  
  ## UNIDO INSTATA2 data ----
  INDSTAT <- readSource('UNIDO', 'INDSTAT2', FALSE) %>% 
    madrat_mule()
  
  ### add iso3c codes and regions ----
  # FIXME We are substituting some historic country codes by 'default' codes of
  # current countries. Generally, they are situated in the same aggregation 
  # region, so this has no impact on the derivation of regional statistics.
  # This does not apply to former Yugoslavia however. Since the countries in 
  # question (currently Slovenia and Kroatia, others might join the EU at a 
  # later time and then require reclassification) are small compared to the 
  # respective regions (Europe and Rest-of-World), the impact should be limited.
  INDSTAT <- INDSTAT %>% 
    # fix some country codes
    filter(810 != .data$country) %>%   # SUN data synthetic anyhow
    mutate(
      country = ifelse(200 == .data$country, 203, .data$country),  # CSE for CSK
      country = ifelse(530 == .data$country, 531, .data$country),  # CUW for ANT
      country = ifelse(890 == .data$country, 688, .data$country),  # SRB for YUG
      country = ifelse(891 == .data$country, 688, .data$country)   # SRB for SCG
    ) %>% 
    # add iso3c country codes
    left_join(
      bind_rows(
        countrycode::codelist %>% 
          select('iso3c', 'un') %>% 
          filter(!is.na(.data$iso3c), !is.na(.data$un)),
        
        # country codes missing from package countrycode
        tribble(
          ~iso3c,   ~un,
          'TWN',    158,   # Taiwan
          'ETH',    230,   # Ethiopia and Eritrea
          'DEU',    278,   # East Germany
          'DEU',    280,   # West Germany
          'PAN',    590,   # Panama
          'SDN',    736    # Sudan
        )
      ),
      
      c('country' = 'un')
    ) %>% 
    assert(not_na, everything()) %>% 
    # add regions based on country codes
    left_join(
      bind_rows(
        region_mapping, 
        tibble(iso3c = 'SUN', region = 'SUN')
      ),
      'iso3c') %>% 
    assert(not_na, everything())
  
  ### censor unreasonable data ----
  INDSTAT_censor <- bind_rows(
    bind_rows(
      tibble(iso3c = 'IRQ', year = 1994:1998),
      tibble(iso3c = 'MDV', year = INDSTAT$year %>% unique()),
      tibble(iso3c = 'BIH', year = 1990:1991)
    ) %>% 
      mutate(censor = TRUE,
             reason = 'unreasonable'),
    
    bind_rows(
      tibble(iso3c = 'HKG', year = unique(INDSTAT$year)),
      tibble(iso3c = 'MAC', year = unique(INDSTAT$year)),
      tibble(iso3c = 'CHN', year = min(INDSTAT$year):1997)
    ) %>% 
      mutate(censor = TRUE,
             reason = 'not representative')
  )
  
  ## population data ----
  population <- calcOutput('Population', FiveYearSteps = FALSE,
                           aggregate = FALSE) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    select(scenario = .data$Data1, iso3c = .data$Region, year = .data$Year, 
           population = .data$Value) %>% 
    character.data.frame() %>% 
    mutate(scenario = sub('^pop_', '', .data$scenario),
           year = as.integer(.data$year),
           # million people * 1e6/million = people
           population = .data$population * 1e6)
  
  ## GDP data ----
  GDP <- calcOutput(type = 'GDPppp', FiveYearSteps = FALSE, 
                    aggregate = FALSE) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    select(scenario = .data$Data1, iso3c = .data$Region, year = .data$Year, 
           GDP = .data$Value) %>% 
    character.data.frame() %>% 
    mutate(scenario = sub('^gdp_', '', .data$scenario),
           year = as.integer(.data$year),
           # $m * 1e6 $/$m = $
           GDP = .data$GDP * 1e6)
  
  # calc manufacturing share in GDP ----
  manufacturing_share <- INDSTAT %>% 
    filter('D' == .data$isic,
           between(.data$utable, 17, 20)) %>% 
    group_by(!!!syms(c('iso3c', 'year'))) %>% 
    filter(max(.data$lastupdated) == .data$lastupdated) %>% 
    ungroup() %>% 
    select('region', 'iso3c', 'year', manufacturing = 'value') %>% 
    inner_join(
      GDP %>% 
        filter(max(INDSTAT$year) >= .data$year) %>% 
        group_by(.data$iso3c, .data$year) %>% 
        summarise(GDP = calc_mode(.data$GDP), .groups = 'drop'),
      
      c('iso3c', 'year')
    ) %>% 
    inner_join(
      population %>% 
        filter(max(INDSTAT$year) >= .data$year) %>% 
        group_by(.data$iso3c, .data$year) %>% 
        summarise(population = calc_mode(.data$population), .groups = 'drop'),
      
      c('iso3c', 'year')
    ) %>% 
    # mark unreasonable data for exclusion
    full_join(
      INDSTAT_censor %>% 
        select(-.data$reason), 
      
      c('iso3c', 'year')
    ) %>% 
    mutate(censor = ifelse(is.na(.data$censor), FALSE, TRUE))
  
  # regress per-capita industry value added ----
  regression_data <- manufacturing_share %>% 
    filter(!.data$censor) %>% 
    duplicate(region = 'World') %>% 
    pivot_longer(c('population', 'GDP', 'manufacturing')) %>% 
    group_by(.data$region, .data$year, .data$name) %>% 
    summarise(value = sum(.data$value), .groups = 'drop') %>% 
    pivot_wider() %>% 
    mutate(
      # mfg.share = .data$manufacturing / .data$GDP,  FIXME
      GDPpC     = .data$GDP / .data$population)
  
  regression_parameters <- tibble()
  for (r in sort(unique(regression_data$region))) {
    regression_parameters <- bind_rows(
      regression_parameters,
      
      nls(formula = manufacturing / population ~ a * exp(b / GDPpC),
          data = regression_data %>%
            filter(.data$region == r),
          start = list(a = 1000, b = -2000),
          trace = FALSE) %>% 
        tidy() %>%
        select('term', 'estimate') %>%
        pivot_wider(names_from = 'term', values_from = 'estimate') %>% 
        mutate(region = r)
    )
  }
  
  # FIXME add scenario differentiation of regression parameters ----
  
  # project total manufacturing share ----
  ## calculate GDPpC projection scenarios ----
  GDPpC <- full_join(population, GDP, c('scenario', 'iso3c', 'year')) %>% 
    mutate(GDPpC = .data$GDP / .data$population) %>% 
    full_join(region_mapping, 'iso3c')
  
  ## converge regional towards global limit ----
  parameter_a_world <- regression_parameters %>% 
    filter('World' == .data$region) %>% 
    getElement('a')
  
  regression_parameters_converging <- regression_parameters %>% 
    filter('World' != .data$region) %>% 
    mutate(year = min(regression_data$year)) %>% 
    complete(nesting(!!!syms(c('region', 'a', 'b'))), 
             year = min(regression_data$year):2100) %>% 
    mutate(a = .data$a + ( (parameter_a_world - .data$a) 
                         / (2200 - 2000) * (.data$year - 2000)
                         )
                       * (.data$year >= 2000))
  
  # calc projection ----
  projected_data <- inner_join(
    regression_parameters_converging, 
    GDPpC,
    c('region', 'year')
  ) %>% 
    mutate(manufacturing = .data$a * exp(.data$b / .data$GDPpC) 
                         * .data$population) %>% 
    select('scenario', 'region', 'iso3c', 'year', 'population', 'GDP', 
           'manufacturing')
  
  projected_data_regions <- projected_data %>%
    pivot_longer(c('GDP', 'population', 'manufacturing')) %>%
    group_by(!!!syms(c('scenario', 'region', 'year', 'name'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop') %>%
    sum_total_('region', name = 'World') %>%
    pivot_wider() %>%
    mutate(mfg.share = .data$manufacturing / .data$GDP,
           GDPpC     = .data$GDP / .data$population)
  
  # calc VA of steel production ----
  data_steel_production <- readSource('worldsteel', 'long', FALSE) %>% 
    madrat_mule()
  
  regression_data_steel <- inner_join(
    INDSTAT %>% 
      filter(20 == .data$ctable, 
             '27' == .data$isic, 
             between(.data$utable, 17, 20),
             0 < .data$value) %>% 
      group_by(.data$iso3c, .data$year) %>% 
      filter(max(.data$lastupdated) == .data$lastupdated) %>% 
      ungroup() %>% 
      select('region', 'iso3c', 'year', steel.VA = 'value'),
    
    data_steel_production %>%
      filter(0 != .data$value) %>% 
      rename(steel.production = 'value'),
    
    c('iso3c', 'year')
  )
  
  # Data with an obvious mismatch between steel production and steel value added
  # figures is excluded from the regression.
  # Data for Hong Kong (1973-1979) is excluded, since no data for China is 
  # available for this period and the data would bias the regression for the CHN
  # region.
  steel_censor <- list_to_data_frame(
    list(BGD = 2011,
         CHE = 1995:1996,
         CHL = 2008,
         HKG = 1973:1979,
         HRV = 2012,
         IRL = 1980,
         LKA = 2006, 
         MAR = 1989:2004,
         MKD = 1996,
         PAK = 1981:1982,
         TUN = 2003:2006),
    'iso3c', 'year') %>% 
    mutate(censored = TRUE)
  
  regression_data_steel <- regression_data_steel %>% 
    left_join(steel_censor, c('iso3c', 'year')) %>% 
    mutate(censored = ifelse(is.na(.data$censored), FALSE, TRUE))
  
  ## compute regional and World aggregates ----
  regression_data_steel <- regression_data_steel %>% 
    inner_join(
      population %>% 
        group_by(.data$iso3c, .data$year) %>% 
        summarise(population = calc_mode(.data$population), .groups = 'drop'), 
      
      c('iso3c', 'year')
    ) %>% 
    inner_join(
      GDP %>% 
        group_by(.data$iso3c, .data$year) %>% 
        summarise(GDP = calc_mode(.data$GDP), .groups = 'drop'), 
      
      c('iso3c', 'year')
    ) %>% 
    pivot_longer(c('population', 'steel.production', 'GDP', 'steel.VA'),
                 names_to = 'variable', 
                 names_transform = list(variable = factor)) %>% 
    duplicate(region = 'World')
  
  regression_data_steel <- bind_rows(
    regression_data_steel,
    
    regression_data_steel %>% 
      filter(!.data$censored) %>% 
      group_by(.data$region, .data$year, .data$censored, .data$variable) %>% 
      summarise(value = sum(.data$value, na.rm = TRUE),
                iso3c = 'Total',
                .groups = 'drop')
  ) %>% 
    group_by(!!!syms(c('region', 'iso3c', 'year', 'censored', 'variable'))) %>% 
    pivot_wider(names_from = 'variable')
  
  ## compute regression parameters ----
  regression_parameters_steel <- tibble()
  for (r in sort(unique(regression_data_steel$region))) {
    regression_parameters_steel <- bind_rows(
      regression_parameters_steel,
      
      nls(formula = steel.VApt ~ a * exp(b / GDPpC),
          data = regression_data_steel %>%
            filter(r == .data$region,
                   'Total' == .data$iso3c,
                   !.data$censored) %>% 
            mutate(steel.VApt = .data$steel.VA / .data$steel.production,
                   GDPpC      = .data$GDP / .data$population),
          start = list(a = 500, b = 500),
          trace = FALSE) %>% 
        tidy() %>%
        select('term', 'estimate') %>%
        pivot_wider(names_from = 'term', values_from = 'estimate') %>% 
        mutate(region = r)
    )
  }
  
  ### substitute World for AFR parameters ----
  if ('AFR' %in% region_mapping$region) {
    replacement_region <- 'AFR'
  } else if ('SSA' %in% region_mapping$region) {
    replacement_region <- 'SSA'
  } else {
    replacement_region <- NA
  }
  
  if (!is.na(replacement_region)) {
    regression_parameters_steel <- bind_rows(
      regression_parameters_steel %>% 
        filter(replacement_region != .data$region),
      
      regression_parameters_steel %>% 
        filter('World' == .data$region) %>% 
        mutate(region = replacement_region)
    )
    rm(replacement_region)
  }
  
  ## project steel VA per tonne of steel ----
  parameter_a_world_steel <- regression_parameters_steel %>% 
    filter('World' == .data$region) %>% 
    pull('a')
  
  regression_parameters_steel_converging <- regression_parameters_steel %>% 
    filter('World' != .data$region) %>% 
    mutate(year = as.integer(2000)) %>% 
    complete(nesting(!!!syms(c('region', 'a', 'b'))), year = 2000:2100) %>% 
    mutate(a = .data$a 
             + ( (parameter_a_world_steel - .data$a) 
               / (2200 - 2000) 
               * (.data$year - 2000)
               ))
  
  projected_steel_data <- inner_join(
    inner_join(
      regression_parameters_steel_converging,
      
      GDPpC,
      
      c('region', 'year')
    ) %>% 
      mutate(steel.VApt = .data$a * exp(.data$b / .data$GDPpC)) %>% 
      select('scenario', 'region', 'iso3c', 'year', 'population', 'GDP', 
             'GDPpC', 'steel.VApt'),
    
    calcOutput(type = 'Steel_Projections', 
               match.historic.values = match.steel.historic.values, 
               match.estimates = match.steel.estimates, 
               aggregate = FALSE, supplementary = FALSE) %>% 
      as.data.frame() %>% 
      as_tibble() %>% 
      select(scenario = 'Data1', iso3c = 'Region', variable = 'Data2', 
             year = 'Year', value = 'Value') %>% 
      character.data.frame() %>% 
      mutate(year = as.integer(.data$year),
             # Mt/year * 1e6 t/Mt = t/year
             value = .data$value * 1e6,
             variable = sub('^ue_steel_(primary|secondary)$',
                            '\\1.production', .data$variable)) %>% 
      filter(between(.data$year, 2000, 2100)) %>% 
      full_join(region_mapping, 'iso3c') %>% 
      assert(not_na, everything()) %>% 
      group_by(!!!syms(c('scenario', 'region', 'iso3c', 'year'))) %>% 
      summarise(steel.production = sum(.data$value), .groups = 'drop'),
    
    c('scenario', 'region', 'iso3c', 'year')
  ) %>% 
    mutate(steel.VA = .data$steel.VApt * .data$steel.production) %>% 
    select(-'steel.VApt', -'GDPpC') %>% 
    pivot_longer(c('population', 'GDP', 'steel.production', 'steel.VA')) %>% 
    duplicate_(c(region = 'World')) %>% 
    sum_total_('iso3c') %>% 
    pivot_wider() %>% 
    mutate(GDPpC      = .data$GDP / .data$population, 
           steel.VApt = .data$steel.VA / .data$steel.production) %>% 
    select('scenario', 'region', 'iso3c', 'year', 'population', 'GDP', 
           'steel.production', 'steel.VA', 'GDPpC', 'steel.VApt')
  
  # project cement production ----
  
}


