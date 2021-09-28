
#' Title
#'
#' @return
#'
#' @importFrom assertr assert
#' @importFrom broom tidy
#' @importFrom car logit
#' @importFrom quitte madrat_mule sum_total_
#' @importFrom stats SSlogis nls

#' @export
calcSteel_Projections <- function()
{
  # table of units ----
  # GDPpC = $/year
  # GDPpC_history = $/year
  # population_history = people

  # get EDGE-Industry switches ----
  # `EDGE-Industry_scenario_switches` <- EDGE_scenario_switches %>% 
  #   select(
  #     'scenario', 
  #     `steel.stock.estimate` = 'EDGE-Industry_steel.stock.estimate',
  #     `scenario.mask.OECD` = 
  #       'EDGE-Industry_scenario.mask.OECD',
  #     `scenario.mask.non-OECD` = 
  #       'EDGE-Industry_scenario.mask.non-OECD')
  
  # FIXME: remove before deploying
  load('./R/sysdata.rda')
  `EDGE-Industry_scenario_switches` <- EDGE_scenario_switches %>% 
    select(
      'scenario', 
      `steel.stock.estimate` = 'EDGE-Industry_steel.stock.estimate',
      `scenario.mask.OECD` = 
        'EDGE-Industry_scenario.mask.OECD',
      `scenario.mask.non-OECD` = 
        'EDGE-Industry_scenario.mask.non-OECD')
  
  
      
  # load required data ----
  ## region mapping for aggregation ----
  region_mapping <- toolGetMapping(name = 'regionmapping_21_EU11.csv',
                                   type = 'regional') %>% 
    as_tibble() %>% 
    select(region = 'RegionCode', iso3c = 'CountryCode')
  
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
  for (.estimate in unique(regression_data$estimate))
  {
    
    Asym <- (regression_data %>% 
      filter(.estimate == .data$estimate) %>% 
      pull('steel.stock.per.capita') %>% 
      max()) * 1.1
    
    coefficients <- lm(
      formula = logit(x, adjust = 0.025) ~ y,
      data = regression_data %>%
        filter(.estimate == .data$estimate,
               .data$steel.stock.per.capita > 0) %>%
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
      
      EDGE_scenario_switches %>%
        select('scenario', estimate = 'steel.stock.estimate'),

      'estimate'
    ),
    
    'scenario'
  ) %>% 
    # make sure all scenarios have associated regression parameters
    assert(
      not_na, .data$Asym, .data$scal, .data$xmid, 
      error_fun = function(errors, data)
      { 
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
    select(.data$scenario, .data$iso3c, .data$year, .data$value, .data$source)
  
  steel_stock_estimates <- bind_rows(
    steel_stock_estimates,
    
    steel_stock_per_capita %>%
      filter(.data$year >= min(steel_stock_estimates$year)) %>%
      full_join(
        EDGE_scenario_switches %>%
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
           error_fun = function(errors, data)
           { 
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
    select(.data$scenario, .data$iso3c, .data$region, .data$year, 
           .data$steel.stock.per.capita)
  
  rm(list = c('fade_start', 'fade_end'))
  
  ## update SSP4 ----
  # SSP4 uses SSP2 estimates for OECD countries and SSP1 estimates for non-OECD
  # countries
  OECD_iso3c <- toolGetMapping(name = 'regionmappingOECD.csv',
                               type = 'regional') %>%
    as_tibble() %>% 
    select(iso3c = 'CountryCode', region = 'RegionCode') %>% 
    filter('OECD' == .data$region) %>% 
    pull('iso3c')
  
  steel_stock_estimates <- bind_rows(
    # non-masked scenarios
    steel_stock_estimates %>% 
      filter('SSP4' != .data$scenario),
    
    # masked scenarios, OECD countries
    EDGE_scenario_switches %>% 
      select('scenario', 'scenario.mask.OECD') %>% 
      filter(!is.na(.data$scenario.mask.OECD)) %>% 
      rename(scenario.mask = 'scenario',
             scenario = 'scenario.mask.OECD') %>% 
      full_join(
        steel_stock_estimates %>% 
          filter(.data$iso3c %in% OECD_iso3c), 
        
        'scenario'
      ) %>% 
      select(-'scenario', 'scenario' = 'scenario.mask'),
    
    # masked scenarios, non-OECD countries
    EDGE_scenario_switches %>% 
      select('scenario', 'scenario.mask.non-OECD') %>% 
      filter(!is.na(.data$`scenario.mask.non-OECD`)) %>% 
      rename(scenario.mask = 'scenario',
             scenario = 'scenario.mask.non-OECD') %>% 
      full_join(
        steel_stock_estimates %>% 
          filter(!.data$iso3c %in% OECD_iso3c), 
        
        'scenario'
      ) %>% 
      select(-'scenario', 'scenario' = 'scenario.mask')
  )
  
  rm(list = 'OECD_iso3c')

  # workbench ----
  steel_stock_estimates %>% 
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
    mutate(steel.stock = .data$steel.stock.per.capita * .data$population)  
    
}
