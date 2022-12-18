#' EDGE-Industry
#'
#' Functions for calculating industry activity trajectories.
#'
#' @md
#' @param subtype One of
#'   - `production` Returns trajectories of primary and secondary steel
#'     production (`calcSteel_Projections()`).
#'   - `secondary.steel.max.share` Returns the maximum share of secondary steel
#'     in total steel production (`calcSteel_Projections()`).
#'   - `physical` Returns physical production trajectories for cement
#'     (`calcIndustry_Value_Added()`).
#'   - `economic` Returns value added trajectories for all subsectors
#'     (`calcIndustry_Value_Added()`).
#' @param match.steel.historic.values Should steel production trajectories match
#'   historic values?
#' @param match.steel.estimates Should steel production trajectories match
#'   exogenous estimates?  `NULL` or one of
#'   - `IEA_ETP` IEA 2017 Energy Transition Pathways steel production totals for
#'     OECD and Non-OECD countries from the _Reference Technologies Scenario_
#'     until 2060, and original growth rates after that.
#' @param save.plots `NULL` (default) if no plots are saved, or the path to save
#'     directories to.
#' @param China_Production A data frame with columns `period` and
#'     `total.production` prescribing total production for China to have,
#'     disregarding results from the stock saturation model.
#'
#' @return A list with a [`magpie`][magclass::magclass] object `x`, `weight`,
#'   `unit`, `description`, `min`, and `max`.
#'
#' @author Michaja Pehl
#'
#' @seealso [`calcOutput()`]
#'
#' @importFrom assertr assert verify within_bounds
#' @importFrom broom tidy
#' @importFrom car logit
#' @importFrom dplyr %>% case_when n right_join semi_join distinct vars
#' @importFrom Hmisc wtd.quantile
#' @importFrom ggplot2 aes expand_limits facet_wrap geom_area geom_line
#'   geom_path geom_point ggplot ggsave guide_legend labs scale_colour_manual
#'   scale_fill_discrete scale_linetype_manual scale_shape_manual theme
#'   theme_minimal
#' @importFrom madrat calcOutput readSource toolGetMapping
#' @importFrom quitte calc_mode df_populate_range duplicate duplicate_
#'   list_to_data_frame madrat_mule order.levels sum_total_
#' @importFrom readr write_rds
#' @importFrom stats nls SSlogis sd
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom zoo na.approx rollmean

#' @rdname EDGE-Industry
#' @export
calcSteel_Projections <- function(subtype = 'production',
                                  match.steel.historic.values = TRUE,
                                  match.steel.estimates = 'none',
                                  save.plots = NULL,
                                  China_Production = NULL) {

  if (!is.null(save.plots)) {
    if (!all(isTRUE(file.info(save.plots)$isdir),
             448L == bitwAnd(file.info(save.plots)$mode, 448L))) {
      stop('No writable directory `save.plots`: ', save.plots)
    }
  }

  produce_plots_and_tables <- TRUE

  . <- NULL

  # get EDGE-Industry switches ----
  # FIXME: remove before deploying
  EDGE_scenario_switches <- bind_rows(
    tribble(
      ~scenario,   ~`EDGE-Industry_steel.stock.estimate`,
      'SDP',       'low',
      'SDP_EI',    'low',
      'SDP_MC',    'low',
      'SDP_RC',    'low',
      'SSP1',      'low',
      'SSP2',      'med',
      'SSP2EU',    'med',
      'SSP3',      'med',
      'SSP4',      'med',
      'SSP5',      'high') %>%
      pivot_longer(-'scenario', names_to = 'switch'),

    tribble(
      ~scenario,   ~`EDGE-Industry_scenario.mask.OECD`,
      'SSP4',      'SSP2') %>%
      pivot_longer(-'scenario', names_to = 'switch'),

    tribble(
      ~scenario,   ~`EDGE-Industry_scenario.mask.non-OECD`,
      'SSP4',      'SSP1') %>%
      pivot_longer(-'scenario', names_to = 'switch'),

    # steel stock lifetime convergence ----
    tribble(
      ~scenario,   ~`EDGE-Industry_steel.stock.lifetime.base.scenario`,
      'SDP',       'SSP2',
      'SDP_EI',    'SSP2',
      'SDP_MC',    'SSP2',
      'SDP_RC',    'SSP2',
      'SSP1',      'SSP2',
      'SSP2',      'SSP2',
      'SSP2EU',    'SSP2',
      'SSP3',      'SSP2',
      'SSP4',      'SSP4',
      'SSP5',      'SSP2') %>%
      pivot_longer(-'scenario', names_to = 'switch'),

    tribble(
      ~scenario,   ~`EDGE-Industry_steel.stock.lifetime.convergence.year`,
      'SDP',       '2100',
      'SDP_EI',    '2100',
      'SDP_MC',    '2100',
      'SDP_RC',    '2100',
      'SSP1',      '2100',
      'SSP2',      '2100',
      'SSP2EU',    '2100',
      'SSP3',      '2100',
      'SSP4',      '2010',
      'SSP5',      '2100') %>%
      pivot_longer(-'scenario', names_to = 'switch'),

    tribble(
      ~scenario,   ~`EDGE-Industry_steel.stock.lifetime.convergence.factor`,
      'SDP',       '1.25',
      'SDP_EI',    '1.25',
      'SDP_MC',    '1.25',
      'SDP_RC',    '1.25',
      'SSP1',      '1.25',
      'SSP2',      '1',
      'SSP2EU',    '1',
      'SSP3',      '1',
      'SSP4',      '1',
      'SSP5',      '0.75') %>%
      pivot_longer(-'scenario', names_to = 'switch'),

    NULL) %>%
    pivot_wider(names_from = 'switch')

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
  GDP <- calcOutput(type = 'GDP', FiveYearSteps = FALSE, aggregate = FALSE) %>%
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
      mix = pmax(0,  .data$l       * .data$computation
                   + (1 - .data$l) * .data$Pauliuk),
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

  if ('steel_stock_estimates' == subtype) {
    return(list(
      x = steel_stock_estimates %>%
        madrat_mule(),
      weight = NULL))
  }

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

  ### split Belgium and Luxembourg by population ----
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

  ## calculate secondary steel max share ----
  secondary.steel.max.switches <- calcOutput(
    type = 'industry_max_secondary_steel_share',
    scenarios = unique(population$scenario),
    regions = unique(region_mapping$region),
    aggregate = FALSE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(scenario = 'Data1', region = 'Data2', name = 'Data3',
           value = 'Value') %>%
    mutate(name = paste0('secondary.steel.max.share.', .data$name)) %>%
    pivot_wider() %>%
    character.data.frame()

  tmp <- full_join(
    steel_historic_prod %>%
      filter('Total' != .data$iso3c) %>%
      mutate(match = TRUE),

    secondary.steel.max.switches %>%
      select('scenario', 'secondary.steel.max.share.from') %>%
      mutate(match = TRUE,
             secondary.steel.max.share.from =
               as.integer(.data$secondary.steel.max.share.from)),

    'match'
  ) %>%
    select(-'match') %>%
    group_by(!!!syms(c('scenario', 'region', 'iso3c', 'variable'))) %>%
    filter(.data$year <= .data$secondary.steel.max.share.from) %>%
    group_by(!!!syms(c('scenario', 'region', 'iso3c', 'year', 'variable'))) %>%
    summarise(value = mean(.data$value), .groups = 'drop') %>%
    sum_total_('iso3c') %>%
    pivot_wider(names_from = 'variable', values_fill = list(value = 0)) %>%
    mutate(share = .data$secondary.production
                 / (.data$primary.production + .data$secondary.production)) %>%
    select('scenario', 'region', 'iso3c', 'year', 'share')

  secondary.steel.max.share <- bind_rows(
      tmp,

      tmp %>%
        distinct(.data$scenario, .data$region, .data$iso3c) %>%
        full_join(
          secondary.steel.max.switches %>%
            select('scenario', 'region', year = 'secondary.steel.max.share.by',
                   share = 'secondary.steel.max.share.target') %>%
            mutate(year = as.integer(.data$year),
                   share = as.numeric(.data$share)),

          c('scenario', 'region')
        )
    ) %>%
    interpolate_missing_periods_(
      periods = list('year' = seq_range(range(steel_stock_estimates$year))),
      value = 'share', expand.values = TRUE)

  # expand regional values to missing countries
  secondary.steel.max.share <- bind_rows(
    secondary.steel.max.share %>%
      filter('Total' != .data$iso3c),

    secondary.steel.max.share %>%
      filter('Total' == .data$iso3c) %>%
      select(-'iso3c') %>%
      right_join(
        region_mapping %>%
          anti_join(secondary.steel.max.share, c('region', 'iso3c')),

        'region'
      ) %>%
      assert(not_na, everything())
  )

  ## calculate primary and secondary production ----

  # Imports (> 0 and exports (< 0) are scaled by factors m such that they
  # balance globally.  If imports are twice as large as exports, the imbalance
  # is solved by scaling imports down by a factor twice as large as the factor
  # with which exports are scaled up.  E.g.:
  # trade <- c(1, 2, -7)
  # m <- (1 + sum(trade) / sum(abs(trade)) * -sign(trade))
  # adjusted.trade <- trade * m
  # sum(adjusted.trade) == 0

  production_estimates <- steel_stock_estimates %>%
    filter('Total' != .data$iso3c) %>%
    inner_join(steel_trade_share_2015 %>% select(-'region'), 'iso3c') %>%
    left_join(lifetime_projections, c('scenario', 'region', 'year')) %>%
    select(-'steel.stock.per.capita', -'population') %>%
    assert(not_na, everything()) %>%
    pivot_longer(c(-'scenario', -'iso3c', -'region', -'year')) %>%
    interpolate_missing_periods_(
      periods = list('year' = seq_range(range(.$year)))) %>%
    pivot_wider() %>%
    full_join(
      secondary.steel.max.share %>%
        rename(secondary.steel.max.share = 'share'),

      c('scenario', 'region', 'iso3c', 'year')
    ) %>%
    group_by(!!!syms(c('scenario', 'region', 'iso3c'))) %>%
    mutate(
      # stock additions: rolling average of stock changes (stocks might decrease
      # with decreasing population, but still become obsolete and need
      # replacement) over five years
      stock.additions = rollmean(
        pmax(0,
             .data$steel.stock - lag(.data$steel.stock, order_by = .data$year,
                                     default = first(.data$steel.stock))),
        k = 5, fill = 'extend', na.rm = TRUE),
      # depreciation: last years steel stock deprecated by 1/lifetime
      depreciation    = lag(x = .data$steel.stock, order_by = .data$year,
                            default = first(.data$steel.stock))
                      / lag(.data$lifetime, order_by = .data$year,
                            default = first(.data$lifetime)),
      # new stock: stock increases and replacements for deprecated old stock
      new.stock       = .data$stock.additions + .data$depreciation,
      # recycable: 90 % of deprecated steel stock are assumed to be recycled
      recyclable      = 0.9 * .data$depreciation, # FIXME: pull parameter out
      # trade: share of new stock serviced by trade
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
      production           = .data$new.stock - .data$adj.trade)

  if (is.data.frame(China_Production)) {
    China_Production <- China_Production %>%
      interpolate_missing_periods(period = seq_range(range(.$period)),
                                  value = 'total.production',
                                  method = 'spline') %>%
      mutate(total.production = .data$total.production * 1e6)

    production_estimates <- production_estimates %>%
      ungroup() %>%
      select('scenario', 'iso3c', 'year', 'production') %>%
      filter('SSP2EU' == .data$scenario,
             'CHN' == .data$iso3c,
             max(steel_historic_prod$year) < .data$year) %>%
      left_join(China_Production, c('year' = 'period')) %>%
      mutate(value = .data$total.production / .data$production) %>%
      select('iso3c', 'year', 'value') %>%
      expand_grid(scenario = unique(production_estimates$scenario)) %>%
      complete(nesting(!!sym('scenario')),
               iso3c = unique(production_estimates$iso3c),
               year = unique(production_estimates$year)) %>%
      mutate(value = case_when(
        max(steel_historic_prod$year) >= .data$year ~ 1,
        is.na(.data$value) ~ last(na.omit(.data$value)),
        TRUE ~ .data$value)) %>%
      left_join(production_estimates, c('scenario', 'iso3c', 'year')) %>%
      mutate(production = .data$production * .data$value) %>%
      select(-'value')
  }

  production_estimates <- production_estimates %>%
    mutate(
      secondary.production = pmin(
        .data$secondary.steel.max.share * .data$production,
        .data$recyclable),
      primary.production   = .data$production - .data$secondary.production) %>%
    select('scenario', 'region', 'iso3c', 'year', 'steel.stock', 'depreciation',
           'primary.production', 'secondary.production',
           trade = 'adj.trade') %>%
    filter(min(.data$year) < .data$year) %>%
    pivot_longer(c('steel.stock', 'depreciation', 'primary.production',
                   'secondary.production', 'trade'),
                 names_to = 'variable') %>%
    group_by(.data$scenario, .data$region, .data$year, .data$variable) %>%
    sum_total_('iso3c') %>%
    ungroup()

  ## calculate production limits of secondary steel----
  # FIXME: move to separate function
  production_limits <- production_estimates %>%
    filter('depreciation' == .data$variable) %>%
    select(-'variable')

  ## construct output ----
  x <- production_estimates %>%
    semi_join(region_mapping, c('region', 'iso3c')) %>%
    filter(min(steel_historic$year) <= .data$year) %>%
    right_join(
      tribble(
        ~variable,                ~pf,
        'primary.production',     'ue_steel_primary',
        'secondary.production',   'ue_steel_secondary'),

      'variable'
    ) %>%
    assert(not_na, everything()) %>%
    # t/year * 1e-6 Gt/t = Gt/year
    mutate(value = .data$value * 1e-9,
           scenario = paste0('gdp_', .data$scenario)) %>%
    select('scenario', 'iso3c', 'pf', 'year', 'value') %>%
    as.magpie(spatial = 2, temporal = 4, data = 5)

  # match historic values ----
  if (match.steel.historic.values) {
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
      # shift country production to meet historic production in the last year
      # for which data is available
      tmp %>%
        filter(.data$year >= max(steel_historic_prod$year)) %>%
        group_by(!!!syms(c('scenario', 'region', 'iso3c', 'variable'))) %>%
        filter(!is.na(first(.data$historic, order_by = .data$year))) %>%
        mutate(
          value = .data$value
                / first(.data$value, order_by = .data$year)
                * first(.data$historic, order_by = .data$year)) %>%
        ungroup() %>%
        select(-'historic') %>%
        interpolate_missing_periods_(periods = list('year' = unique(.$year)),
                                     expand.values = TRUE),

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

    ## update max secondary steel shares ----
    update.secondary.steel.max.share <- function(production,
                                                 secondary.steel.max.share) {
      full_join(
        secondary.steel.max.share %>%
          rename(max.share = 'share'),

        production %>%
          pivot_wider(names_from = 'variable') %>%
          mutate(share = .data$secondary.production
                 / (.data$primary.production + .data$secondary.production)),

        c('scenario', 'region', 'iso3c', 'year')
      ) %>%
        mutate(share = pmax(.data$share, .data$max.share, na.rm = TRUE)) %>%
        select(all_of(colnames(secondary.steel.max.share))) %>%
        assert(not_na, everything())
    }

    secondary.steel.max.share <- update.secondary.steel.max.share(
      tmp, secondary.steel.max.share)

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
      # t/year * 1e-9 Gt/t = Gt/year
      mutate(value = .data$value * 1e-9,
             scenario = paste0('gdp_', .data$scenario)) %>%
      select('scenario', 'iso3c', 'pf', 'year', 'value') %>%
      as.magpie(spatial = 2, temporal = 4, data = 5)
  }

  # match exogenous estimates ----
  ## IEA ETP 2017 ----
  if ('IEA_ETP' == match.steel.estimates) {
    # projected SSP2 production aggregated into OECD/Non-OECD regions
    # w/o Chinese production if that is exogenously prescribed
    if (!is.data.frame(China_Production)) {
      projected_production <- tmp %>%
        filter('SSP2' == .data$scenario) %>%
        mutate(
          region = ifelse(.data$iso3c %in% OECD_iso3c, 'OECD', 'Non-OECD')) %>%
        group_by(.data$region, .data$year) %>%
        summarise(value = sum(.data$value) * 1e-6, .groups = 'drop')
    } else  {
      projected_production <- tmp %>%
        filter('SSP2' == .data$scenario,
               'CHN' != .data$iso3c) %>%
        mutate(
          region = ifelse(.data$iso3c %in% OECD_iso3c, 'OECD', 'Non-OECD')) %>%
        group_by(.data$region, .data$year) %>%
        summarise(value = sum(.data$value) * 1e-6, .groups = 'drop')
    }

    # IEA ETP RTS production, minus Chinese production if exogenously prescribed
    ETP_production <- readSource('IEA_ETP', 'industry', convert = FALSE) %>%
      `[`(,,'RTS.Industry|Materials production|Crude steel.Mt') %>%
      as.data.frame() %>%
      as_tibble() %>%
      select(region = 'Region', year = 'Year', ETP = 'Value') %>%
      filter(.data$region %in% c('OECD', 'Non-OECD')) %>%
      character.data.frame() %>%
      mutate(year = as.integer(.data$year))

    if (is.data.frame(China_Production)) {
      ETP_production <- bind_rows(
        ETP_production %>%
          filter('OECD' == .data$region),

        ETP_production %>%
          filter('Non-OECD' == .data$region) %>%
          left_join(
            China_Production %>%
              mutate(total.production = .data$total.production * 1e-6),

            c('year' = 'period')
          ) %>%
          mutate(total.production = ifelse(!is.na(.data$total.production),
                                           .data$total.production,
                                           last(na.omit(.data$total.production))),
                 ETP = .data$ETP - .data$total.production) %>%
          select(-'total.production')
      ) %>%
        verify(expr = .data$ETP > 0,
               description = paste('exogenous Chinese production does not exceed',
                                   'IEA ETP Non-OECD production'))
    }

    scaling_factor <- inner_join(
      projected_production,
      ETP_production,

      c('region', 'year')
    ) %>%
      group_by(.data$region) %>%
      mutate(
        factor = .data$ETP / .data$value,
        factor = .data$factor / first(.data$factor, order_by = .data$year))

    # If exogenous Chinese production trajectories gobble up all Non-OECD
    # production, temper the scaling factor to only meet 2060 production exactly
    if (is.data.frame(China_Production)) {
      scaling_factor <- scaling_factor %>%
      mutate(factor = ( .data$factor
                      + ( first(.data$factor, order_by = .data$year)
                        + ( ( last(.data$factor, order_by = .data$year)
                            - first(.data$factor, order_by = .data$year)
                            )
                          / (max(.data$year) - min(.data$year))
                          * (.data$year - min(.data$year))
                          )
                        )
                      )
                    / 2)
      }

      scaling_factor <- scaling_factor %>%
      select(-'value', -'ETP') %>%
      bind_rows(
        tibble(
          year = c(max(steel_historic_prod$year), 2100), factor = 1)) %>%
      complete(year = unique(tmp$year)) %>%
      filter(!is.na(.data$region)) %>%
      mutate(factor = na.approx(object = .data$factor,
                                x = .data$year,
                                yleft = first(na.omit(.data$factor)),
                                yright = last(na.omit(.data$factor)))) %>%
      ungroup() %>%
      assert(not_na, everything())

    if (!is.data.frame(China_Production)) {
      IEA_ETP_matched <- tmp %>%
        mutate(OECD.region = ifelse(.data$iso3c %in% OECD_iso3c,
                                    'OECD', 'Non-OECD')) %>%
        full_join(scaling_factor, c('year', 'OECD.region' = 'region')) %>%
        mutate(value = .data$value * .data$factor) %>%
        select(-'factor', -'OECD.region') %>%
        complete(nesting(!!!syms(c('scenario', 'region', 'iso3c', 'variable'))),
                 year = unique(.$year),
                 fill = list(value = 0)) %>%
        arrange('scenario', 'region', 'iso3c', 'year', 'variable')
    } else {
      IEA_ETP_matched <- tmp %>%
        filter('CHN' != .data$iso3c) %>%
        mutate(OECD.region = ifelse(.data$iso3c %in% OECD_iso3c,
                                    'OECD', 'Non-OECD')) %>%
        full_join(scaling_factor, c('year', 'OECD.region' = 'region')) %>%
        mutate(value = .data$value * .data$factor) %>%
        select(-'factor', -'OECD.region') %>%
        bind_rows(
          tmp %>%
            filter('CHN' == .data$iso3c)
        ) %>%
        complete(nesting(!!!syms(c('scenario', 'region', 'iso3c', 'variable'))),
                 year = unique(.$year),
                 fill = list(value = 0)) %>%
        arrange('scenario', 'region', 'iso3c', 'year', 'variable')
    }

    ## update max secondary steel share ----
    secondary.steel.max.share <- update.secondary.steel.max.share(
      IEA_ETP_matched, secondary.steel.max.share)

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
      # t/year * 1e-9 Gt/t = Gt/year
      mutate(value = .data$value * 1e-9,
             scenario = paste0('gdp_', .data$scenario)) %>%
      select('scenario', 'iso3c', 'pf', 'year', 'value') %>%
      as.magpie(spatial = 2, temporal = 4, data = 5)
  } else if ('none' != match.steel.estimates) {
    stop('Unknown setting \'', match.steel.estimates,
         '\' for match.steel.estimates')
  }

  ### return secondary steel max share ----
  if ('secondary.steel.max.share' == subtype) {
    return(
      list(x = secondary.steel.max.share %>%
             filter(.data$year %in% unique(quitte::remind_timesteps$period)) %>%
             mutate(scenario = paste0('gdp_', .data$scenario)) %>%
             select('scenario', 'iso3c', 'year', 'share') %>%
             as.magpie(spatial = 2, temporal = 3, data = 4),
           weight = calcOutput(
             type = 'Steel_Projections',
             match.steel.historic.values = match.steel.historic.values,
             match.steel.estimates = match.steel.estimates,
             aggregate = FALSE, years = unique(quitte::remind_timesteps$period),
             supplementary = FALSE) %>%
             dimSums(dim = 3.2),
           unit = 'fraction',
           description = 'maximum secondary steel production share'
      )
    )
  }

  # return statement ----
  return(list(x = x,
              weight = NULL,
              unit = 'Gt steel/year',
              description = 'primary and secondary steel production'))
}

#' @rdname EDGE-Industry
#' @export
calcIndustry_Value_Added <- function(subtype = 'physical',
                                     match.steel.historic.values = TRUE,
                                     match.steel.estimates = 'none',
                                     save.plots = NULL,
                                     China_Production = NULL) {
  if (!is.null(save.plots)) {
    if (!all(isTRUE(file.info(save.plots)$isdir),
             448L == bitwAnd(file.info(save.plots)$mode, 448L))) {
      stop('No writable directory `save.plots`: ', save.plots)
    }
  }

  linetype_scenarios <- c(regression = 'dashed',
                          # SSP1       = 'dotted',
                          SSP2       = 'solid',
                          # SSP5       = 'dashed'
                          NULL)

  # load required data ----
  ## region mapping for aggregation ----
  region_mapping <- toolGetMapping(name = 'regionmapping_21_EU11.csv',
                                   type = 'regional') %>%
    as_tibble() %>%
    select(region = 'RegionCode', iso3c = 'CountryCode')

  ## UNIDO INSTATA2 data ----
  INDSTAT <- readSource('UNIDO', 'INDSTAT2', convert = FALSE) %>%
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
  GDP <- calcOutput(type = 'GDP', FiveYearSteps = FALSE, aggregate = FALSE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(scenario = .data$Data1, iso3c = .data$Region, year = .data$Year,
           GDP = .data$Value) %>%
    character.data.frame() %>%
    mutate(scenario = sub('^gdp_', '', .data$scenario),
           year = as.integer(.data$year),
           # $m * 1e6 $/$m = $
           GDP = .data$GDP * 1e6)

  ## ---- load cement production data ----
  data_cement_production <- calcOutput('Cement', aggregate = FALSE) %>%
    magclass_to_tibble() %>%
    select(-'data') %>%
    filter(!is.na(.data$value))

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
             year = min(regression_data$year):2150) %>%
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
  data_steel_production <- readSource('worldsteel', 'long', convert = FALSE) %>%
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
               match.steel.historic.values = match.steel.historic.values,
               match.steel.estimates = match.steel.estimates,
               China_Production = China_Production,
               aggregate = FALSE, supplementary = FALSE) %>%
      as.data.frame() %>%
      as_tibble() %>%
      select(scenario = 'Data1', iso3c = 'Region', variable = 'Data2',
             year = 'Year', value = 'Value') %>%
      character.data.frame() %>%
      mutate(year = as.integer(.data$year),
             # Gt/year * 1e9 t/Gt = t/year
             value = .data$value * 1e9,
             variable = sub('^ue_steel_(primary|secondary)$',
                            '\\1.production', .data$variable),
             scenario = sub('^gdp_', '', .data$scenario)) %>%
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
    duplicate(region = 'World') %>%
    sum_total_('iso3c') %>%
    pivot_wider() %>%
    mutate(GDPpC      = .data$GDP / .data$population,
           steel.VApt = .data$steel.VA / .data$steel.production) %>%
    select('scenario', 'region', 'iso3c', 'year', 'population', 'GDP',
           'steel.production', 'steel.VA', 'GDPpC', 'steel.VApt')

  ## plot steel VA =============================================================
  if (!is.null(save.plots)) {
    d_plot_region_totals <- regression_data_steel %>%
      ungroup() %>%
      filter('Total' == .data$iso3c,
             !.data$censored) %>%
      mutate(GDPpC = .data$GDP / .data$population,
             steel.VA.pt = .data$steel.VA / .data$steel.production) %>%
      # filter outliers
      filter(2000 >= .data$steel.VA.pt) %>%
      select('region', 'year', 'GDPpC', 'steel.VA.pt')

    d_plot_region_totals %>%
      filter('SSA' == .data$region) %>%
      select('region', 'steel.VA.pt') %>%
      mutate(cuts = cut(x = .data$steel.VA.pt,
                        breaks = seq_range(range(.data$steel.VA.pt),
                                           length.out = 31),
                        labels = 1:30, include.lowest = TRUE)) %>%
      group_by(!!!syms(c('region', 'cuts'))) %>%
      summarise(count = n(), .groups = 'drop_last') %>%
      mutate(cuts = as.integer(.data$cuts)) %>%
      ungroup() %>%
      complete(nesting(!!sym('region')), cuts = 1:30) %>%
      mutate(foo = cumsum(is.na(.data$count))) %>%
      filter(cumsum(is.na(.data$count)) > 30 / 2) %>%
      head(n = 1) %>%
      select(-'count', -'foo')

    d_plot_regression <- full_join(
      regression_parameters_steel,

      d_plot_region_totals %>%
        select('region', 'GDPpC') %>%
        df_populate_range('GDPpC'),

      'region'
    ) %>%
      mutate(steel.VA.pt = .data$a * exp(.data$b / .data$GDPpC)) %>%
      select('region', 'GDPpC', 'steel.VA.pt')

    d_plot_projections <- projected_steel_data %>%
      filter(.data$scenario %in% names(linetype_scenarios),
             'Total' == .data$iso3c) %>%
      select('scenario', 'region', 'year', 'GDPpC', steel.VA.pt = 'steel.VApt')

    d_plot_projections <- left_join(
      d_plot_projections,

      d_plot_projections %>%
        select('region', 'year', 'GDPpC') %>%
        filter(max(.data$year) == .data$year) %>%
        group_by(.data$region) %>%
        filter(min(.data$GDPpC) == .data$GDPpC) %>%
        select('region', max.GDPpC = 'GDPpC'),

      'region'
    ) %>%
      filter(.data$GDPpC <= .data$max.GDPpC) %>%
      select(-'max.GDPpC')

    p <- ggplot(mapping = aes(x = !!sym('GDPpC') / 1000,
                              y = !!sym('steel.VA.pt'))) +
      geom_point(data = d_plot_region_totals,
                 mapping = aes(shape = 'region totals')) +
      scale_shape_manual(values = c('region totals' = 'cross'),
                         name = NULL) +
      geom_line(data = d_plot_projections %>%
                  filter(2050 >= .data$year),
                mapping = aes(colour = !!sym('scenario'))) +
      geom_line(data = d_plot_regression,
                mapping = aes(colour = 'regression')) +
      scale_colour_manual(values = c('regression' = 'red',
                                     'SSP2' = 'black'),
                          name = NULL,
                          guide = guide_legend(direction = 'horizontal')) +
      facet_wrap(vars(!!sym('region')), scales = 'free') +
      expand_limits(x = 0, y = 0) +
      labs(x = 'per-capita GDP [1000$/yr]',
           y = 'specific Steel Value Added [$/t]') +
      theme_minimal() +
      theme(legend.justification = c(1, 0),
            legend.position = c(1, 0))

    ggsave(plot = p, filename = '04_Steel_VA_regressions_projections.svg',
           device = 'svg', path = save.plots, bg = 'white',
           width = 18, height = 14, units = 'cm', scale = 1.73)

    write_rds(x = p,
              file = file.path(save.plots,
                               '04_Steel_VA_regressions_projections.rds'))
  }

  # ========================================================================== =

  # project cement production ----
  ## calculate regression data ----
  regression_data_cement <- full_join(
    INDSTAT %>%
      filter(20 == .data$ctable,
             '26' == .data$isic,
             between(.data$utable, 17, 20),
             0 < .data$value) %>%
      group_by(.data$iso3c, .data$year) %>%
      filter(max(.data$lastupdated) == .data$lastupdated) %>%
      ungroup() %>%
      select('region', 'iso3c', 'year', cement.VA = 'value') %>%
      filter(.data$year >= min(data_cement_production$year)),

    data_cement_production %>%
      rename(cement.production = 'value') %>%
      left_join(region_mapping, 'iso3c'),

    c('region', 'iso3c', 'year')
  ) %>%
    filter(!is.na(.data$cement.production))

  ### censor nonsensical data ----
  cement_censor <- list_to_data_frame(list(
    BDI = 1980:2010,   # zero cement production
    CIV = 1990:1993,   # cement VA 100 times higher than before and after
    NAM = 2007:2010,   # zero cement production
    HKG = 1973:1979,   # no data for CHN prior to 1980
    IRQ = 1992:1997,   # cement VA 100 times higher than before and after
    RUS = 1970:1990,   # exclude data from Soviet period which biases
                       # projections up
    NULL),
    'iso3c', 'year') %>%
    mutate(censored = TRUE)

  regression_data_cement <- regression_data_cement %>%
    left_join(cement_censor, c('iso3c', 'year')) %>%
    mutate(censored = ifelse(is.na(.data$censored), FALSE, TRUE))

  ### compute regional and World aggregates ----
  regression_data_cement <- regression_data_cement %>%
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
    pivot_longer(c('population', 'cement.production', 'GDP', 'cement.VA')) %>%
    duplicate(region = 'World')

  regression_data_cement <- bind_rows(
    regression_data_cement,

    regression_data_cement %>%
      filter(!.data$censored,
             # exclude CHA from global regression data, because it dominates
             # the global regression
             !('World' == .data$region & 'CHN' == .data$iso3c)) %>%
      group_by(!!!syms(c('region', 'year', 'censored', 'name'))) %>%
      summarise(value = sum(.data$value, na.rm = TRUE),
                iso3c = 'Total',
                .groups = 'drop')
  ) %>%
    pivot_wider()

  ## compute regression parameters ----
  regression_parameters_cement_production <- tibble()
  for (r in sort(unique(regression_data_cement$region))) {
    regression_parameters_cement_production <- bind_rows(
      regression_parameters_cement_production,

      nls(formula = cement.PpC ~ a * exp(b / GDPpC),
          data = regression_data_cement %>%
            filter(r == .data$region,
                   'Total' == .data$iso3c,
                   !.data$censored) %>%
            mutate(cement.PpC = .data$cement.production / .data$population,
                   GDPpC      = .data$GDP / .data$population),
          start = list(a = 1, b = -1000),
          trace = FALSE) %>%
        tidy() %>%
        select('term', 'estimate') %>%
        pivot_wider(names_from = 'term', values_from = 'estimate') %>%
        mutate(region = r)
    )
  }

  ## project cement production per capita ----
  param_a <- regression_parameters_cement_production %>%
    filter('World' == .data$region) %>%
    pull('a')

  regression_parameters_cement_production_converging <-
    regression_parameters_cement_production %>%
    filter('World' != .data$region) %>%
    mutate(year = as.integer(2000)) %>%
    complete(nesting(!!!syms(c('region', 'a', 'b'))), year = 2000:2100) %>%
    mutate(a = .data$a
             + ( (param_a - .data$a)
               / (2200 - 2000)
               * (.data$year - 2000)
               ))

  projected_cement_data <- inner_join(
    regression_parameters_cement_production_converging,

    GDPpC,

    c('region', 'year')
  ) %>%
    mutate(cement.production = .data$a * exp(.data$b / .data$GDPpC)
                             * .data$population) %>%
    select('scenario', 'region', 'iso3c', 'year', 'population', 'GDP',
           'cement.production') %>%
    pivot_longer(c('population', 'GDP', 'cement.production')) %>%
    duplicate(region = 'World') %>%
    sum_total_('iso3c') %>%
    pivot_wider() %>%
    mutate(GDPpC      = .data$GDP / .data$population,
           cement.PpC = .data$cement.production / .data$population)

  last_cement_year <- max(data_cement_production$year)

  projected_cement_data <- left_join(
    projected_cement_data %>%
      select(-'GDPpC', -'cement.PpC') %>%
      filter('Total' != .data$iso3c),

    data_cement_production %>%
      rename(data = 'value'),

    c('iso3c', 'year')
  ) %>%
    group_by(!!!syms(c('scenario', 'region', 'iso3c'))) %>%
    mutate(
      shift.factor = .data$data / .data$cement.production,
      shift.factor = ifelse(
        between(.data$year, last_cement_year, last_cement_year + 14),
        1 + ( ( last(na.omit(.data$shift.factor)) - 1)
            * ((last_cement_year + 14 - .data$year) / 14) ^ 2
            ),
        ifelse(last_cement_year > .data$year, .data$shift.factor, 1)),
      shift.factor = na.approx(object = .data$shift.factor, x = .data$year,
                               yleft = first(na.omit(.data$shift.factor)),
                               yright = last(na.omit(.data$shift.factor)),
                               na.rm = FALSE),
      cement.production = .data$shift.factor * .data$cement.production) %>%
    ungroup() %>%
    select(-'data', -'shift.factor') %>%
    pivot_longer(c('cement.production', 'GDP', 'population')) %>%
    sum_total_('iso3c') %>%
    pivot_wider() %>%
    mutate(GDPpC      = .data$GDP / .data$population,
           cement.PpC = .data$cement.production / .data$population)

  # project cement VA ----
  ## compute regression parameters ----
  regression_parameters_cement <- tibble()
  for (r in sort(unique(regression_data_cement$region))) {
    regression_parameters_cement <- bind_rows(
      regression_parameters_cement,

      nls(formula = cement.VApt ~ a * exp(b / GDPpC),
          data = regression_data_cement %>%
            filter(r == .data$region,
                   'Total' != .data$iso3c,
                   !is.na(.data$cement.VA),
                   !.data$censored) %>%
            pivot_longer(all_of(c('population', 'cement.production', 'GDP',
                                  'cement.VA'))) %>%
            group_by(.data$region, .data$year, .data$censored, .data$name) %>%
            summarise(value = sum(.data$value),
                      iso3c = 'Total',
                      .groups = 'drop') %>%
            pivot_wider() %>%
            mutate(cement.VApt = .data$cement.VA / .data$cement.production,
                   GDPpC       = .data$GDP / .data$population),
          start = list(a = 250, b = -4000),
          trace = FALSE) %>%
        tidy() %>%
        select('term', 'estimate') %>%
        pivot_wider(names_from = 'term', values_from = 'estimate') %>%
        mutate(region = r)
    )
  }

  # project cement VA per tonne of cement ----
  parameter_a_world_cement <- regression_parameters_cement %>%
    filter('World' == .data$region) %>%
    pull('a')

  regression_parameters_cement_converging <- regression_parameters_cement %>%
    filter('World' != .data$region) %>%
    mutate(year = as.integer(2000)) %>%
    complete(nesting(!!!syms(c('region', 'a', 'b'))), year = 2000:2100) %>%
    mutate(a = .data$a
             + ( (parameter_a_world_cement - .data$a)
               / (2200 - 2000)
               * (.data$year - 2000)
               ))

  projected_cement_data <- inner_join(
    regression_parameters_cement_converging,

    projected_cement_data,

    c('region', 'year')
  ) %>%
    filter('Total' != .data$iso3c) %>%
    mutate(cement.VA = .data$a * exp(.data$b / (.data$GDP / .data$population))
                     * .data$cement.production) %>%
    select('scenario', 'region', 'iso3c', 'year', 'population', 'GDP',
           'cement.production', 'cement.VA') %>%
    pivot_longer(c('population', 'GDP', 'cement.production', 'cement.VA')) %>%
    sum_total_('iso3c') %>%
    pivot_wider()

  ## plot cement regressions ====
  if (!is.null(save.plots)) {
    d_plot_region_totals <- regression_data_cement %>%
      filter(!.data$censored,
             'Total' == .data$iso3c) %>%
      mutate(GDPpC = .data$GDP / .data$population)

    d_plot_countries <- regression_data_cement %>%
      filter(!.data$censored) %>%
      semi_join(
        regression_data_cement %>%
          filter(!.data$censored,
                 'World' != .data$region,
                 'Total' != .data$iso3c) %>%
          distinct(.data$region, .data$iso3c) %>%
          group_by(.data$region) %>%
          filter(1 != n()) %>%
          ungroup(),

        c('region', 'iso3c')
      ) %>%
      mutate(GDPpC = .data$GDP / .data$population)

    d_plot_projections <- projected_cement_data %>%
      filter(.data$scenario %in% names(linetype_scenarios),
             'Total' == .data$iso3c,
             between(.data$year, max(d_plot_region_totals$year), 2100)) %>%
      mutate(GDPpC = .data$GDP / .data$population) %>%
      inner_join(
        regression_data_cement %>%
          filter('Total' != .data$iso3c) %>%
          group_by(.data$region) %>%
          mutate(GDPpC = .data$GDP / .data$population) %>%
          filter(max(.data$GDPpC) == .data$GDPpC) %>%
          ungroup() %>%
          select('region', max.GDPpC = 'GDPpC'),

        'region'
      ) %>%
      filter(.data$GDPpC <= 2 * .data$max.GDPpC)

    d_plot_asymptote <- bind_cols(
      regression_parameters_cement_production %>%
        select('region', 'a') %>%
        filter('World' != .data$region),

      regression_parameters_cement_production %>%
        filter('World' == .data$region) %>%
        select(World = 'a')
    ) %>%
      mutate(a.conv = (.data$a + .data$World) / 2) %>%
      select(-'World') %>%
      pivot_longer(c('a', 'a.conv')) %>%
      full_join(
        tribble(
          ~name,      ~year,
          'a',        max(d_plot_region_totals$year),
          'a.conv',   2100),

        'name'
      ) %>%
      select('region', 'year', 'value')

    d_plot_asymptote <- full_join(
      bind_cols(
        regression_parameters_cement_production %>%
          select('region', 'a') %>%
          filter('World' != .data$region),

        regression_parameters_cement_production %>%
          filter('World' == .data$region) %>%
          select(World = 'a')
      ),

      d_plot_projections %>%
        group_by(.data$scenario, .data$region) %>%
        filter(.data$year %in% c(max(d_plot_region_totals$year),
                                 max(.data$year))) %>%
        select('scenario', 'region', 'year', 'GDPpC'),

      'region'
    ) %>%
      mutate(value = .data$a
             + ( (.data$World - .data$a)
                 / (2200 - 2000)
                 * (.data$year - 2000))) %>%
      select('scenario', 'region', 'GDPpC', 'value')

    d_plot_regression <- full_join(
      regression_parameters_cement_production,

      d_plot_region_totals %>%
        select('region', 'GDPpC') %>%
        df_populate_range('GDPpC'),

      'region'
    ) %>%
      mutate(value = .data$a * exp(.data$b / .data$GDPpC))

    d_plot_foo <- full_join(
      regression_parameters_cement_production,

      d_plot_projections %>%
        group_by(.data$scenario, .data$region) %>%
        filter(.data$year %in% c(max(d_plot_region_totals$year),
                                 max(.data$year))) %>%
        select('scenario', 'region', 'year', 'GDPpC'),

      'region'
    ) %>%
      mutate(value = .data$a * exp(.data$b / .data$GDPpC))

    y_max <- d_plot_region_totals %>%
      filter('World' == .data$region) %>%
      mutate(
        cement.production.pC = .data$cement.production / .data$population) %>%
      filter(max(.data$cement.production.pC) == .data$cement.production.pC) %>%
      pull('cement.production.pC')

    projection_points <- c(2015, 2030, 2050, 2075, 2100)

    p <- ggplot(mapping = aes(x = !!sym('GDPpC') / 1000,
                              y = !!sym('cement.production')
                              / !!sym('population'))) +
      # plot region totals
      geom_point(
        data = d_plot_region_totals,
        mapping = aes(shape = 'region totals'),
        size = 2) +
      # plot regression line
      geom_path(
        data = d_plot_regression,
        mapping = aes(y = !!sym('value'), colour = 'regression')) +
      # plot projections
      geom_path(
        data = d_plot_projections,
        mapping = aes(colour = 'projection')) +
      geom_point(
        data = d_plot_projections %>%
          filter(.data$year %in% projection_points),
        mapping = aes(shape = as.character(!!sym('year'))),
        size = 3) +
      scale_shape_manual(
        values = c('region totals' = 'o',
                   setNames(rep('x', length(projection_points)),
                            projection_points)),
        name = NULL) +
      scale_colour_manual(values = c('regression' = 'red',
                                     'projection' = 'black'),
                          name = NULL) +
      facet_wrap(vars(!!sym('region')), scales = 'free') +
      expand_limits(y = c(0, ceiling(y_max * 2) / 2)) +
      labs(x = 'per-capita GDP [1000 $/year]',
           y = 'per-capita Cement Production [tonnes/year]') +
      theme_minimal()


    ggsave(plot = p, filename = '01_Cement_regression_projection.svg',
           device = 'svg', path = save.plots, bg = 'white',
           width = 18, height = 14, units = 'cm', scale = 1.73)

    write_rds(x = p,
              file = file.path(save.plots,
                               '01_Cement_regression_projection.rds'))
  }

  # ========================================================================== =

  ## plot cement VA regressions ====
  if (!is.null(save.plots)) {
    d_plot_region_totals <- regression_data_cement %>%
      ungroup() %>%
      filter('Total' == .data$iso3c,
             !.data$censored) %>%
      mutate(GDPpC = .data$GDP / .data$population,
             cement.VA.pt = .data$cement.VA / .data$cement.production) %>%
      # filter outliers
      # filter(2000 >= .data$cement.VA.pt) %>%
      select('region', 'year', 'GDPpC', 'cement.VA.pt')

    d_plot_regression <- full_join(
      regression_parameters_cement,

      d_plot_region_totals %>%
        select('region', 'GDPpC') %>%
        df_populate_range('GDPpC'),

      'region'
    ) %>%
      mutate(cement.VA.pt = .data$a * exp(.data$b / .data$GDPpC)) %>%
      select('region', 'GDPpC', 'cement.VA.pt')

    d_plot_projections <- projected_cement_data %>%
      filter(.data$scenario %in% names(linetype_scenarios),
             'Total' == .data$iso3c) %>%
      mutate(GDPpC = .data$GDP / .data$population,
             cement.VA.pt = .data$cement.VA / .data$cement.production) %>%
      select('scenario', 'region', 'year', 'GDPpC', 'cement.VA.pt')

    d_plot_projections <- left_join(
      d_plot_projections,

      d_plot_projections %>%
        select('region', 'year', 'GDPpC') %>%
        filter(max(.data$year) == .data$year) %>%
        group_by(.data$region) %>%
        filter(min(.data$GDPpC) == .data$GDPpC) %>%
        select('region', max.GDPpC = 'GDPpC'),

      'region'
    ) %>%
      filter(.data$GDPpC <= .data$max.GDPpC) %>%
      select(-'max.GDPpC')

    p <- ggplot(mapping = aes(x = !!sym('GDPpC') / 1000,
                              y = !!sym('cement.VA.pt'))) +
      geom_point(data = d_plot_region_totals,
                 mapping = aes(shape = 'region totals')) +
      scale_shape_manual(values = c('region totals' = 'cross'),
                         name = NULL) +
      geom_line(data = d_plot_regression,
                mapping = aes(linetype = 'regression')) +
      geom_line(data = d_plot_projections,
                mapping = aes(linetype = !!sym('scenario'))) +
      scale_linetype_manual(values = linetype_scenarios, name = NULL,
                            guide = guide_legend(direction = 'horizontal')) +
      facet_wrap(vars(!!sym('region')), scales = 'free') +
      expand_limits(x = 0, y = 0) +
      labs(x = 'per-capita GDP [1000$/yr]',
           y = 'specific Cement Value Added [$/t]') +
      theme_minimal() +
      theme(legend.justification = c(1, 0),
            legend.position = c(1, 0))


    ggsave(plot = p, filename = '05a_Cement_VA_regressions_projections.svg',
           device = 'svg', path = save.plots, bg = 'white',
           width = 18, height = 14, units = 'cm', scale = 1.73)

    write_rds(x = p,
              file = file.path(save.plots,
                               '05_Cement_VA_regressions_projections.rds'))
  }

  # ========================================================================== =

  # project chemicals VA ----
  ## compile regression data ----
  regression_data_chemicals <- inner_join(
    INDSTAT %>%
      filter(20 == .data$ctable,
             '24' == .data$isic,
             between(.data$utable, 17, 20),
             0 < .data$value) %>%
      group_by(!!!syms(c('region', 'iso3c', 'year'))) %>%
      filter(max(.data$lastupdated) == .data$lastupdated) %>%
      ungroup() %>%
      select('region', 'iso3c', 'year', chemicals.VA = 'value'),

    INDSTAT %>%
      filter(20 == .data$ctable,
             'D' == .data$isic,
             between(.data$utable, 17, 20),
             0 < .data$value) %>%
      group_by(!!!syms(c('region', 'iso3c', 'year'))) %>%
      filter(max(.data$lastupdated) == .data$lastupdated) %>%
      ungroup() %>%
      select('region', 'iso3c', 'year', manufacturing.VA = 'value'),

    c('region', 'iso3c', 'year')
  ) %>%
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
    duplicate(region = 'World')

  ### censor nonsensical data ----
  chemicals_censor <- list_to_data_frame(list(
    CIV = 1989,
    NER = 1999:2002,
    HKG = c(1973:1979, 2008:2015),
    MAC = c(1978:1979)),
    'iso3c', 'year') %>%
    mutate(censored = TRUE)

  regression_data_chemicals <- regression_data_chemicals %>%
    left_join(chemicals_censor, c('iso3c', 'year')) %>%
    mutate(censored = ifelse(is.na(.data$censored), FALSE, TRUE))

  ### compute regional and World aggregates ----
  regression_data_chemicals <- bind_rows(
    regression_data_chemicals,

    regression_data_chemicals %>%
      filter(!.data$censored) %>%
      pivot_longer(c('population', 'GDP', 'manufacturing.VA',
                     'chemicals.VA')) %>%
      group_by(!!!syms(c('region', 'year', 'censored', 'name'))) %>%
      summarise(value = sum(.data$value),
                iso3c = 'Total',
                .groups = 'drop') %>%
      pivot_wider()
  ) %>%
    mutate(chemicals.share = .data$chemicals.VA / .data$manufacturing.VA,
           GDPpC           = .data$GDP / .data$population)

  ## compute regression parameters ----
  regression_parameters_chemicals <- tibble()
  for (r in sort(unique(regression_data_chemicals$region))) {
    regression_parameters_chemicals <- bind_rows(
      regression_parameters_chemicals,

      nls(formula = chemicals.VA / population ~ a * exp(b / GDPpC),
          data = regression_data_chemicals %>%
            filter(r == .data$region,
                   'Total' == .data$iso3c,
                   !.data$censored),
          start = list(a = 1000, b = -100),
          trace = FALSE) %>%
        tidy() %>%
        select('term', 'estimate') %>%
        pivot_wider(names_from = 'term', values_from = 'estimate') %>%
        mutate(region = r)
    )
  }

  ## replace outliers with global parameters
  outliers_chemicals <- regression_parameters_chemicals %>%
    select('region', 'a') %>%
    filter(abs((.data$a - mean(.data$a)) / sd(.data$a)) > 3) %>%
    pull('region')

  regression_parameters_chemicals <- bind_rows(
    regression_parameters_chemicals %>%
      filter(!.data$region %in% outliers_chemicals),

    tibble(
      regression_parameters_chemicals %>%
        filter('World' == .data$region) %>%
        select(-'region'),

      region = outliers_chemicals)
  )

  ## project chemicals VA and share ----
  param_a <- regression_parameters_chemicals %>%
    filter('World' == .data$region) %>%
    pull('a')

  regression_parameters_chemicals_converging <-
    regression_parameters_chemicals %>%
    filter('World' != .data$region) %>%
    mutate(year = as.integer(2000)) %>%
    complete(nesting(!!!syms(c('region', 'a', 'b'))), year = 2000:2100) %>%
    mutate(a = .data$a
             + ( (param_a - .data$a)
               / (2200 - 2000)
               * (.data$year - 2000)
               ))

  projected_chemicals_data <- inner_join(
    regression_parameters_chemicals_converging,

    projected_data,

    c('region', 'year')
  ) %>%
    mutate(
      chemicals.VA = .data$a * exp(.data$b / (.data$GDP / .data$population))
                   * .data$population) %>%
    select('scenario', 'region', 'iso3c', 'year', 'population', 'GDP',
           'manufacturing', 'chemicals.VA') %>%
    pivot_longer(c('population', 'GDP', 'manufacturing', 'chemicals.VA')) %>%
    duplicate(region = 'World') %>%
    sum_total_('iso3c') %>%
    pivot_wider() %>%
    mutate(GDPpC           = .data$GDP / .data$population,
           chemicals.share = .data$chemicals.VA / .data$manufacturing)

  ## plot chemicals regressions ================================================
  if (!is.null(save.plots)) {
    d_plot_region_totals <- regression_data_chemicals %>%
      filter(!.data$censored,
             'Total' == .data$iso3c)

    d_plot_countries <- regression_data_chemicals %>%
      filter(!.data$censored) %>%
      semi_join(
        regression_data_chemicals %>%
          filter(!.data$censored,
                 'World' != .data$region,
                 'Total' != .data$iso3c) %>%
          distinct(.data$region, .data$iso3c) %>%
          group_by(.data$region) %>%
          filter(1 != n()) %>%
          ungroup(),

        c('region', 'iso3c')
      ) %>%
      mutate(GDPpC = .data$GDP / .data$population)

    d_plot_regression <- full_join(
      regression_parameters_chemicals,

      d_plot_region_totals %>%
        select('region', 'GDPpC') %>%
        df_populate_range('GDPpC'),

      'region'
    ) %>%
      mutate(value = .data$a * exp(.data$b / .data$GDPpC))

    d_plot_projections <- projected_chemicals_data %>%
      filter(.data$scenario %in% names(linetype_scenarios),
             'Total' == .data$iso3c,
             between(.data$year, max(d_plot_region_totals$year), 2100)) %>%
      select('scenario', 'region', 'year', 'GDPpC', 'chemicals.VA',
             'population') %>%
      group_by(.data$region) %>%
      filter(.data$GDPpC <= .data$GDPpC[  'SSP2' == .data$scenario
                                          & 2100 == .data$year]) %>%
      ungroup()

    p <- ggplot(
      mapping = aes(x = !!sym('GDPpC') / 1000,
                    y = !!sym('chemicals.VA') / !!sym('population'))) +
      # plot region totals
      geom_point(
        data = d_plot_region_totals,
        mapping = aes(shape = 'region totals')) +
      # # plot regression line
      geom_path(
        data = d_plot_regression,
        mapping = aes(y = !!sym('value'), colour = 'regression')) +
      # # plot projections
      geom_path(
        data = d_plot_projections,
        mapping = aes(colour = 'projection')) +
      geom_point(
        data = d_plot_projections %>%
          filter(.data$year %in% projection_points),
        mapping = aes(shape = as.character(!!sym('year'))),
        size = 3) +
      scale_shape_manual(
        values = c('region totals' = 'o',
                   setNames(rep('x', length(projection_points)),
                            projection_points)),
                   name = NULL) +
      scale_colour_manual(values = c('regression' = 'red',
                                     'projection' = 'black'),
                            name = NULL) +
      facet_wrap(vars(!!sym('region')), scales = 'free') +
      expand_limits(y = c(0, ceiling(y_max * 2) / 2)) +
      labs(x = 'per-capita GDP [1000 $/year]',
           y = 'per-capita Chemicals Value Added [$/year]') +
      theme_minimal()

    ggsave(plot = p, filename = '02_Chemicals_regression_projection.svg',
           device = 'svg', path = save.plots, bg = 'white',
           width = 18, height = 14, units = 'cm', scale = 1.73)

    write_rds(x = p,
              file = file.path(save.plots,
                               '02_Chemicals_regression_projection.rds'))
  }

  # ======================================================================== ===

  # calculate other Industries Value Added projections ----
  projections <- bind_rows(
    projected_data %>%
      filter('Total' != .data$iso3c, 'World' != .data$region) %>%
      select('scenario', 'region', 'iso3c', 'year', 'population', 'GDP',
             manufacturing.VA = 'manufacturing') %>%
      pivot_longer(c('population', 'GDP', 'manufacturing.VA')),

    projected_cement_data %>%
      filter('Total' != .data$iso3c, 'World' != .data$region) %>%
      select('scenario', 'region', 'iso3c', 'year', 'cement.production',
             'cement.VA') %>%
      pivot_longer(c('cement.production', 'cement.VA')),

    projected_chemicals_data %>%
      filter('Total' != .data$iso3c, 'World' != .data$region) %>%
      select('scenario', 'region', 'iso3c', 'year', 'chemicals.VA') %>%
      pivot_longer('chemicals.VA'),

    projected_steel_data %>%
      filter('Total' != .data$iso3c, 'World' != .data$region) %>%
      select('scenario', 'region', 'iso3c', 'year', 'steel.production',
             'steel.VA') %>%
      pivot_longer(c('steel.production', 'steel.VA'))
  ) %>%
    pivot_wider() %>%
    mutate(otherInd.VA = .data$manufacturing.VA
                       - .data$cement.VA
                       - .data$chemicals.VA
                       - .data$steel.VA)

  ## filter projections with negative VA for otherInd ----
  tmp_negative_otherInd <- projections %>%
    select('scenario', 'region', 'iso3c', 'year', 'otherInd.VA') %>%
    filter(0 > .data$otherInd.VA) %>%
    select(-'otherInd.VA')

  # scenario/region/year combinations with negative values for all countries
  tmp_all_negative_otherInd <- projections %>%
    select('scenario', 'region', 'iso3c', 'year', 'otherInd.VA') %>%
    group_by(!!!syms(c('scenario', 'region', 'year'))) %>%
    filter(sum(0 > .data$otherInd.VA) == n()) %>%
    ungroup() %>%
    select('scenario', 'region', 'year')

  ## calculate regional shares  ----
  # disregarding negative VA for otherInd
  projections_regional_shares <- projections %>%
    select('scenario', 'region', 'iso3c', 'year', matches('VA$')) %>%
    anti_join(
      tmp_negative_otherInd %>%
        select(-'region'),

      c('scenario', 'iso3c', 'year')
    ) %>%
    pivot_longer(matches('VA$')) %>%
    assert(within_bounds(0, Inf), .data$value) %>%
    group_by(!!!syms(c('scenario', 'region', 'year', 'name'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop') %>%
    pivot_wider() %>%
    pivot_longer(matches('^(cement|chemicals|steel|otherInd)\\.VA$'),
                 names_to = 'variable') %>%
    mutate(share = .data$value / .data$manufacturing.VA) %>%
    select('scenario', 'region', 'year', 'variable', 'share') %>%
    # fill temporal gaps arising when for some combination of
    # scenario/region/year the otherInd.VA for all countries are negative
    interpolate_missing_periods_(
      periods = list('year' = unique(projections$year)), value = 'share',
      expand.values = TRUE)

  ## replace VA projections with negative otherInd via regional shares ----
  projections <- bind_rows(
    projections %>%
      select('scenario', 'region', 'iso3c', 'year', 'manufacturing.VA') %>%
      semi_join(
        tmp_negative_otherInd,

        c('scenario', 'region', 'iso3c', 'year')
      ) %>%
      inner_join(
        projections_regional_shares,

        c('scenario', 'region', 'year')
      ) %>%
      mutate(value = .data$manufacturing.VA * .data$share) %>%
      select(-'manufacturing.VA', -'share'),

    projections %>%
      pivot_longer(c('population', 'GDP', 'manufacturing.VA',
                     'cement.production', 'cement.VA', 'chemicals.VA',
                     'steel.production', 'steel.VA', 'otherInd.VA'),
                   names_to = 'variable') %>%
      anti_join(
        tmp_negative_otherInd %>%
          mutate(variable = '') %>%
          complete(nesting(!!!syms(c('scenario', 'region', 'iso3c', 'year'))),
                   variable = c('cement.VA', 'chemicals.VA', 'steel.VA',
                                       'otherInd.VA')),

        c('scenario', 'region', 'iso3c', 'year', 'variable')
      )
  ) %>%
    duplicate(region = 'World') %>%
    sum_total_('iso3c') %>%
    filter(!('World' == .data$region & 'Total' != .data$iso3c)) %>%
    pivot_wider(names_from = 'variable')

  # plot otherInd VA ===========================================================

  if (!is.null(save.plots)) {
    p <- ggplot() +
      geom_area(
        data = projections %>%
          select('scenario', 'region', 'iso3c', 'year', 'cement.VA',
                 'chemicals.VA', 'steel.VA', 'otherInd.VA') %>%
          filter('SSP2' == .data$scenario,
                 2000 <= .data$year,
                 'Total' == .data$iso3c) %>%
          select(-'scenario', -'iso3c') %>%
          pivot_longer(matches('\\.VA$')) %>%
          mutate(name = sub('\\.VA$', '', .data$name)) %>%
          order.levels(
            name = c('cement', 'chemicals', 'steel', 'otherInd')),
        mapping = aes(x = !!sym('year'), y = !!sym('value') / 1e12,
                      fill = !!sym('name'))) +
      scale_fill_discrete(name = NULL,
                          guide = guide_legend(direction = 'horizontal')) +
      facet_wrap(vars(!!sym('region')), scales = 'free_y') +
      labs(x = NULL, y = 'Value Added [$tn/year]') +
      theme_minimal() +
      theme(legend.justification = c(1, 0), legend.position = c(1, 0))

    ggsave(plot = p, filename = '05b_Value_Added_projection.svg',
           device = 'svg', path = save.plots, bg = 'white',
           width = 18, height = 14, units = 'cm', scale = 1.73)

    write_rds(x = p,
              file = file.path(save.plots,
                               '05_Value_Added_projection.rds'))
  }

  # ========================================================================== =

  # construct output ----
  if ('physical' == subtype) {
    x <- projections %>%
      filter(1993 <= .data$year,
             'Total' != .data$iso3c,
             'World' != .data$region) %>%
      select('scenario', 'iso3c', 'year', ue_cement = 'cement.production',
             ue_chemicals = 'chemicals.VA', ue_otherInd = 'otherInd.VA') %>%
      pivot_longer(matches('^ue_'), names_to = 'pf') %>%
      verify(!(is.na(.data$value) & between(.data$year, 2000, 2100))) %>%
      # t/year * 1e-9 Gt/t = Gt/year      | cement
      # $/year * 1e-12 $tn/$ = $tn/year   | chemicals and other industry
      mutate(
        value = .data$value * case_when(
          'ue_cement'    == pf ~ 1e-9,
          'ue_chemicals' == pf ~ 1e-12,
          'ue_otherInd'  == pf ~ 1e-12),
        scenario = paste0('gdp_', .data$scenario)) %>%
      interpolate_missing_periods_(periods = list(year = 1993:2150),
                                   expand.values = TRUE) %>%
      select('scenario', 'iso3c', 'pf', 'year', 'value')
  }

  if ('economic' == subtype) {
    x <- projections %>%
      filter(1993 <= .data$year,
             'Total' != .data$iso3c,
             'World' != .data$region) %>%
      select('scenario', 'iso3c', 'year', 'cement.VA', 'chemicals.VA',
             'steel.VA', 'otherInd.VA') %>%
      pivot_longer(matches('\\.VA$')) %>%
      # $/yr * 1e12 $tn/$ = $tn/yr
      mutate(value = .data$value * 1e-12,
             scenario = paste0('gdp_', .data$scenario)) %>%
      select('scenario', 'iso3c', 'name', 'year', 'value')
  }

  # return statement ----
  return(list(x = x %>%
                as.magpie(spatial = 2, temporal = 4, data = 5),
              weight = NULL,
              unit = '$tn/year',
              description = 'chemicals and other industry value added'))

}
