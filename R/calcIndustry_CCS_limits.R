#' Calculate Limits on Industry CCS Capacities
#'
#' @md
#' @details
#' The limits on industry CCS capacities are calculated from data of the
#' [Global Status of CCS 2023](zotero://select/items/3_E5GNNPZ8) report (through
#' [`readGlobalCCSinstitute()`].  CCS projects are
#' - filtered for valid (i.e. not "Under Evaluation") data for "Operation date"
#'   and "CO~2~ capture capacity"
#' - assigned to REMIND industry subsectors according to `facility_subsector`,
#'   which defaults to
#'   | Facility Industry               | subsector |
#'   |:--------------------------------|:----------|
#'   | Cement                          | cement    |
#'   | Chemical                        | chemicals |
#'   | Hydrogen / Ammonia / Fertiliser | chemicals |
#'   | Ethan                           | chemicals |
#'   | Iron and Steel Production       | steel     |
#'
#' - weighted by lifecycle stage according to `stage_weight`, which defaults to
#'    | Lifecycle stage      | weight |
#'    |:---------------------|-------:|
#'    | Operational          | 100 %  |
#'    | In construction      | 100 %  |
#'    | Advanced development |  50 %  |
#'    | Early development    |  20 %  |
#'
#' The resulting project capacities constitute the limits on industry subsector
#' CCS capacity for 2025.  The limit on CCS capacities for regions (or countries
#' if `region_mapping` is `NULL`) is set to a value of total 2025 subsector CCS
#' capacity, times the regions share in subsector activity (e.g. cement
#' production) of the SSP2EU scenario
#' - in 2030 if the region as some CCS capacity in 2025 in a different industry
#'   subsector, or
#' - in 2035 if the region has no industry CCS capacity in 2030 at all.
#'
#' CCS capacities are increased by the annual growth factor `a1` for the ten
#' first years, and by the annual growth factor `a2` afterwards (defaulting to
#' 70 % and 20 %, respectively).
#'
#' @param a1,a2 Annual growth factors of CCS capacity limits, for the first ten
#'     years and thereafter, default to `0.7` and `0.2` (70 % and 20 %,
#'     respectively).
#' @param installation_minimum Minimum emission capacity (in MtCO~2~/year)
#'     capacities are rounded up to.  Defaults to `0.5` (500 ktCO~2~/year).
#' @param stage_weight  A named vector of weight factors for different lifecycle
#'     stages.  See Details.
#' @param facility_subsector A named vector mapping the "Facility Industry" of
#'     CCS projects to REMIND industry subsectors.  See Details.
#' @param region_mapping A data frame with columns `iso3c` and `region` detailing
#'     the regional resolution on which data should be extrapolated.  If `NULL`
#'     (the default), extrapolation is done at the country level.
#'
#' @return A list with a [`magpie`][magclass::magclass] object `x`, `weight`,
#'   `unit`, `description`, and `min`.
#'
#' @author Michaja Pehl
#'
#' @importFrom dplyr arrange group_by full_join inner_join left_join mutate
#'     pull rename select summarise tibble ungroup
#' @importFrom quitte add_countrycode_ madrat_mule magclass_to_tibble
#' @importFrom readr read_delim
#' @importFrom rlang .data
#' @importFrom tidyr complete fill pivot_longer replace_na
#'
#' @export
calcIndustry_CCS_limits <- function(
    a1 = 0.3, a2 = 0.15,
    installation_minimum = 1,
    stage_weight = c('Operational'          = 1,
                     'In construction'      = 1,
                     'Advanced development' = 0.5,
                     'Early development'    = 0.2),
    facility_subsector = c('Cement'                          = 'cement',
                           'Chemical'                        = 'chemicals',
                           'Hydrogen / Ammonia / Fertiliser' = 'chemicals',
                           'Ethan'                           = 'chemicals',
                           'Iron and Steel Production'       = 'steel'),
    region_mapping = NULL) {

  # configuration ----
  stage_weight <- tibble(stage = names(stage_weight), factor = stage_weight)

  facility_subsector <- tibble(`Facility Industry` = names(facility_subsector),
                               subsector = facility_subsector)

  remind_timesteps <- unique(quitte::remind_timesteps$period)

  ## read SSP2EU industry activity ----
  ind_activity <- calcOutput('FEdemand', aggregate = FALSE,
                             years = remind_timesteps) %>%
    `[`(,,paste0('gdp_SSP2EU.',
                 c('ue_cement', 'ue_chemicals', 'ue_steel_primary'))) %>%
    magclass_to_tibble() %>%
    mutate(subsector = sub('ue_([^_]+).*', '\\1', .data$item),
           .keep = 'unused') %>%
    select(iso3c = 'region', 'subsector', period= 'year',  activity = 'value')

  ## set/check region mapping ----
  iso3c_list <- read_delim(
      file = system.file('extdata', 'iso_country.csv', package = 'madrat'),
      delim = ';',
      col_names = c('-', 'iso3c'),
      col_types = '-c',
      skip = 1) %>%
    pull('iso3c') %>%
    sort()

  if (is.null(region_mapping)) {
    region_mapping <- tibble(iso3c = iso3c_list, region = iso3c_list)
  }
  else if (!setequal(region_mapping[['iso3c']], iso3c_list)) {
    stop('region_mapping iso3c column does not match madrat prescribed iso3c list')
  }

  # calculation ----
  x <- readSource('GlobalCCSinstitute', '2023-11', convert = FALSE) %>%
    madrat_mule() %>%
    rename(value = 'Capture, transport and/or storage capacity (Mtpa CO2)',
           stage = 'Lifecycle stage') %>%
    filter(!is.na(.data$value)) %>%
    # filter for facility industry
    inner_join(facility_subsector, 'Facility Industry') %>%
    add_countrycode_(origin = c('Country' = 'country.name'),
                     destination = 'iso3c') %>%
    left_join(region_mapping, 'iso3c' ) %>%
    left_join(stage_weight, 'stage') %>%
    # split facilities existing already in 2025 or only in 2030
    mutate(`2025` = .data$`Operational date` <= 2027,
           `2030` = .data$`Operational date` <= 2032) %>%
    pivot_longer(cols = c('2025', '2030'), names_to = 'period',
                 names_transform = as.integer, values_to = 'include') %>%
    filter(.data$include) %>%
    select(-'include') %>%
    # regional aggregation and applying stage factors
    group_by(.data$period, .data$region, .data$subsector) %>%
    summarise(value = sum(.data$value * .data$factor), .groups = 'drop') %>%
    complete(crossing(!!!syms(c('region', 'subsector', 'period'))),
             fill = list(value = 0)) %>%
    full_join(
      ind_activity %>%
        full_join(region_mapping, 'iso3c') %>%
        group_by(.data$region, .data$subsector, .data$period) %>%
        summarise(activity = sum(.data$activity), .groups = 'drop'),

      c('region', 'subsector', 'period')
    ) %>%
    replace_na(list(period = 2025L, value = 0)) %>%
    # classes: - A: non-zero 2025 data (is continued)
    #          - B: zero 2025 data, but non-zero 2025 data in different
    #               subsector (gets initialised in 2030)
    #          - C: zero 2025 data in all subsectors (get initialised in 2035)
    group_by(.data$region, .data$subsector) %>%
    mutate(
      class = ifelse(0 != .data$value[2025 == .data$period], 'A', NA)) %>%
    group_by(.data$region) %>%
    mutate(
      class = case_when(
        'A' == .data$class                          ~ .data$class,
        0 != sum(.data$value[2025 == .data$period]) ~ 'B',
        0 == sum(.data$value[2025 == .data$period]) ~ 'C')) %>%
    ungroup() %>%
    complete(nesting(!!!syms(c('region', 'subsector', 'class'))),
             period = remind_timesteps,
             fill = list(value = NA)) %>%
    group_by(.data$period, .data$subsector) %>%
    mutate(activity.total = sum(.data$activity, na.rm = TRUE),
           value.total = case_when(
             .data$period <= 2030 ~ sum(.data$value, na.rm = TRUE))) %>%
    group_by(.data$subsector) %>%
    fill('value.total', .direction = 'down') %>%
    arrange(.data$region, .data$subsector, .data$period) %>%
    # conversion factors for activity to emissions for cement and steel
    full_join(
      tribble(
        ~subsector,    ~factor,   # ad-hoc emission factors
        'cement',      0.78,      # Gt cement/a -> GtCO2/a
        'chemicals',   NA,
        'steel',       1.85),     # Gt steel/a  -> GtCO2/a

      'subsector') %>%
    group_by(.data$region, .data$subsector) %>%
    mutate(
      value = case_when(
        2045 < .data$period ~ NA,
        TRUE                ~ .data$value),

      value = case_when(
        ## before 2025 ----
        .data$period <  2025 ~ 0,

        # 2025 just keeps data present

        ## 2030 ----

        'A' == .data$class & 2030 == .data$period ~
          # either the 2030 value, or the expanded 2025 value, whichever is
          # higher
          max(sum(.data$value, na.rm = TRUE),
              .data$value[2025 == .data$period] * (1 + a1) ^ 5),

        'B' == .data$class & 2030 == .data$period ~
          # global 2025 subsector CCS times regional share in global subsector
          # activity, rounded up to <installation_minimum> MtCO2/a, but not
          # exceeding regional subsector emissions
          (.data$value.total * .data$activity / .data$activity.total) %>%
          max(installation_minimum) %>%
          min(.data$activity * 1e3 * .data$factor, na.rm = TRUE),

        'C' == .data$class & 2030 == .data$period ~ 0,

        TRUE ~ .data$value),

      ## 2035 ----
      value = case_when(
        .data$class %in% c('A', 'B') & 2035 == .data$period ~
          # expanded 2030 value
          .data$value[2030 == .data$period] * (1 + a1) ^ 5,

        'C' == .data$class & 2035 == .data$period ~
          # global 2025 subsector CCS times regional share in global subsector
          # activity, rounded up to <installation_minimum> MtCO2/a, but not
          # exceeding regional subsector emissions
          (.data$value.total * .data$activity / .data$activity.total) %>%
          max(installation_minimum) %>%
          min(.data$activity * 1e3 * .data$factor, na.rm = TRUE),

        TRUE ~ .data$value),

      ## 2040 ----
      value = case_when(
        'A' == .data$class & 2040 == .data$period ~
          # expand 2035 value, using a2
          .data$value[2035 == .data$period] * (1 + a2) ^ 5,

        .data$class %in% c('B', 'C') & 2040 == .data$period ~
          # expand 2035 value, using a1
          .data$value[2035 == .data$period] * (1 + a1) ^ 5,

        TRUE ~ .data$value),

      ## 2045 ----
      value = case_when(
        .data$class %in% c('A', 'B') & 2045 == .data$period ~
          # expand 2040 value, using a2
          .data$value[2040 == .data$period] * (1 + a2) ^ 5,

        'C' == .data$class & 2045 == .data$period ~
          # expand 2040 value, using a1
          .data$value[2040 == .data$period] * (1 + a1) ^ 5,

        TRUE ~ .data$value),

      ## after 2045 ----
      value = ifelse(!is.na(.data$value), .data$value,
                     ( last(.data$value[!is.na(.data$value)])
                     * (1 + a2)
                     ^ (.data$period - last(.data$period[!is.na(.data$value)]))
                     ))) %>%
    ungroup() %>%
    select('region', 'subsector', 'period', 'value') %>%
    # expand from regions to iso3c (if different), convert unit
    full_join(
      full_join(ind_activity, region_mapping, 'iso3c'),

      by = c('region', 'subsector', 'period'),
      relationship = 'many-to-many'
    ) %>%
    group_by(.data$region, .data$subsector, .data$period) %>%
    ## convert units ----
    # MtCO2/yr * 1e-3 Gt/Mt / (44/12 CO2/C) = GtC/yr
    mutate(value = .data$value
                 * .data$activity / sum(.data$activity)
                 * 12/44 * 1e-3) %>%
    ungroup() %>%
    select('iso3c', 'period', 'subsector', 'value')

  return(list(x = as.magpie(x = x, spatial = 1, temporal = 2, datacol = 4),
              weight = NULL,
              unit = 'GtC/yr',
              description = 'Limits on Industry CCS Capacities',
              min = 0))
}
