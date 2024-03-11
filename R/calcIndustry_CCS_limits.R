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
#' CCS capacity for 2030.  The limit on CCS capacities for regions (or countries
#' if `region_mapping` is `NULL`) is set to a value of total 2030 subsector CCS
#' capacity, times the regions share in 2030 subsector activity (e.g. cement
#' production)
#' - in 2035 if the region as some CCS capacity in 2030 in a different industry
#'   subsector, or
#' - in 2040 if the region has no industry CCS capacity in 2030 at all.
#'
#' CCS capacities are increase by the annual growth factor (default: 20 %/p.a.).
#'
#' @param annual_growth_factor Annual growth factor of CCS capacity limits,
#'     defaults to 1.2 (20 % annually).
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
#' @importFrom dplyr %>% group_by full_join inner_join left_join mutate pull
#'     rename select summarise tibble ungroup
#' @importFrom quitte add_countrycode_ madrat_mule magclass_to_tibble
#' @importFrom readr read_delim
#' @importFrom tidyr complete pivot_longer
#'
#' @export
calcIndustry_CCS_limits <- function(
    annual_growth_factor = 1.2, # 20 %
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

  ## read 2030 SSP2EU industry activity ----
  ind_activity_2030 <- calcOutput('FEdemand', aggregate = FALSE,
                                  years = 2030) %>%
    `[`(,,paste0('gdp_SSP2EU.',
                 c('ue_cement', 'ue_chemicals', 'ue_steel_primary'))) %>%
    magclass_to_tibble() %>%
    mutate(subsector = sub('ue_([^_]+).*', '\\1', .data$item),
           .keep = 'unused') %>%
    select(iso3c = 'region', 'subsector', activity = 'value')

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
    # regional aggregation and applying stage factors
    group_by(.data$region, .data$subsector) %>%
    summarise(value = sum(.data$value * .data$factor), .groups = 'drop') %>%
    # set data for missing regions/countries to zero
    complete(region = sort(unique(region_mapping$region)),
             .data$subsector,
             fill = list(value = 0)) %>%
    # calculate data for regions/countries w/o CCS projects through scaling with
    # industry subsector activity
    full_join(
      full_join(ind_activity_2030, region_mapping, 'iso3c') %>%
        group_by(.data$region, .data$subsector) %>%
        summarise(activity = sum(.data$activity), .groups = 'drop'),

      c('region', 'subsector')
    ) %>%
    group_by(.data$subsector) %>%
    mutate(total.value = sum(.data$value, na.rm = TRUE),
           total.activity = sum(.data$activity, na.rm = TRUE)) %>%
    group_by(.data$region) %>%
    mutate(
      value_2035 = case_when(
        0 != .data$value ~
          .data$value * annual_growth_factor ^ 5,
        0 != sum(.data$value, na.rm = TRUE) ~
          .data$total.value / .data$total.activity * .data$activity,
        TRUE ~
          0),
      value_2040 = case_when(
        0 != .data$value ~
          .data$value * annual_growth_factor ^ 10,
        0 != .data$value_2035 ~
          .data$value_2035 * annual_growth_factor ^ 5,
        TRUE ~
          .data$total.value / .data$total.activity * .data$activity)) %>%
    ungroup() %>%
    select('region', 'subsector',
           '2030' = 'value', '2035' = 'value_2035', '2040' = 'value_2040') %>%
    pivot_longer(c('2030', '2035', '2040'), names_to = 'period',
                 names_transform = as.integer) %>%
    # expand from regions to iso3c (if different), convert unit
    full_join(
      full_join(ind_activity_2030, region_mapping, 'iso3c'),

      by = c('region', 'subsector'),
      relationship = 'many-to-many'
    ) %>%
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
