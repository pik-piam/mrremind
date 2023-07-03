#' Calculate Cement Share in NONMET FE Use
#'
#' Estimated shares of cement in `NONMET` final energy use based on OECD and
#' Non-OECD figures from IEA 2017 [Energy Technology Perspectives](
#' https://www.zotero.org/groups/52011/rd3/items/X8XCUJ5U).  Shares are weighted
#' by GDP for aggregation and converge towards global values by 2100.
#'
#' @md
#' @return A list with a [`magpie`][magclass::magclass] object `x`, `weight`,
#'   `unit`, `description`, `min`, and `max`.
#'
#' @author Michaja Pehl
#'
#' @seealso [`calcOutput()`]
#'
#' @importFrom dplyr bind_rows left_join select filter mutate pull
#' @importFrom madrat toolGetMapping calcOutput
#' @importFrom magclass as.magpie dimSums
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom tibble tribble as_tibble
#' @importFrom tidyr pivot_longer complete nesting

#' @export
calcCementShare <- function() {
  cement_NONMET_share <- tribble(
    ~fety,           ~OECD,   ~`Non-OECD`,   ~World,
    'feso_cement',   0.80,    0.68,          0.69,
    'feli_cement',   0.73,    0.62,          0.66,
    'fega_cement',   0.03,    0.75,          0.40,
    'feh2_cement',   0.03,    0.75,          0.40,
    'feel_cement',   0.34,    0.68,          0.59) %>%
    pivot_longer(-'fety', names_to = 'region')

  list(x = bind_rows(
    left_join(
      toolGetMapping('regionmappingOECD.csv', 'regional', where = "mappingfolder") %>%
        as_tibble() %>%
        select(iso3c = 'CountryCode', region = 'RegionCode'),

      cement_NONMET_share %>%
        filter('World' != !!sym('region')),

      'region'
    ) %>%
      select(-'region') %>%
      mutate(year = 2015),

    cement_NONMET_share %>%
      filter('World' == !!sym('region')) %>%
      select(-'region') %>%
      mutate(iso3c = NA_character_) %>%
      complete(nesting(!!sym('fety'), !!sym('value')),
               iso3c = toolGetMapping('regionmappingOECD.csv', 'regional') %>%
                 pull('CountryCode')) %>%
      filter(!is.na(!!sym('iso3c'))) %>%
      mutate(year = 2100)
  ) %>%
    quitte::interpolate_missing_periods(
      year = unique(quitte::remind_timesteps$period),
      expand.values = TRUE) %>%
    select('iso3c', 'year', 'fety', 'value') %>%
    as.magpie(spatial = 1, temporal = 2, data = 4),

  weight = calcOutput('GDP', aggregate = FALSE) %>%
    `[`(,unique(quitte::remind_timesteps$period),'gdp_SSP2') %>%
    dimSums(dim = 3),

  unit = 'share',
  description = 'Share of Cement in NONMET FE use',
  min = 0, max = 1)
}
