#' Expand tibble across scenarios and regions with default values
#'
#' The data.frame `d` is expanded in such a manner that all rows with `NA` in
#' either the `scenario` or `region` columns are extended to repeat for all
#' scenarios and regions listed in `scenarios` and `regions`.  Rows with
#' specified scenarios and/or regions will overwrite extended ones.  Regions are
#' expanded before scenarios.
#'
#' @param d A data.frame with columns `scenario` and `region`.
#' @param scenarios A character vector of scenario names.
#' @param regions A character vector of region names.
#' @param structure.columns A character vector of column names to be carried
#'   along.
#'
#' @return A `tibble`.
#'
#' @importFrom dplyr anti_join bind_rows filter select
#' @importFrom magrittr %>%
#' @importFrom rlang syms
#' @importFrom tidyr complete nesting
#' @importFrom tidyselect all_of
#'
#' @examples
#' \dontrun{
#' tribble(
#'   ~scenario,   ~region,   ~value,
#'   NA,          NA,        0,
#'   NA,          'CHA',     1,
#'   'SSP1',      NA,        2,
#'   'SSP2EU',    'DEU',     3) %>%
#'   tool_expand_tibble(scenarios = c('SSP1', 'SSP2EU', 'SSP5'),
#'                      regions = c('CHA', 'DEU', 'USA')) %>%
#'   pivot_wider(names_from = 'region')
#'
#' tribble(
#'   ~scenario,   ~region,   ~name,   ~value,
#'   NA,          NA,        'A',     0,
#'   NA,          'CHA',     'B',     1,
#'   'SSP1',      NA,        'A',     2,
#'   'SSP2EU',    'DEU',     'B',     3) %>%
#'   tool_expand_tibble(scenarios = c('SSP1', 'SSP2EU', 'SSP5'),
#'                      regions = c('CHA', 'DEU', 'USA'),
#'                      structure.columns = 'name')
#' }

#' @export
tool_expand_tibble <- function(d, scenarios, regions,
                               structure.columns = NULL) {
  . <- NULL

  # entries with both scenarios and regions defined
  d.scenario.region <- d %>%
    filter(!is.na(.data$scenario), .data$scenario %in% scenarios,
           !is.na(.data$region), .data$region %in% regions)

  # entries with only scenarios defined
  d.scenario <- d %>%
    filter(!is.na(.data$scenario), .data$scenario %in% scenarios,
           is.na(.data$region)) %>%
    complete(nesting(!!!syms(setdiff(colnames(.), 'region'))),
             region = regions) %>%
    filter(!is.na(.data$region))

  # entries with only regions defined
  d.region <- d %>%
    filter(is.na(.data$scenario),
           !is.na(.data$region), .data$region %in% regions) %>%
    complete(nesting(!!!syms(setdiff(colnames(.), 'scenario'))),
             scenario = scenarios) %>%
    filter(!is.na(.data$scenario))

  # entries with neither scenario nor regions defined
  d.global <- d %>%
    filter(is.na(.data$scenario), is.na(.data$region)) %>%
    complete(nesting(!!!syms(setdiff(colnames(.), c('scenario', 'region')))),
             scenario = scenarios,
             region = regions) %>%
    filter(!is.na(.data$scenario), !is.na(.data$region))

  # combine all entries
  d.global %>%
    # scenarios overwrite global data
    anti_join(
      d.scenario,

      c('scenario', 'region', structure.columns)
    ) %>%
    bind_rows(d.scenario) %>%
    # regions overwrite global and scenario data
    anti_join(
      d.region,

      c('scenario', 'region', structure.columns)
    ) %>%
    bind_rows(d.region) %>%
    # specific data overwrites everything
    anti_join(
      d.scenario.region,

      c('scenario', 'region', structure.columns)
    ) %>%
    bind_rows(d.scenario.region) %>%
    select(all_of(colnames(d))) %>%
    return()
}
