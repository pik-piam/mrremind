#' Read ODYM_RECC data from the SHAPE Project
#'
#' @md
#' @param subtype One of
#'   - `'REMIND_industry_trends'`: Trends in per-capita production of industry
#'     subsectors `cement`, `chemicals`, `steel_primary`, `steel_secondary`,
#'     and `otherInd`.  Trends for `chemicals` and `otherInd` are averages of
#'     the other three trends, which are provided by NTNU.
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Michaja Pehl
#'
#' @importFrom magrittr %>%
#' @importFrom quitte madrat_mule
#' @importFrom readr read_csv

#' @export
#' @rdname ODYM_RECC
readODYM_RECC <- function(subtype) {
  # file path (for easier debugging)
  # path <- './'
  path <- '~/PIK/swap/inputdata/sources/ODYM_RECC/'

  # subtype switchboard ----
  switchboard <- list(
    'REMIND_industry_trends' = function() {
      read_csv(file = file.path(path, 'REMIND_industry_trends.csv'),
               col_names = c('scenario', 'iso3c', 'name', 'year', 'value'),
               col_types = '--ccc-id',
               skip = 1) %>%
        madrat_mule()
    })


  # check if the subtype called is available ----
  if (!subtype %in% names(switchboard)) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               paste(names(switchboard), collapse = ', ')))
  }

  # load data and do whatever ----
  return(switchboard[[subtype]]())
}

#' @export
#' @rdname ODYM_RECC
calcODYM_RECC <- function(subtype) {
  # subtype switchboard ----
  switchboard <- list(
    'REMIND_industry_trends' = function() {
      . <- NULL

      match_subsector <- tribble(
        ~name,                                              ~subsector,
        'Production|cement|Primary|per capita',             'cement',
        'Production|iron and steel|Primary|per capita',     'steel_primary',
        'Production|iron and steel|Secondary|per capita',   'steel_secondary')

      return(
        list(x = readSource(type = 'ODYM_RECC', subtype, convert = FALSE) %>%
               madrat_mule() %>%
               left_join(match_subsector, 'name') %>%
               select(-'name') %>%
               group_by(.data$scenario, .data$iso3c, .data$subsector) %>%
               mutate(value = .data$value / first(.data$value,
                                                  order_by = .data$year)) %>%
               ungroup() %>%
               pivot_wider(names_from = 'subsector') %>%
               group_by(.data$scenario, .data$iso3c, .data$year) %>%
               mutate(chemicals = mean(c(!!!syms(match_subsector$subsector))),
                      otherInd = .data$chemicals) %>%
               ungroup() %>%
               pivot_longer(-c('scenario', 'iso3c', 'year'),
                            names_to = 'subsector') %>%
               as.magpie(spatial = 2, temporal = 3, data = ncol(.)),
             weight = NULL,
             unit = '',
             description = ''))
    })

  # check if the subtype called is available ----
  if (!subtype %in% names(switchboard)) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               paste(names(switchboard), collapse = ', ')))
  }

  # load data and do whatever ----
  return(switchboard[[subtype]]())
}
