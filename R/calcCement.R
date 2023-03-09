#' Calculate Historic Cement Production
#'
#' Combines cement production data from [`readvanRuijven2016()`] and
#' [`readUSGS(cement)`][readUSGS] into a single data set, using USGS data from
#' 2005 on.
#'
#' @md
#' @return A list with a [`magpie`][magclass::magclass] object `x` with
#'   country-level cement production in tonnes, `weight`, `unit`, `description`,
#'   and `min` fields.
#'
#' @author Michaja Pehl
#'
#' @seealso [calcOutput]
#'
#' @importFrom dplyr anti_join arrange bind_rows filter group_by select ungroup
#' @importFrom magclass as.magpie
#' @importFrom magrittr %>%
#' @importFrom rlang .data syms !!!

#' @export
calcCement <- function() {
  transition_year <- 2005
  . <- NULL

  d_vanRuijvan2016 <- readSource('vanRuijven2016', convert = FALSE) %>% quitte::madrat_mule()

  d_USGS_cement <- readSource('USGS', 'cement', convert = FALSE) %>%
    quitte::madrat_mule() %>%
    group_by(!!!syms(c('iso3c', 'year'))) %>%
    filter(max(.data$reporting.year) == .data$reporting.year) %>%
    ungroup() %>%
    select(-'reporting.year')

  d <- d_USGS_cement %>% filter(transition_year <= .data$year)

  d <- bind_rows(
    d,

    d_vanRuijvan2016 %>%
      anti_join(d, c('iso3c', 'year'))
  ) %>%
    group_by(!!!syms(c('iso3c', 'year'))) %>%
    mutate(count = n()) %>%
    assertr::verify(1 == .data$count, description = 'only one data point per country/year') %>%
    select(-'count') %>%
    arrange(!!!syms(c('iso3c', 'year')))

  return(list(x = d %>%
                as.magpie(spatial = 1, temporal = 2, data = ncol(.)) %>%
                toolCountryFill(verbosity = 2),
              weight = NULL,
              description = 'historical cement production',
              unit = 'tonnes of cement',
              min = 0))
}
