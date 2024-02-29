#' Convert Global CCS Institute Project Database
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned by
#'     [readGlobalCCSinstitute()].
#' @inherit readGlobalCCSinstitute
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @importFrom dplyr %>%
#' @importFrom madrat toolCountry2isocode toolCountryFill
#' @importFrom magclass dimSums getRegions getItems<-
#' @importFrom quitte add_countrycode_ madrat_mule
#'
#' @export
convertGlobalCCSinstitute <- function(x, version = '08-09-2017') {
  if ('08-09-2017' == version) { # 08-09-2017 ----

    # sum over districts of a country
    y <- dimSums(x, dim = 3)

    # transfer into ISO country names
    getItems(y, dim = 1) <- toolCountry2isocode(getRegions(y))

    # fill all other countries with 0
    y <- toolCountryFill(y, fill = 0, verbosity = 2)

    return(y)
  }
  else if ('2023-11' == version) { # 2023-11 ----
    x %>%
      madrat_mule() %>%
      add_countrycode_(origin = c('Country' = 'country.name'),
                          destination = 'iso3c') %>%
         select(-'Lifecycle stage', 'Facility', 'iso3c', 'Operational date',
                'Facility Industry',
                'Capture, transport and/or storage capacity (Mtpa CO2)',
                'Facility storage code') %>%
      madrat_mule() %>%
      return()
  }
  else {
    stop('Unsupported version argument.')
  }
}
