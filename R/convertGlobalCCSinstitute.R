#' Convert Global CCS Institute Project Database
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned by [readGlobalCCSinstitute()].
#' @inherit readGlobalCCSinstitute
#' @return A [`magpie`][magclass::magclass] object.
convertGlobalCCSinstitute <- function(x, subtype = '08-09-2017') {
  if ('08-09-2017' == subtype) { # 08-09-2017 ----

    # sum over districts of a country
    y <- dimSums(x, dim = 3)

    # transfer into ISO country names
    getItems(y, dim = 1) <- toolCountry2isocode(getRegions(y))

    # fill all other countries with 0
    y <- toolCountryFill(y, fill = 0, verbosity = 2)

    return(y)
  }
  else if ('2023-11' == subtype) { # 2023-11 ----
    stop('Subtype "2023-11" does not support conversion.')
  }
  else {
    stop('Unsupported version argument.')
  }
}
