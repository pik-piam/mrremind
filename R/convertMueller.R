#' Convert Mueller data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readMueller()`].
#'
#' @return A [`magpie`][magclass::magclass] object.
#' @param subtype One of:
#'   - `countries`: read table mapping country names use by Müller et al. 2013
#'     to ISO 3166-1 alpha-3 codes.
#'   - `stocks`: read low/medium/high estimates of per-capita steel stocks from
#'     Müller et al. 2013 SI2
#'
#' @author Falk Benke
#'
#' @importFrom dplyr mutate select
#' @importFrom quitte madrat_mule
#'
#'
#' @export
convertMueller <- function(x, subtype) {
  if (subtype == "stocks") {
    x %>%
      madrat_mule() %>%
      mutate(!!sym("variable") := paste0("Steel stock per-capita|", !!sym("estimate"), " (t)")) %>%
      select("region" = "iso3c", "period" = "year", "variable", "value" = "steel.stock.per.capita") %>%
      as.magpie() %>%
      toolCountryFill(fill = 0, verbosity = 2, no_remove_warning = c("ANT")) %>%
      return()
  } else {
    return(x)
  }
}
