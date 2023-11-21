#' Calculate REMIND variables from Global Energy Monitor
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#' @importFrom magclass add_dimension
#' @export

calcGlobalEnergyMonitor <- function() {

  x <- readSource("GlobalEnergyMonitor")

  return(list(
    x = x,
    weight = NULL,
    unit = c("GW"),
    description = "Global Energy Monitor Capacities"
  ))

}
