#' Calc capacities from Global Energy Monitor
#'
#' Calculate near-term expectations of capacities for use in fullVALIDATION.R
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#' @export

calcGlobalEnergyMonitor <- function() {

  x <- readSource("GlobalEnergyMonitor")

  # set 0s in other CHA countries than China to approximate CHA as China
  x[c("HKG", "MAC", "TWN"), , ] <- 0

  # sum over all statuses
  x <- dimSums(x, dim = "status")

  return(list(
    x = x,
    weight = NULL,
    unit = c("GW"),
    description = "Global Energy Monitor Capacities"
  ))

}
