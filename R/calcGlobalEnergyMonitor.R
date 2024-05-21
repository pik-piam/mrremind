#' Calculate REMIND variables from Global Energy Monitor
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

  return(list(
    x = x,
    weight = NULL,
    unit = c("GW"),
    description = "Global Energy Monitor Capacities"
  ))

}
