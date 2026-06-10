#' Calculate capital costs for energy technologies
#'
#' Provides REMIND data for realization Investment_Inefficiencies in 01_macro Module
#'
#' @author Diamantis Koutsandreas
#'
#' @examples
#' \dontrun{
#' calcOutput("IMF_PHI")
#' }
#'

calcPHI <- function() {

  output <- readSource("IMF_PHI")

  # ------------------------------------------------------------
  # Manual overrides for specific regions
  # ------------------------------------------------------------
  return(
    list(
      x = output,
      unit = "%",
      description = "share of investments in each region that are not transformed into productive capital",
      isocountries = FALSE
      )
  )
}
