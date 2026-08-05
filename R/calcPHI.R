#' Provides REMIND data for realization Investment_Inefficiencies in 01_macro Module
#'
#' @author Diamantis Koutsandreas
#'
#'
calcPHI <- function() {

  output <- readSource("IMF_PHI")
  getNames(output) <- NULL
  return(
    list(
      x = output,
      unit = "%",
      description = "share of investments in each region that are not transformed into productive capital",
      isocountries = FALSE
      )
  )
}
