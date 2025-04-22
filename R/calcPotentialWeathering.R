#' Calculate hydro potential
#'
#' Provides weathering potential data
#'
#'
#' @return weathering potential data and corresonding weights as a list of
#' two MAgPIE objects
#' @author Lavinia Baumstark
#'
#' @examples
#' \dontrun{
#' calcOutput("PotentialWeathering")
#' }
#'
calcPotentialWeathering <- function() {
  # read weathering data
  pot <- readSource("Strefler", subtype = "weathering_graderegi")

  # change the unit:  10^6 km^2 -> kg/m^2 =  Gt / 10^6 km^2
  pot <- pot * 15

  # delete total
  pot <- pot[, , "total", invert = TRUE]
  # allocate "warm" and "temperate" to the right grades
  getNames(pot) <- gsub("warm", "1", getNames(pot))
  getNames(pot) <- gsub("temperate", "2", getNames(pot))

  return(list(
    x = pot,
    weight = NULL,
    unit = "Gt of ground stone/a",
    description = "This file includes max. potential for enhanced weathering in Gt of ground stone/a for different regions and warm (1) and temperate (2) grades. (=maxprod)"
  ))
}
