#' @title readDams
#' @description Read dam file (no source information available)
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @examples
#'
#' \dontrun{
#'   readSource("Dams", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

readDams <- function(){

  x <- read.magpie("dams_0.5.mz")

  return(x)
}
