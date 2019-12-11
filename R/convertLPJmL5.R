#' @title convertLPJmL5
#' @description Convert LPJmL 5 content
#' @param subtype Switch between diffrent input
#' @param x magpie object provided by the read function
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @examples
#'
#' \dontrun{
#' readSource("LPJmL5", subtype="soilc", convert=TRUE)
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom lpjclass readLPJ

convertLPJmL5<-function(x,subtype){
  stop("Convert functions for LPJml are not written yet due to missing aggregation weights. Set convert=FALSE")
  return(x)
}
