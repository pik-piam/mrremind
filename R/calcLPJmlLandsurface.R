#' @title calcLPJmlLandsurface
#' @description Calculates LPJmL land surface area 
#' 
#' 
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @seealso
#' \code{\link{readLPJml_rev21}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LPJmlLandsurface", aggregate=FALSE)
#' }

calcLPJmlLandsurface<-function(){
  
   
  cellarea   <- readSource("LPJml_rev21", subtype="cellarea",   convert="onlycorrect")
  oceanshare <- readSource("LPJml_rev21", subtype="oceanshare", convert="onlycorrect")
  lakeshare  <- readSource("LPJml_rev21", subtype="lakeshare",  convert="onlycorrect")
  
  out <- collapseNames(cellarea * (1-oceanshare) * (1-lakeshare))
  getNames(out) <- "land"
  
  return(list(
    x            = out,
    weight       = NULL,
    unit         = "million ha",
    description  = "LPJmL Landsurface area without lake and ocean fraction.",
    isocountries = FALSE))
}