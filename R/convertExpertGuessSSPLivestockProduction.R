#' @title convertExpertGuessSSPLivestockProduction
#' @description {convert the Expert Guesses for future Livestock Production for the SSP Scenarios
#' }
#' 
#' 
#' @param x MAgPIE-Object contaiing data to convert
#' 
#'
#' 
#' @return magpie object containing converted expert guesses
#' @author Stephen Wirth
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{ 
#' a <- readSource("ExperGuessSSPLivestockProduction", "ssp1")

#' }


convertExpertGuessSSPLivestockProduction <- function(x){
  
  
   map <- toolMappingFile(type="regional", readcsv=T, name="regionmappingMAgPIE.csv")
   
   y<- toolAggregate(x,map, from=3, to=2)
   
  return(y)
}