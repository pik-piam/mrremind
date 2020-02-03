#' @title convertMissingIslands
#' @description Read in core data for minor islands which are not included in big inventories but have a countrycode
#' 
#' @param subtype pop for population or gdp for gdp
#' 
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Benjamin Bodirsky, Eweron Araujo
#' @seealso
#' \code{\link{readLPJmlCarbon}},
#' @examples
#' 
#' \dontrun{ 
#' readSource("MissingIslands", subtype="pop",convert=FALSE)
#' }
#'


convertMissingIslands<-function(subtype){
  
stop("This dataset should not be converted.")
  
  return(NULL)
}  