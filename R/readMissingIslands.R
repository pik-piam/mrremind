#' @title readMissingIslands
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


readMissingIslands<-function(subtype){
  
  files <- c(pop = "pop_past_missing.csv",
             gdp = "gdp_past_missing.csv")
  
  file <- toolSubtypeSelect(subtype = subtype,files = files)
  
  x=read.csv(file,header = T)
  names(x)<-substring(names(x),1,5)
  x=as.magpie(x)
  
  return(x)
}  