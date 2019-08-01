#' Read IIASApop
#' 
#' Read-in an population data csv file as magclass object
#' 
#' 
#' @return magpie object of the population data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="IIASApop")
#' }
#' 
readIIASApop<- function() {
  data <- read.csv("Data A1 Country total population-SSPs.csv")
  data <- data[,-ncol(data)]
  data[,c("cc")] <- toolCountryCode2isocode(data[,c("cc")])
  data <- data[!is.na(data[,c("cc")]),]
  x <- as.magpie(data,datacol=3)
  getNames(x) <- paste("pop_",gsub("ssp","SSP",getNames(x)),sep="")
  return(x)
}  
