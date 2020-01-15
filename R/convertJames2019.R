#' Convert James data
#' 
#' Convert James data on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing James data region resolution
#' @param subtype subtype of GDP indicator to be selected
#' @return GDP per capita in USD05 in PPP or MER as magpie object
#' @author David CHen Benjamin Bodirsky
#' @seealso \code{\link{readJames}}
#' @examples
#' 
#' \dontrun{ a <- convertJames2019(x,"IHME_USD05_PPP_pc")
#' }
#' @importFrom countrycode countrycode

convertJames2019 <- function(x,subtype) {
  x<-x[c("USSR_FRMR","CHN_354","CHN_361"),,,invert=TRUE] #Macao and HKG and Former USSR have 0 values in the dataset
  y<-toolCountryFill(x[,,subtype],fill = 0) 
  return(y)
}  
