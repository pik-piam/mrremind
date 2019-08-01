#' Convert FRA2015Doc data
#' 
#' 
#' @param x MAgPIE object containing original values coming from read function
#' @param subtype The data table type, e.g.: forest_area
#' @return Data as MAgPIE object 
#' @author Abhijeet Mishra
#' @seealso \code{\link{readFRA2015Doc}}, \code{\link{readSource}},
#' @examples
#' 
#' \dontrun{ a <- readSource("FRA2015Doc","forest_area", convert=TRUE)}
#' @importFrom magclass magpiesort
#' @importFrom madrat toolCountryFill
#' 


convertFRA2015Doc <- function(x,subtype) { 
 
 if (!is.null(x)) {
    x[is.na(x)] <- 0
    x <- toolCountryFill(x, fill=0, verbosity = 2)
  } 
 else {return(x)}
}


