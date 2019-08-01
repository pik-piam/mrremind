#' Convert IEA_EV
#' 
#' Convert IEA_EV on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing IEA_EV country resolution
#' @return Krausmann data as MAgPIE object aggregated/disaggregated to country
#' level
#' @author Lavinia Baumstark
#' @seealso \code{\link{readIEA_EV}}
#' @examples
#' 
#' \dontrun{ a <- convertIEA_EV(x)
#' }


convertIEA_EV <- function(x) {
      # remove global data
      x <- x["Total",,,invert=TRUE]
      # delete other countries
      x <- x["Others",,,invert=TRUE]
      # rename countries into ISO
      getRegions(x) <- toolCountry2isocode(getRegions(x))
      # fill missing countries
      x <- toolCountryFill(x,fill = 0)
      # set all NA to 0
      x[is.na(x)] <- 0
      
      return(x)
}  
