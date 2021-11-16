#' Convert IEA_EV
#' 
#' Convert IEA_EV on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing IEA_EV country resolution
#' @return Krausmann data as MAgPIE object aggregated/disaggregated to country
#' level
#' @author Lavinia Baumstark, Christoph Bertram
#' @seealso \code{\link{readIEA_EV}}
#' @examples
#' 
#' \dontrun{ a <- convertIEA_EV(x)
#' }


convertIEA_EV <- function(x) {
      # remove global data
      x <- x["Total",,,invert=TRUE]
	  # make extra object for other countries
	  y <- x["Others",,]
      # delete other countries
      x <- x["Others",,,invert=TRUE]
      # rename countries into ISO
      getRegions(x) <- toolCountry2isocode(getRegions(x))
	  # distribute rest on remaining countries with GDP as weight
      GDP2015 <- calcOutput("GDPPast", aggregate = F)[,"y2015",]
	  getSets(GDP2015)[1]<-"region"
	  # additional countries
	  oth <- setdiff(getRegions(GDP2015),getRegions(x))
	  #region mapping from Other to all remaining countries
	  map <- data.frame(reg=rep("Others",length(oth)),region=oth)
	  #downscale data of others to remaining countries, using GDP as weight
	  y <- toolAggregate(y, map,
                      from = "reg", to = "region",dim=1,
                      weight = GDP2015[oth,,])
      #combine data
      x <- mbind(x,y)	  
      # fill missing countries
      x <- toolCountryFill(x,fill = 0)
      # set all NA to 0
      x[is.na(x)] <- 0
      
      return(x)
}  
