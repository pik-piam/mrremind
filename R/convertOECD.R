#' Convert OECD GDP / ratioPM
#' 
#' Convert GDP / ratioPM data from OECD to data on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing GDP data mixed country-region resolution
#' @param subtype data subtype. Either "gdp" or "ratioPM"
#' @return GDP / ratioPM data as MAgPIE object aggregated to country level
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{
#' a <- readSource("OECD","gdp")
#' a <- readSource("OECD","ratioPM")
#' }
#' @importFrom magclass getCells<-
convertOECD <- function(x,subtype) {

  if(subtype=="gdp") {
    # disaggregate "ROW"
    l <- c("ATG","DMA","FSM","GRD","IMN","KNA","PLW","SYC") #"TLS"
    w <- calcOutput("Population",aggregate=FALSE)[l,2010,"pop_SSP2"]    # FIXME time and scenario dependent?
    x_ROW <- toolAggregate(x[c("ROW"),,],"OECD_ROW2ISO.csv",weight=w) 
    # delete ROW entry
    delete_ROW <- setdiff(getRegions(x),c("ROW"))
    x <- x[delete_ROW,,]
    # add data for ROW
    x <- mbind(x[,getYears(x_ROW),],x_ROW)
    # fill all the rest with 0
    x <- toolCountryFill(x,fill=0)
    
  } else if(subtype=="ratioPM") {
    # disaggregate "ROW"
    l <- c("ATG","DMA","FSM","GRD","IMN","KNA","PLW","SYC") #"TLS"
    l_ROW <- rep(c("ROW"),times=length(l))
    x_ROW <- x[l_ROW,,] 
    getCells(x_ROW) <- l
    # delete ROW entry
    delete_ROW <- setdiff(getRegions(x),c("ROW"))
    x <- x[delete_ROW,,]
    # add data for ROW
    x <- mbind(x[,getYears(x_ROW),],x_ROW)
    # fill all the rest with 1
    x <- toolCountryFill(x,fill=1)
  } else if(subtype=="riskClass") {
    # fill NA with 1
    x[is.na(x)] <- 0
    # fill all the rest with 1
    x <- toolCountryFill(x,fill=0)
  }
  
  return(x)
}  
