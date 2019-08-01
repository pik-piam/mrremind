#' Aggregation and calculation of the mean of each MAgPIE region for the source
#' SathayeForest
#' 
#' This function aggregates the data from source SathayeForest. A weight is
#' implemented as the mean for each MAgPIE region is calculated.
#' 
#' 
#' @return MAgPIE object of the calculated means of each MAgPIE region
#' @author Nele Steinmetz
#' @seealso \code{\link{calcOutput}}, \code{\link{readSathayeForest}},
#' \code{\link{convertSathayeForest}}
#' @examples
#' 
#' \dontrun{ 
#' 
#' a <- calcOutput("AfforestCosts")
#' 
#' }
#' @importFrom magclass getNames<- setYears



calcAfforestCosts <- function() {
  x    <- readSource("SathayeForest")
  
  rename <- c("RecurrentCost"="recur",  "MonitoringCost"="mon")
  x <- x[,,names(rename)]
  getNames(x) <- rename

  l <- setYears(readSource("FAO","Land")[,2001,"6601|Land area.area",drop=TRUE],NULL)  
  l[is.na(l)|is.nan(l)] <- 0
  
  return(list(x=x,weight=l,unit="US$2004/ha",description="Afforestation factor requirement costs"))
}
