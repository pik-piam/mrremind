#' Calculate FAO Livestock Primary
#' 
#' Provides the FAO Livestock Primary data. No changes to the content have been
#' done, besides changes to the countries list. Yield information has been
#' removed.
#' 
#' 
#' @return FAO Livestock Primary data and corresonding weights as a list of two
#' MAgPIE objects
#' @author Ulrich Kreidenweis
#' @seealso \code{\link{calcOutput}}, \code{\link{readFAO}},
#' \code{\link{convertFAO}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LivePrim")
#' 
#' }
#' 
calcFAOLivePrim <- function() {
  x <- readSource("FAO","LivePrim")
  
  if (any( grepl("+ (Total)",getNames(x, fulldim=T)[[1]], fixed = TRUE))) {
    x <- x[,,"+ (Total)", pmatch=T, invert=T]
  }
  
  
  ## Yields currently removed, because it is unclear with which absolute value would have to be used as weight
  if (any( grepl("Yield",getNames(x, fulldim=T)[[2]], fixed = TRUE))) {
    x <- x[,,"Yield", pmatch=T, invert=T]
  }
  
  
  return(list(x=x,weight=NULL, unit="tonnes if not stated otherwise in dimension name", description="FAO Livestock Primary data"))
}

## Yields:

#probably the following weight
# "Yield_(100Mg/An)": "Laying_(Head)"
