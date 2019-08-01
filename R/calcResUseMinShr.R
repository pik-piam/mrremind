#' Calculate UseMin
#' 
#' Provides MAgPIE-FEED data for UseMin. No changes to the content have been
#' done. Usually no weight needed as the data will beused in MAgPIE-FEED model
#' country based.
#' 
#' 
#' @return MAgPIE-FEED data for UseMin and corresonding weights as a list of
#' two MAgPIE objects
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ResUseMinShr")
#' 
#' }
#' 
calcResUseMinShr <- function() {
  x <- readSource("Bodirsky")
  w <- calcOutput("FAOCrop_aggr",aggregate=FALSE)
  w <- dimSums(w[,2000,"production"],dim=3)    # Isabelle to check w <- w[,2000,".production"]
  
  return(list(x=x,
              weight=w,
              unit="-",
              description="minimum use of residues for different purposes"
              ))
}
