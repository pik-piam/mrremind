#' Calculate Res Feed use share
#' 
#' Provides MAgPIE-FEED data for Res Feed use share. No changes to the content
#' have been done. Usually no weight needed as the data will beused in
#' MAgPIE-FEED model country based.
#' 
#' 
#' @return MAgPIE-FEED data for Res Feed use share and corresonding weights as
#' a list of two MAgPIE objects
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ResFeeduseShr")
#' 
#' }
#' @importFrom magclass getNames
calcResFeeduseShr <- function() {
  x <- readSource("Krausmann")
  getNames(x) <- NULL
  w <- calcOutput("FAOCrop_aggr",aggregate=FALSE)
  w <- dimSums(w[,2000,"production"],dim=3)    # Isabelle to check w <- w[,2000,".production"]
  
  return(list(x=x,
              weight=w,
              unit="-",
              description="share of recovered crop residues used as feed"
              ))
}
