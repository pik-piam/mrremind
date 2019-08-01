#' Calculate FAO Fodder Crop Production aggregated to MAgPIE_FEED sectors
#' 
#' Provides the FAOSTAT Fodder production data (in mio ton) aggregated to three
#' MAgPIE_FEED fodder categories.
#' 
#' 
#' @return FAO fodder production data and corresponding weights as a list of
#' two MAgPIE objects
#' @author Isabelle Weindl
#' @seealso \code{\link{calcOutput}}, \code{\link{calcFAOFodder_aggrFEED}},
#' \code{\link{readFAO}}, \code{\link{convertFAO}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ProdFoddr")
#' 
#' }
#' 
calcProdFoddr <- function() {
  
  # load data and convert to mio. tones WM
  prod <- calcOutput("FAOFodder_aggrFEED",aggregate=FALSE)[,,"production",pmatch=TRUE]/1000000
  output <- collapseNames(prod)
  
  return(list(x=output,
              weight=NULL,
              unit="mio ton WM",
              description="FAO fodder production aggregated to MAgPIE_FEED categories")
  )
}





