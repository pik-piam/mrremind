#' Calculate FAO Fodder used as feed aggregated to MAgPIE_FEED sectors
#' 
#' Provides the FAOSTAT Fodder feed use data (in mio ton) aggregated to three
#' MAgPIE_FEED fodder categories.
#' 
#' 
#' @return FAO fodder feed use data and corresponding weights as a list of two
#' MAgPIE objects
#' @author Isabelle Weindl
#' @seealso \code{\link{calcProdFoddr}}, \code{\link{calcOutput}},
#' \code{\link{calcFAOFodder_aggrFEED}}, \code{\link{readFAO}},
#' \code{\link{convertFAO}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FeeduseFoddr")
#' 
#' }
#' 
calcFeeduseFoddr <- function() {
  
  # load data and convert to mio. tones WM
  feed <- calcOutput("FAOFodder_aggrFEED",aggregate=FALSE)[,,"feed",pmatch=TRUE]/1000000
  output <- collapseNames(feed)
  
  return(list(x=output,
              weight=NULL,
              unit="mio ton WM",
              description="FAO fodder used as feed aggregated to MAgPIE_FEED categories")
  )
}
