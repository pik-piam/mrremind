#' Calculate FAO feed use
#' 
#' Provides primary and secondary agricultural products as well as conversion
#' byproducts used as feed. This is based on feed use values from the Food
#' Balance Sheets (FBS) and Commodity Balance Sheets from FAOSTAT. Function is
#' based on average values of the specified years.
#' 
#' @return FAO feed use and corresponding weights as a list of two MAgPIE
#' objects
#' @author Isabelle Weindl
#' @seealso \code{\link{calcOutput}}, \code{\link{calcFAOmassbalance}}
#' @examples
#' 
#' \dontrun{ 
#' a <- calcFAOFeed()
#' }
#' 
calcFAOFeed <- function() {
  
  massbalance <- calcOutput("FAOmassbalance_pre", aggregate=FALSE)
  
  #select "dm" (mio. ton)
  feed <- massbalance[,,"feed.dm"]
  
  feed[is.nan(feed)] <- 0
  feed[feed < 0] <- 0
  feed <- collapseNames(feed)
  
  # FAOmassbalance is already aggregated to MAgPIE items
  feed.items <- c("tece", "maiz", "trce", "rice_pro", "soybean", "rapeseed", "groundnut", "sunflower", "oilpalm", "puls_pro", "potato", "cassav_sp", "sugr_cane", "sugr_beet", "others", "cottn_pro", "foddr",
                  "livst_rum","livst_pig", "livst_chick", "livst_egg", "livst_milk",
                  "oilcakes","molasses","distillers_grain","brans")
  feed <- feed[,,feed.items,pmatch=F]
  
  return(list(x=feed,
              weight=NULL,
              unit="mio ton DM",
              description="FAO FBS and CBS feed use")
  )
}
