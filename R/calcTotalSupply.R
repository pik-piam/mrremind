#' Calculate total supply aggregated over all commodities and assigned to
#' categories food, feed, other util, seed and waste
#' 
#' Provides aggregated total supply differentiated according to food, feed,
#' other util, seed and waste from the FAOSTAT Food Balance Sheets (FBS).
#' 
#' 
#' @return FAO total supply and corresponding weights as a list of two MAgPIE
#' objects
#' @author Isabelle Weindl
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ 
#' a <- calcTotalSupply()
#' }
#' 
calcTotalSupply <- function() {
  
  massbalance <- calcOutput("FAOmassbalance_pre",aggregate=FALSE)
  
  # select "dm" (mio. ton)
  supply <- c("food","feed","other_util","seed","waste")
  total.supply <- massbalance[,,supply]
  
  total.supply <- dimSums(collapseNames(total.supply[,,"dm"]),dim = 3.1)
  
  total.supply[is.nan(total.supply)] <- 0
  total.supply[total.supply < 0] <- 0
  
  return(list(x=total.supply,
              weight=NULL,
              unit="mio ton DM",
              description="FAO FBS supply")
  )
}

