## calculate the corrective balance flow that is needed because global production doesn't equal global supply
# becomes 0 in 2050
## f21_domestic_balanceflow















#' Calculate global under-/overproduction
#' 
#' Calculate the difference between the global production and the global
#' domestic_supply. The difference is the result of imports not equaling
#' exports, and because storage is not considered. The calculated
#' DomesticBalanceflow assures that production matches domestic_supply. The
#' goods come from nowhere and go to nowhere. The numbers are usually decreased
#' linearly and become zero in 2050.
#' 
#' 
#' @return global Domestic Balanceflows as MAgPIE object
#' @author Ulrich Kreidenweis, Xiaoxi Wang
#' @seealso \code{\link{calcOutput}}, \code{\link{calcFAOmassbalance}}
#' @examples
#' 
#' \dontrun{ 
#' a <- calcTradeBalanceflow()
#' }
#' 
calcTradeBalanceflow <- function() {
  
  massbalance<-calcOutput("FAOmassbalance",aggregate = F)
  k_trade<-findset("k_trade")
  massbalance<-massbalance[,,k_trade]
  
  x <- dimSums(massbalance[,,c("production.dm","domestic_supply.dm")],dim=1)
  balanceflow <- collapseNames(x[,,"production.dm"]) -collapseNames(x[,,"domestic_supply.dm"])
  
  out <- toolHoldConstantBeyondEnd(balanceflow)
  #fading out the balanceflow until 2020.
  out<-convergence(origin = out,aim = 0,start_year = "y2010",end_year = "y2020",type = "s")
  
  
  return(list(x=out,
              weight=NULL,
              unit="mio. ton dm",
              description="Balanceflow to match global production and domestic supply")
  )
}
