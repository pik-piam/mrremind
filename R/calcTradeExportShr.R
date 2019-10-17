#' Calculate export shares
#' 
#' Provides export shares of countries compared to total export. This is based
#' on export values from FAOSTAT. Function calculates this based on average
#' values of the specified years.
#' 
#' 
#' @return Export shares
#' @author Ulrich Kreidenweis, Xiaoxi Wang
#' @seealso \code{\link{calcOutput}}, \code{\link{calcFAOmassbalance}}
#' @examples
#' 
#' \dontrun{ 
#' a <- calcTradeExportShr()
#' }
#' 
calcTradeExportShr <- function() {
  
  massbalance<-calcOutput("FAOmassbalance",aggregate = F)
  newproducts<-c("betr","begr","scp","wood","woodfuel")
    
  k_trade<-findset("k_trade")
  massbalance<-massbalance[,,k_trade]
  
  # export share is the regions' share in total exports. So the sum per commodity over all regions is 1
  
  netexp<-massbalance[,,"export"][,,"dm"]-massbalance[,,"import"][,,"dm"]
  netexp[netexp<0]<-0
  netexp[,,c("betr","begr","scp")]<- 1
  
  exp_glo <- dimSums(netexp, dim=1)
  exp_shr <- netexp/exp_glo
  exp_shr <- collapseNames(exp_shr)
  exp_shr[is.nan(exp_shr)] <- 0


  exp_shr <- toolHoldConstantBeyondEnd(exp_shr)
  
  return(list(x=exp_shr,
              weight=NULL,
              unit="share",
              description="share of export of individual counties in total global exports")
  )
}

