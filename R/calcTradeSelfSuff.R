#' Calculate food/material self sufficiencies
#' 
#' Calculates regional self sufficiences from FAO data as
#' production/domestic_supply.
#' 
#' 
#' @return Self sufficiences
#' @author Ulrich Kreidenweis
#' @seealso \code{\link{calcOutput}}, \code{\link{calcFAOmassbalance}}
#' @examples
#' 
#' \dontrun{ 
#' a <- calcTradeSelfSuff()
#' }
#' 
calcTradeSelfSuff <- function() {
  
  massbalance<-calcOutput("FAOmassbalance",aggregate = F)
  # add missing products
  newproducts<-c("betr","begr","scp")
  massbalance[,,newproducts]<-0
  k_trade<-findset("k_trade")
  massbalance<-massbalance[,,k_trade]
  
  self_suff <- massbalance[,,"production.dm"]/massbalance[,,"domestic_supply.dm"]
  self_suff <- collapseNames(self_suff)
  self_suff[is.nan(self_suff)] <- 0
  self_suff[self_suff == Inf] <- 1
  self_suff[,,newproducts]<-1
  
  weight <- massbalance[,,"domestic_supply.dm"]
  weight <- collapseNames(weight)
  weight[is.nan(weight)] <- 0
  weight[,,newproducts]<-1
  
  out <- toolHoldConstantBeyondEnd(self_suff)
  weight <- toolHoldConstantBeyondEnd(weight)
  #fading out the self sufficiency until 2050.
  #out<-convergence(origin = self_suff,aim = 1,start_year = "y2010",end_year = "y2050",type = "s")
    
  return(list(x=out,
              weight=weight,
              unit="ratio",
              description="countries' self sufficiencies in agricultural production. Production/Domestic supply")
  )
}



# compare these aggregated values to current ones

# test <- calcSelfSuffSeedred(years = 1995, avrg_years=3)
# aggregation <- read.csv(toolMappingFile("regional","regionmappingMAgPIE.csv"), sep=";") 
# self_suff <- toolAggregate(x = test$x, rel = aggregation, weight = test$weight, from="CountryCode", to="RegionCode")
# 
# 
# self_suff_old <- read.magpie("D:/MAGPIE/modules/21_trade/input/self_suff.csv")
# getYears(self_suff_old) <-  1995
# comitems <- intersect(getNames(self_suff), getNames(self_suff_old))
# ratio <- self_suff[,1995,comitems]/self_suff_old[,1995,comitems]
# ratio[ratio < 1.05 & ratio > 0.95] <- 1
# ratio

## high agreement for data where definition hasn't changed. For instance for sugar it changed, therefore different values are well explained.

