#' @title calcSeedRate
#' @description 
#' Calculates seed demand as a share of area harvested.
#' 
#' @return List of magpie objects with results on country level, weight on country level, unit, Max.&Min. values along with description.
#' @author Benjamin L. Bodirsky, Abhijeet Mishra
#' @seealso
#' \code{\link{calcFAOmassbalance}},
#' \code{\link{calcCroparea}}
#'  
#' @examples
#' 
#' \dontrun{ 
#' x <- calcOutput("SeedRate")
#' }
#' @importFrom magclass getNames<- as.magpie
#' 
calcSeedRate<-function(){
  # Calcculate massbalance
  massbalance <- calcOutput("FAOmassbalance",aggregate=F)
  kcr         <- findset("kcr")
  area        <- calcOutput("Croparea", sectoral = "kcr", physical=FALSE, aggregate=FALSE)
  seedrate    <- collapseNames(massbalance[,,"seed"][,,"dm"][,,kcr])/area[,getYears(massbalance),][,,kcr]
  weight      <- area[,getYears(massbalance),][,,kcr]
  
  #Seed rates
  #Miscanthus: 0.5t/ha (http://www.seai.ie/Renewables/Bioenergy/Miscanthus_Best_Practice_Guide_2010.pdf)
  seedrate[,,c("begr")]<-0.5/8 #Assuming 8 years life with 0.5t/ha required for planting
  seedrate[,,c("betr")]<-0
  weight[,,c("begr","betr")]<-1
  seedrate[is.nan(seedrate)]<-0
  weight[seedrate==Inf]<-0
  seedrate[seedrate==Inf]<-0
  
  # x <- seedrate1
  seedrate <- toolHoldConstantBeyondEnd(seedrate)
  weight <- toolHoldConstantBeyondEnd(weight)
  
  return(list(x=seedrate,weight=weight,unit="DM per ha, weight: area harvested",description="Proportion of harvested area used for seed"))
}