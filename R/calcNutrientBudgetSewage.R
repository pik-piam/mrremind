#' @title calcNutrientBudgetSewage
#' @description Nutrient Budget for Wastewater treatment and sewage
#' @param historic when TRUE only for the historic period, otherwise including future scenarios
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcNitrogenBudgetCropland}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("NutrientBudgetSewage")
#' }
#' 

#recycling of food waste and sewage?
calcNutrientBudgetSewage<-function(historic=TRUE){
  
  past<-findset("past") 
  
  drecht=readSource("VanDrecht2009")
  
  if(historic){
    drecht<-collapseNames(time_interpolate(drecht[,,"GO"],interpolated_year = past,integrate_interpolated_years = FALSE,extrapolation_type = "constant"))
    population<-calcOutput("PopulationPast",aggregate = FALSE)[,past,]
  } else {
    ssps=c("SSP1","SSP2","SSP3","SSP4","SSP5")
    drecht<-add_columns(x = drecht,dim = 3.1,addnm = ssps)
    drecht[,,"SSP1"]<-drecht[,,"TG"]
    drecht[,,"SSP2"]<-drecht[,,"GO"]
    drecht[,,"SSP3"]<-drecht[,,"OS"]
    drecht[,,"SSP4"]<-drecht[,,"AM"]
    drecht[,,"SSP5"]<-drecht[,,"GO"]
    drecht<-drecht[,,ssps]
    drecht<-collapseNames(time_interpolate(drecht,interpolated_year = findset("time"),integrate_interpolated_years = FALSE,extrapolation_type = "constant"))
    population<-collapseNames(calcOutput("Population",naming="indicator.scenario",PopulationFuture="SSP",aggregate = FALSE))
  }
  
  detergent=population*drecht[,,"det_p_pp"]
  
  inflow<-dimSums(calcOutput("FoodWasteAndSewage",historic=historic,aggregate = FALSE)[,,c("urine","feces")],dim=c(3.1))
  inflow[,,"p"]<-inflow[,,"p"]+detergent
  
  sewage=removed=surfacewater=collapseNames(inflow*drecht[,,"sewage_shr"])
  untreated=collapseNames(inflow*(1-drecht[,,"sewage_shr"]))
  removed[,,"nr"]=sewage[,,"nr"]*drecht[,,"sewage_n_removal_shr"]
  removed[,,"p"]=sewage[,,"p"]*drecht[,,"sewage_p_removal_shr"]
  surfacewater[,,"nr"]=sewage[,,"nr"]*(1-drecht[,,"sewage_n_removal_shr"])
  surfacewater[,,"p"]=sewage[,,"p"]*(1-drecht[,,"sewage_p_removal_shr"])
  
  out<-mbind(
    add_dimension(untreated,dim = 3.2,nm="untreated"),
    add_dimension(removed,dim = 3.2,nm="removed"),
    add_dimension(surfacewater,dim = 3.2,nm="freshwater")
  )
  
  # add recycling as feed, fertilizer
  
  return(list(x=out,
              weight=NULL,
              unit="Mt Nr, P",
              description="Further destiny of food")
  )    
}
