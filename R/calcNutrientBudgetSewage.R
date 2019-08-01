#' @title calcNutrientBudgetSewage
#' @description Nutrient Budget for Wastewater treatment and sewage
#'
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
calcNutrientBudgetSewage<-function(){
  
  past<-findset("past") 
  
  drecht<-collapseNames(time_interpolate(readSource("VanDrecht2009")[,,"GO"],interpolated_year = past,integrate_interpolated_years = FALSE,extrapolation_type = "constant"))
  
  population<-calcOutput("PopulationPast",aggregate = FALSE)[,past,]
  detergent=population*drecht[,,"det_p_pp"]
  
  inflow<-dimSums(calcOutput("FoodWasteAndSewage",aggregate = FALSE)[,,c("urine","feces")],dim=c(3.1,3.3))
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
