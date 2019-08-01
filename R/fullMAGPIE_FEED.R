#' fullMAgPIE_FEED
#' 
#' Function that produces the complete country data set required for the
#' MAgPIE-FEED model (preprocessing).
#' 
#' @param rev data revision which should be used as input (positive numeric).
#' \code{\link{setConfig}} (e.g. for setting the mainfolder if not already set
#' properly).
#' \code{\link{setConfig}} (e.g. for setting the mainfolder if not already set
#' properly).
#' @author Isabelle Weindl, Lavinia Baumstark
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}},\code{\link{setConfig}}
#' @examples
#' 
#' \dontrun{ 
#' fullMAgPIE_FEED()
#' }
#' 
fullMAgPIE_FEED <- function(rev=0) {

  mag_years <- findset("time")
  mag_years_past <- findset("past")
  short_years <- findset("t_all")
  

  #time-dependent data:
  calcOutput("LivstProduction",         years=mag_years_past,round=8, file="livst_production.csv",  aggregate=FALSE)
  calcOutput("LivstSubProduction",      years=mag_years_past,round=8, file="livst_sub_production.cs3",  aggregate=FALSE)
  
  calcOutput("LivestockProductivity",   years=mag_years_past,round=8, file="livestock_productivity.cs3", future=FALSE,  aggregate=FALSE)
  calcOutput("FeedRequirement",         years=mag_years_past,round=8, file="feed_req.cs3",              aggregate=FALSE)
  calcOutput("NutrientDensity",         years=mag_years_past,round=8, file="nutrient_dens.cs3",         aggregate=FALSE)
  
  calcOutput("FAOFeed",                 years=mag_years_past,round=8, file="FAO_feed.csv",          aggregate=FALSE)
  calcOutput("ProdFoddr",               years=mag_years_past,round=8, file="prod_foddr.csv",     aggregate=FALSE)
  calcOutput("FeeduseFoddr",            years=mag_years_past,round=8, file="feeduse_foddr.csv",  aggregate=FALSE)
  calcOutput("ResFeedAvailability",     years=mag_years_past,round=8, file="res_feed_avail.cs3",  aggregate=FALSE)
  calcOutput("HHFoodWaste",               years=mag_years_past,round=8, file="food_waste.cs3",            aggregate=FALSE)
 
  
  #data only available for one time step:
  calcOutput("FoodWasteRecycle",        years=2000,round=2, file="foodwaste_DM_recycle.cs4",  aggregate=FALSE)
  calcOutput("ProdSystRatio_2000",      years=2000,round=8, file="prod_system_ratio_2000.cs3",     aggregate=FALSE)
  
  
  #data that is not time-dependent:
  calcOutput("Attributes", round=8, file="product_attributes.cs3", aggregate = F)

}