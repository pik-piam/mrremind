#' @title calcNutrientWasteBudget
#' @description Estimate waste flows of nutrients from differents sources, including Household waste, slaughterwaste and Processingwaste
#'
#' @param nutrient The nutrient in which the results shall be reported.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("NutrientWasteBudget")
#' }
#' 
#' @importFrom magpiesets findset
#' @importFrom magpiesets reportingnames

calcNutrientWasteBudget<-function(nutrient="nr"){
  if (length(nutrient)!=1) {stop("nutrient should only contain one element")}
  massbalance<-calcOutput("FAOmassbalance",aggregate=F)
  ksd<-findset("ksd")
  kall<-findset("kall")
  kli<-findset("kli")  
  past<-findset("past")
  processing<-setdiff(c(findset("processing20"),"brans1","branoil1"),c("milling","ginning","breeding"))
  
  # Food Loss
  supply_chain_loss<-setNames(dimSums(massbalance[,,"waste"][,,kall][,,nutrient],dim=c(3)),"waste")
  
  # Processing Waste 1
  processing_loss<-dimSums(massbalance[,,processing][,,kall][,,nutrient],dim=c(3))-dimSums(massbalance[,,"production"][,,ksd][,,nutrient],dim=c(3))
  processing_loss<-setNames(processing_loss,"processing_loss")
  
  # Processing Waste 2
  food_processing_loss<-calcOutput("NutrientBudgetFoodProcessing",aggregate = FALSE)[,,nutrient]
  food_processing_loss<-dimSums(food_processing_loss[,,"food_processing_loss"],dim=c(3.2,3.3))
  
  # Slaughter Waste
  livestock<-collapseNames(massbalance[,,kli][,,"production"][,,nutrient],collapsedim = 2)
  slaughtermass<-calcOutput("Slaughtermass",aggregate=FALSE)[,,kli][,,nutrient]
  slaughterwaste<-setNames(dimSums(slaughtermass-livestock,dim=3),"slaughterwaste")
  
  # HH Waste
  hhwaste<-dimSums(calcOutput("FoodWasteAndSewage",aggregate = FALSE)[,,nutrient][,,"hh_food_waste"],dim=c(3))
  hhwaste<-setNames(hhwaste,"hh_food_waste")
  
  out<-mbind(supply_chain_loss,processing_loss,food_processing_loss,slaughterwaste,hhwaste)
  
  if(nutrient=="dm"){unit="Mt DM/yr"
  } else if (nutrient=="nr"){unit="Mt Nr/yr"
  } else if (nutrient=="p"){unit="Mt P/yr"
  } else if (nutrient=="k"){unit="Mt K/yr"
  } else if (nutrient=="ge"){unit="PJ/yr"
  } else if (nutrient=="wm"){unit="Mt WM/yr"}
  return(list(x=out,
              weight=NULL,
              unit=unit,
              description="Livestock Budget")
  )
}
