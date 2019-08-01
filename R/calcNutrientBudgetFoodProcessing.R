#' @title calcNutrientBudgetFoodProcessing
#' @description Fooduse before processing, after processing and food processing loss
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcFAOmassbalance}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("NutrientBudgetFoodProcessing")
#' }
#' 
calcNutrientBudgetFoodProcessing<-function(){
  massbalance=calcOutput("FAOmassbalance",aggregate = FALSE)[,,c("wm","ge","nr")]
  demand<-massbalance[,,"households"]
  unprocessed <- dimSums(massbalance[,,c("food","milling")],dim=c(3.2)) 
  food<-dimSums(demand,dim=3.2)
  food_processing_loss = unprocessed-food
  
  out<-mbind(
    add_dimension(unprocessed,dim = 3.1,nm = "fooduse"),
    add_dimension(food,dim = 3.1,nm = "food_availability"),
    add_dimension(food_processing_loss,dim = 3.1,nm = "food_processing_loss")
  )
  
  return(list(x=out,
              weight=NULL,
              unit="Mt nr, wm, ge",
              description="Fooduse before processing, after processing and food processing loss")
  )    
}