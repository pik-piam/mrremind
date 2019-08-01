#' Calculate Feed Requirements
#' 
#' Provides MAgPIE-FEED data for Nutrient Density calculated in the regression
#' for feed (calcRegressionFEED). No changes to the content have been done.
#' Usually no weight needed as the data will be used in MAgPIE-FEED model which
#' is country based.
#' 
#' 
#' @return MAgPIE-FEED data for NutrientDensity and corresonding weights as a
#' list of two MAgPIE objects
#' @author Lavinia Baumstark, Isabelle Weindl
#' @seealso \code{\link{calcOutput}}, \code{\link{calcRegressionFEED}},
#' \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("NutrientDensity")
#' }
#' @importFrom luscale rename_dimnames
#' @importFrom magclass as.magpie
calcNutrientDensity <- function() {
  
  past<-findset("past")
  
  nDens <- calcOutput("RegressionFEED",aggregate=FALSE)[,,"nutrient",pmatch=TRUE]
   
  output <- unwrap(nDens[,past,])
  mapping<-data.frame(
    old=c("nutrient_beef_gr","nutrient_beef_rep","nutrient_chicken","nutrient_dairy_gr","nutrient_dairy_rep","nutrient_hen","nutrient_pig"),
    new=c("beef_growing","beef_reproducer","chicken","dairy_growing","dairy_reproducer","hen","pig_all")
  )
  output<-rename_dimnames(output,dim = 3.1,query = mapping,from = "old", to="new")
  output <- as.magpie(output)
  
  # load weight
  # todo: use production as weight
      
  return(list(x=output,
              weight=NULL,
              unit="GJ per ton DM",
              description="nutrient density requirements for livestock subsystems"
              ))
}
