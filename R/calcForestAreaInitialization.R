#' @title calcForestAreaInitialization
#' @description 
#' Calculates the management factor(s) needed to upscale the yield of forest plantations as compared to natural vegetation based on FAO data.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link{calcFAOmassbalance_pre}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ForestAreaInitialization")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @export

calcForestAreaInitialization <- function(){
  
  ## Reading in the area data from FAO
  forestry_area <- readSource("FRA2015Doc","plantation_forest")[,"y1995",]
  
  ## Reading NatVeg area from FAO
  natveg_area <- setNames(dimSums(mbind(readSource("FRA2015Doc","secondary_forest")[,"y1995",],readSource("FRA2015Doc","primary_forest")[,"y1995",])),"natveg_forest")
  
  zero_check <- where(forestry_area==0 & natveg_area==0)$true$regions
  if(length(zero_check)!=0){
    cat("Countries without forest area detected. Will be manually corrected ---")
    zero_area <- where(forestry_area==0 & natveg_area==0)$true$regions
    forestry_area[zero_area,,] <- median(forestry_area[forestry_area!=0])
    natveg_area[zero_area,,] <- median(natveg_area[natveg_area!=0])*0.25
  }

  zero_check <- where(forestry_area==0 & natveg_area==0)$true$regions
  if(length(zero_check==0)){
    cat("Countries without forest area filled with median values.")
  }
  
  out <- setYears(mbind(setNames(forestry_area,"forestry"),setNames(natveg_area,"natveg")),NULL)
  
#  weight <- setNames(out[,,1],NULL)
#  weight[weight>=0] = 1
  
  return(list(x=out,
              weight=NULL,
              min=0,
              unit="factor",
              description="Calculates forestry area for preparing plantation management factors in MAgPIE."))
  
}
