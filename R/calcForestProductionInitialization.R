#' @title calcForestProductionInitialization
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
#' calcOutput("ForestProductionInitialization")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @export

calcForestProductionInitialization <- function(){
  
  ## Calculate overall timber production
  timber_production <- collapseNames(calcOutput("TimberDemand",aggregate = F)[,"y1995","production"])[,,c("Roundwood")]
  
  ## Splitting production -- Calculating production share 
  prod_plant_share <- setYears(collapseNames(readSource("TimberShare",subtype = "abare")),NULL)
  
  ## Create dummy magpie object with production from plantations and natveg
  split_prodn <- setNames(timber_production,"forestry")
  split_prodn <- add_columns(split_prodn,addnm = "natveg",dim = 3.1)
  
  ## Filling the values
  split_prodn[,,"forestry"] <- setNames(timber_production,NULL) * prod_plant_share
  split_prodn[,,"natveg"] <- setNames(timber_production,NULL) - setNames(split_prodn[,,"forestry"],NULL)
  
  ## Check if a country has 0 area but still reporting production
  FAI <- calcOutput("ForestAreaInitialization",aggregate = F) ## Calling the area data
  
  ## Check 0 area and positive production in plantations
  zero_check <- where(FAI[,,"forestry"]==0 & split_prodn[,,"forestry"]>0)$true$regions
  if(length(zero_check!=0)){
    cat("Countries without plantation area but positive production from plantation detected ---")
    FAI[zero_check,,"forestry"] <- 0.1
  }
  
  ## Check 0 area and positive production in natveg
  zero_check <- where(FAI[,,"natveg"]==0 & split_prodn[,,"natveg"]>0)$true$regions
  if(length(zero_check!=0)){
    cat("Countries without natveg area but positive production from natveg detected ---")
    FAI[zero_check,,"natveg"] <- 2
  }
  
  out <- setYears(mbind(setNames(split_prodn[,,"forestry"],"forestry_prod"),setNames(FAI[,,"forestry"],"forestry_area"),
                        setNames(split_prodn[,,"natveg"],"natveg_prod"),setNames(FAI[,,"natveg"],"natveg_area")),NULL)
  
  #  weight <- setNames(out[,,1],NULL)
  #  weight[weight>=0] = 1
  
  return(list(x=out,
              weight=NULL,
              min=0,
              unit="factor",
              description="Calculates forestry area for preparing plantation management factors in MAgPIE."))
  
}
