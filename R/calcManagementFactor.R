#' @title calcManagementFactor
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
#' calcOutput("ManagementFactor")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @export

calcManagementFactor <- function(){
  
  ## Define a mapping
  map <- read.csv(toolMappingFile("regional",getConfig("regionmapping")),sep=";")
  
  ## Reading in the area data from FAO
  forestry_area <- readSource("FRA2015Doc","plantation_forest")
  forestry_area_reg <- toolAggregate(x = forestry_area,rel = map,from = "CountryCode",to = "RegionCode",dim = 1)
  
  ## Reading NatVeg area from FAO
  #natveg_area <- setNames(dimSums(mbind(readSource("FRA2015Doc","primary_forest"),readSource("FRA2015Doc","secondary_forest")),dim=3),"natveg_forest")
  natveg_area <- setNames(dimSums(mbind(readSource("FRA2015Doc","secondary_forest"),readSource("FRA2015Doc","primary_forest"))),"natveg_forest")
  natveg_area_reg <- toolAggregate(x = natveg_area,rel = map,from = "CountryCode",to = "RegionCode",dim = 1)
  
  ## Reading TOTAl timber production --- this will be split later into production from plantations and production from natveg
  ## FAO doesn't report on production sources for timber.
  ## We will read in total roundwood production.
  timber_production <- collapseNames(calcOutput("TimberDemand",aggregate = F)[,,"production"])[,,c("Roundwood")]
  timber_production_reg <- toolAggregate(x = timber_production,rel = map,from = "CountryCode",to = "RegionCode",dim = 1)
  
  zero_area <- where(forestry_area[,"y2015",]==0 & natveg_area[,"y2015",]==0)$true$regions
  timber_production[zero_area,,] = 0
  
  ## Calculate share of production coming through plantations based on FAO study ---- y2000 is reported
  prod_plant_share <- setYears(collapseNames(readSource("TimberShare",subtype = "abare")),NULL)
#  prod_plant_share[prod_plant_share>0.30] = 0.30

  ######### Splitting of production start
  natveg_production <- timber_production * (1-prod_plant_share)
  forestry_production <- timber_production * prod_plant_share
  #natveg_production <- timber_production_reg * (0.6)
  #forestry_production <- timber_production_reg * 0.4
  
  ## Representative yield from natveg and forestry ---- y2015 is the data without any NAs which were replaced by 0s
  # rep_yield_natveg <- setYears(natveg_production[,"y2015",],NULL)/setYears(natveg_area[,"y2015",],NULL)
  # rep_yield_forestry <- setYears(forestry_production[,"y2015",],NULL)/setYears(forestry_area[,"y2015",],NULL)
  rep_yield_natveg <- setNames(natveg_production[,"y2015",]/natveg_area[,"y2015",],"rep_yield_natveg")
  rep_yield_forestry <- setNames(forestry_production[,"y2015",]/forestry_area[,"y2015",],"rep_yield_forestry")
  
  ## Valid countries
  vc <- intersect(getRegions(rep_yield_forestry),getRegions(rep_yield_natveg))
  ry_forestry <- setNames(rep_yield_forestry[vc,,],NULL)
  ry_natveg <- setNames(rep_yield_natveg[vc,,],NULL)
  
  ry_forestry[is.infinite(ry_forestry)] <- 0
  sub <- mean(ry_forestry,na.rm = T)
  ry_forestry[is.na(ry_forestry)] <- sub
  
  ry_natveg[is.infinite(ry_natveg)] <- 0
  sub <- mean(ry_natveg,na.rm = T)
  ry_natveg[is.na(ry_natveg)] <- sub
  
  ## fix ry_natveg
  ry_natveg[where(ry_natveg==0)$true$regions,,]<- sub
  ry_natveg[where(ry_natveg==0 & ry_forestry==0)$true$regions,,] <- sub
  
  ## Problem_regs >30
  prob_reg <- where(ry_forestry/ry_natveg>30)$true$regions
  ry_forestry[prob_reg,,] <- 30
  ry_natveg[prob_reg,,] <- 1
  
  ## Fixing reg < 3
  fix_reg <- where(ry_forestry/ry_natveg<4)$true$regions
  ry_forestry[fix_reg,,] <- 4
  ry_natveg[fix_reg,,] <- 1
  
  out <- setYears(mbind(setNames(ry_forestry,"plantations"),setNames(ry_natveg,"natveg")),NULL)
  
  weight <- setYears(setNames(readSource("FRA2015Doc","forest_area")[,"y2015",],NULL),NULL)
  weight[weight>=0] = 1
  
  return(list(x=ceiling(out),
              weight=weight,
              min=0,
              unit="factor",
              description="Calculates forestry management factors"))
  
}
