#' @title calcForestArea
#' @description Calculates consistent forest area and its subcategories based on FAO_FRA2015 and LanduseInitialisation data.
#'
#' @param selectyears defaults to past
#' @return List of magpie object with results on country level, weight, unit and description.
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ForestArea")
#' }
#' @export

calcForestArea <- function(selectyears="past"){
  
  years <- sort(findset(selectyears,noset = "original"))
  
  forest_country   <- readSource("FAO_FRA2015","fac")[,,c("Forest","NatFor","PrimFor","NatRegFor","PlantFor")]
  forest_country   <- time_interpolate(forest_country,interpolated_year = years,integrate_interpolated_years = TRUE,extrapolation_type = "constant")[,years,]
  vcat(verbosity = 3,"Forest is interpolated for missing years and held constant for the period before FAO starts")
  
  ### fix know issues
  
  forest_country["HND",,"PlantFor"]   <- forest_country["HND",,"Forest"] - forest_country["HND",,"NatFor"]  
  forest_country["IDN",,"Forest"]     <- forest_country["IDN",,"NatFor"] + forest_country["IDN",,"PlantFor"]  
  forest_country["FIN",,"NatRegFor"]  <- forest_country["FIN",,"NatFor"] - forest_country["FIN",,"PrimFor"]
  forest_country["PSE",,"PlantFor"]   <- 2/3*forest_country["PSE",,"Forest"]
  forest_country["PSE",,"NatRegFor"]  <- 1/3*forest_country["PSE",,"Forest"]
  
  ### fixing inconstinicies assuming total forest areas and shares of subcategories are reported correctly  
  
  forest_sumsub                       <- dimSums(forest_country[,,c("NatFor","PlantFor")], dim=3)
  forest_country[,,"PlantFor"]        <- toolNAreplace(forest_country[,,"PlantFor"]/forest_sumsub * setNames(forest_country[,,"Forest"],NULL))$x
  forest_country[,,"NatFor"]          <- toolNAreplace(forest_country[,,"NatFor"]/forest_sumsub * setNames(forest_country[,,"Forest"],NULL))$x
  
  forest_sumsubsub                    <- dimSums(forest_country[,,c("PrimFor","NatRegFor")], dim=3)
  forest_country[,,"PrimFor"]         <- toolNAreplace(forest_country[,,"PrimFor"]/forest_sumsubsub * setNames(forest_country[,,"NatFor"],NULL))$x
  forest_country[,,"NatRegFor"]       <- toolNAreplace(forest_country[,,"NatRegFor"]/forest_sumsubsub * setNames(forest_country[,,"NatFor"],NULL))$x
 
  ### fixing missing data on split between PrimFor (primforest), NatRegFor (secdforest) and PlantFor (forestry) with LUH data
  
  Landuse_Ini <- calcOutput("LanduseInitialisation", land="new", aggregate = FALSE, selectyears=selectyears ,cellular=FALSE)[,,c("primforest","secdforest","forestry")]
  
 
  reg_missing         <- where(round(dimSums(forest_country[,,c("NatFor", "PlantFor")], dim=3),6)!=round(forest_country[,,"Forest"],6))$true$reg    
  Ini_sum             <- dimSums(Landuse_Ini, dim=3)
  forest_country[reg_missing,,"PrimFor"]      <- toolNAreplace(setNames(Landuse_Ini[reg_missing,,"primforest"],"PrimFor")/Ini_sum[reg_missing,,] * setNames(forest_country[reg_missing,,"Forest"],NULL))$x
  forest_country[reg_missing,,"NatRegFor"]    <- toolNAreplace(Landuse_Ini[reg_missing,,"secdforest"]/Ini_sum[reg_missing,,] * setNames(forest_country[reg_missing,,"Forest"],NULL))$x
  forest_country[reg_missing,,"PlantFor"]     <- toolNAreplace(Landuse_Ini[reg_missing,,"forestry"]/Ini_sum[reg_missing,,] * setNames(forest_country[reg_missing,,"Forest"],NULL))$x
  forest_country[reg_missing,,"NatFor"]       <- forest_country[reg_missing,,"PrimFor"] + forest_country[reg_missing,,"NatRegFor"]
  
  reg_missing         <- where(round(dimSums(forest_country[,,c("PrimFor","NatRegFor")], dim=3),6)!=round(forest_country[,,"NatFor"],6))$true$reg     
  Ini_sumsub          <- dimSums(Landuse_Ini[,,c("primforest","secdforest")], dim=3)
  forest_country[reg_missing,,"PrimFor"]     <- toolNAreplace(Landuse_Ini[reg_missing,,"primforest"]/Ini_sumsub[reg_missing,,] * setNames(forest_country[reg_missing,,"NatFor"],NULL))$x
  forest_country[reg_missing,,"NatRegFor"]   <- toolNAreplace(Landuse_Ini[reg_missing,,"secdforest"]/Ini_sumsub[reg_missing,,] * setNames(forest_country[reg_missing,,"NatFor"],NULL))$x
  

  if(any(round(dimSums(forest_country[,,c("NatFor","PlantFor")], dim=3),4)!=round(forest_country[,,"Forest"],4))|
     any(round(dimSums(forest_country[,,c("NatRegFor","PrimFor")], dim=3),4)!=round(forest_country[,,"NatFor"],4))){
    vcat(verbosity = 2,"There are still inconsistencies within the forest area data set.")
  }
  
  map <- data.frame(fao    = c("Forest", "NatFor",     "PrimFor",    "NatRegFor",  "PlantFor"),
                    magpie = c("forest", "natrforest", "primforest", "secdforest", "forestry"))
  out <- toolAggregate(forest_country, map, from="fao", to="magpie", dim=3)

  return(list(x=out,
              weight=NULL,
              unit="Mha",
              description="Forest are and its subcategories")
  )
}

