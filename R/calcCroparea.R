#' @title calcCroparea
#' @description Returns harvested areas of individual crops from FAOSTAT. Total harvested areas can be lower or higher than arable land because of multicropping or fallow land.
#' 
#' @param sectoral for landtype = "area_harvested" this aggregates FAO products. "ProductionItem" for unaggregated ProdSTAT items, "FoodBalanceItem" for Food Balance Sheet categories, "kcr" for MAgPIE items and "lpj" for LPJmL items.
#' @param physical if TRUE the sum over all crops agrees with the cropland area per country
#' @param cellular if TRUE crop area it calculates the cellular MAgPIE crop area for all magpie croptypes. Crop area from LUH2 crop types (c3ann, c4ann, c3per, c4per, cnfx) 
#' are mapped to MAgpIE crop types using mappingLUH2cropsToMAgPIEcrops.csv. Harvested areas of FAO weight area within a specific LUH crop type to devide into MAgPIE crop types.
#' @param irrigation if true, the cellular areas are returned sperated by irrigated and rainfed. More about irrigation setup in calcLUH2v2 
#'
#' @return areas of individual crops from FAOSTAT and weight
#' @author Ulrich Kreidenweis, Kristine Karstens
#' @importFrom utils read.csv
#' @importFrom magclass fulldim setNames getCells
#' @importFrom magpiesets findset
#' @importFrom madrat toolAggregate


calcCroparea <- function(sectoral="kcr", physical=TRUE, cellular=FALSE, irrigation=FALSE) {
  
  selectyears<-findset("past")
  
  if(!cellular){
    
    if(irrigation) stop("Irrigation levels for country based data not yet implemented!")
    
    #################################
    ### Croparea on country level ###
    #################################
    
    if (!is.null(sectoral) & !(sectoral=="lpj")) {
      CropPrim <- readSource("FAO", "Crop")[,selectyears,"area_harvested"]
      Fodder <- readSource("FAO", "Fodder")[,selectyears,"area_harvested"]

      data <- toolFAOcombine(CropPrim, Fodder)
      data <- data[,,"(Total)", pmatch=TRUE, invert=TRUE]
      data <- data/10^6 # convert to Mha
      
      if (sectoral=="FoodBalanceItem") {

        aggregation <- toolGetMapping("FAOitems.rda", type = "sectoral", where="moinput")
        data <- toolAggregate(data, rel=aggregation, from="ProductionItem", to="FoodBalanceItem", dim=3.1, partrel=T)
        
      } else if (sectoral=="kcr") {

        aggregation <- toolGetMapping("FAOitems.rda", type = "sectoral", where="moinput")
        data <- toolAggregate(data, rel=aggregation, from="ProductionItem", to="k", dim=3.1, partrel=T)
        
        # add bioenergy with 0 values
        data <- add_columns(x = data,addnm = c("betr","begr"),dim = 3.1)
        data[,,c("betr","begr")] <- 0
        
        # remove all non kcr items
        kcr <- findset("kcr")
        remove <- setdiff(fulldim(data)[[2]][[3]],kcr)
        if(length(remove)>0){
          remain_area <- mean( dimSums(data[,,"remaining.area_harvested"], dim=1)/dimSums(dimSums(data[,,"area_harvested"], dim=3), dim=1) )
          if (remain_area > 0.02) vcat(1,"Aggregation created a 'remaining' category. The area harvested is", round(remain_area,digits = 3)*100, "% of total \n")
          vcat(2, paste0("Data for the following items removed: ", remove))
          data <- data[,,kcr]
        }
        
      } else {stop("Sectoral aggregation not supported")}
      
    } else if(sectoral=="lpj"){
      
      MAGcroparea <- calcOutput("Croparea", sectoral="kcr", physical=physical, cellular=FALSE, irrigation=FALSE, aggregate=FALSE)
      MAGtoLPJ    <- read.csv(toolMappingFile("sectoral","MAgPIE_LPJmL.csv"))
      MAGtoLPJ    <- MAGtoLPJ[!(MAGtoLPJ$MAgPIE=="pasture"),]
      LPJcroparea <- toolAggregate(MAGcroparea, rel=MAGtoLPJ, from="MAgPIE", to="LPJmL", dim=3.1)
      data        <- LPJcroparea
      
    } else {stop("Sectoral aggregation not supported")}
    
    # use the share of the single crops to calculate their "physical" area
    if (physical) {
      cropland        <- setNames(collapseNames(calcOutput("FAOLand", aggregate=FALSE)[,,"6620|Arable land and Permanent crops"]), "crop")
      harvested_share <- data/dimSums(data, dim=3.1)
      commonyears     <- intersect(getYears(cropland),getYears(harvested_share))
      data            <- collapseNames(cropland[,commonyears,]*harvested_share[,commonyears,])
    }
    
    data[is.na(data)] <- 0
    
  } else {
    
    ##################################
    ### Croparea on cellular level ###
    ##################################
    
    if (sectoral=="kcr"){
      
      #LUH related data input on cell level
      LUHcroptypes     <- c("c3ann","c4ann","c3per","c4per","c3nfx")
      LUHcroparea      <- toolCell2isoCell(calcOutput("LUH2v2",landuse_types="LUH2v2", aggregate = FALSE, irrigation=irrigation, cellular=TRUE, selectyears="past"))
      LUHcroparea      <- LUHcroparea[,,LUHcroptypes]
      if(irrigation==TRUE) LUHcroparea <- LUHcroparea[,,"total",invert=TRUE] #if "total" is also reported magpie object grows to big (over 1.3Gb)
      
      LUHweights       <- calcOutput("LUH2MAgPIE", share = "MAGofLUH", aggregate = FALSE) 
      
      LUH2MAG          <- LUHcroparea * toolIso2CellCountries(LUHweights)
      MAGcroparea      <- dimSums(LUH2MAG, dim=3.1)
      data             <- collapseNames(MAGcroparea)
      
    } else if(sectoral=="lpj"){
      
      MAGcroparea   <- calcOutput("Croparea", sectoral="kcr", physical=physical, cellular=TRUE, irrigation=irrigation, aggregate = FALSE)
      MAGtoLPJ      <- read.csv(toolMappingFile("sectoral","MAgPIE_LPJmL.csv"))
      MAGtoLPJ      <- MAGtoLPJ[!(MAGtoLPJ$MAgPIE=="pasture"),]
      LPJcroparea   <- toolAggregate(MAGcroparea, rel=MAGtoLPJ, from="MAgPIE", to="LPJmL", dim="MAG")
      data          <- LPJcroparea
      
      
    } else { stop("Not possible (for now) for the given item set (sectoral)!")}
    
    if(!physical){
      
      MultiCropping <- calcOutput("Multicropping", selectyears = "past", aggregate = FALSE)
      
      data          <- data[,selectyears,] * toolIso2CellCountries(MultiCropping) 
      
    }
    
  }
  
  data <- collapseNames(data)
  
  return(list(x           = data,
              weight      = NULL, 
              unit        ="million ha", 
              description = "harvested crop areas from FAOSTAT",
              isocountries=!cellular))
}

