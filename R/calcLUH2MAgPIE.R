#' @title calcLUH2MAgPIE
#' @description Calculates out of LUH2FAO and FAO2MAgPIE mappings the real aggregation of LUH croptypes to MAgPIE croptypes 
#' 
#' @param share total (for total numbers), LUHofMAG (for share of LUH within kcr types), MAGofLUH (for share of kcr within LUH types) 
#' @param bioenergy ignore (for 0 for share and totals), fix (for fixing betr and begr shares in LUHofMAG to 1 for c3per and c4per)
#' @param selectyears defaults to past
#' @return List of magpie objects with results on country level, weight on country level, unit and description. 
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LUH2MAgPIE")
#' }


calcLUH2MAgPIE <- function(share = "total", bioenergy = "ignore", selectyears = "past"){
  
  if(share == "total"){
    
    selectyears <- sort(findset(selectyears, noset = "original"))
    
    CropPrim    <- readSource("FAO", "Crop")/10^6
    Fodder      <- readSource("FAO", "Fodder")/10^6
    FAOdata     <- toolFAOcombine(CropPrim, Fodder)[,selectyears,"area_harvested"]
    
    if (any( grepl("+ (Total)",getNames(FAOdata, fulldim=T)[[1]], fixed = TRUE))) {
      FAOdata <- FAOdata[,,"+ (Total)", pmatch=T, invert=T]}
    
    kcr         <- findset("kcr")
  
    mapping     <- toolGetMapping("FAOvsLUHvsMAG_croptypes.csv", type="sectoral",  where="moinput")
    aggregation <- toolAggregate(FAOdata, rel=mapping, from="ProductionItem", to="LUH2kcr", dim=3.1, partrel = TRUE)
    aggregation <- add_columns(aggregation, addnm = c("betr", "begr"), dim = 3.2)
    aggregation <- aggregation[,,kcr]
    aggregation <- complete_magpie(collapseNames(aggregation), fill = 0)
    aggregation[which(is.na(aggregation))] <- 0
    getSets(aggregation, fulldim=FALSE) <- c("ISO","Year","LUH.MAG")
    
    x    <- aggregation
    unit <- "million ha"
    
  } else if(share == "LUHofMAG"){
    
    aggregation <- calcOutput("LUH2MAgPIE", aggregate = FALSE, selectyears = selectyears)
    
    MAG  <- dimSums(aggregation, dim="LUH")
    x    <- aggregation/MAG
    x[which(is.na(x))] <- 0
    unit <- "share of area"
    
    if(bioenergy == "fix"){
      
      x[,,"c3per.betr"] <- 1
      x[,,"c4per.begr"] <- 1
      
    } else if(bioenergy != "ignore"){stop("2nd generation bioenergy setting not supported")}
    
  } else if(share == "MAGofLUH"){
    
    aggregation <- calcOutput("LUH2MAgPIE", aggregate = FALSE, selectyears = selectyears)
    
    LUH  <- dimSums(aggregation, dim="MAG")
    x    <- aggregation/LUH
    x[which(is.na(x))] <- 0
    unit <- "share of area"
    
    if(bioenergy != "ignore"){stop("2nd generation bioenergy setting not supported")}
    
  } else {stop("Share type not supported")}
  
  return(list(
    x=x,
    weight=NULL,
    unit=unit,
    description="Relation matrix for LUH croptype and MAgPIE croptype areas"))
}