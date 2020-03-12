#' @title calcLUH2MAgPIE
#' @description Calculates out of LUH2FAO and FAO2MAgPIE mappings the real aggregation of LUH croptypes to MAgPIE croptypes 
#' 
#' @param share total (for total numbers), LUHofMAG (for share of LUH within kcr types), MAGofLUH (for share of kcr within LUH types) 
#' @param bioenergy ignore (for 0 for share and totals), fix (for fixing betr and begr shares in LUHofMAG to 1 for c3per and c4per)
#' @param selectyears defaults to past
#' @param missing "ignore" will leave data as is. "fill" will add proxy values for data gaps of FAO. 
#' @return List of magpie objects with results on country level, weight on country level, unit and description. 
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LUH2MAgPIE")
#' }
#' 
#' @importFrom magpiesets findset

calcLUH2MAgPIE <- function(share = "total", bioenergy = "ignore", selectyears = "past", missing="ignore"){
  
  if(share == "total"){
    
    if(missing=="fill") warning("No missing data for total numbers assumend.")
    
    FAOdata     <- calcOutput("Croparea", sectoral="ProductionItem", physical=FALSE, aggregate = FALSE)
  
    kcr         <- findset("kcr")
    mapping     <- toolGetMapping("FAOvsLUHvsMAG_croptypes.csv", type="sectoral",  where="mappingfolder")
    
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
    
    if(missing=="fill"){
      
      # check for countries/years where no data is reported from FAO and fill with proxy of similiar country
      noData       <- where(dimSums(toolIso2CellCountries(x), dim=3)==0)$true$individual
      proxyMapping <- c(ATF="ISL", ESH="MAR", FLK="ISL", GRL="ISL" , PSE="ISR", SGS="ISL", SJM="NOR", SSD="SDN")
      for(i in row(noData)[,1]) x[noData[i,"ISO"], noData[i,"Year"],]  <- x[proxyMapping[noData[i,"ISO"]], noData[i,"Year"],] 
  
      # check for countries/years/croptypes where no data is reported from FAO and fill with default values
      noData     <- where(dimSums(x, dim=3.1)==0)$true$individual
      meanValues <- dimSums(x * dimSums(aggregation, dim="LUH"), dim="ISO")/dimSums(aggregation, dim=c("ISO","LUH"))
      meanValues[is.nan(meanValues)] <- 0
      for(i in row(noData)[,1]) x[noData[i,"ISO"], noData[i,"Year"], noData[i,"MAG"]]  <- meanValues[, noData[i,"Year"], noData[i,"MAG"]] 

      # check constsistency
      if(any(round(dimSums(x, dim=3.1),4)!=1)) warning("Not all factors could been filled, even so 'missing' was set to 'fill'.")
    } 
    
  } else if(share == "MAGofLUH"){
    
    aggregation <- calcOutput("LUH2MAgPIE", aggregate = FALSE, selectyears = selectyears)
    
    LUH  <- dimSums(aggregation, dim="MAG")
    x    <- aggregation/LUH
    x[which(is.na(x))] <- 0
    unit <- "share of area"
    
    if(bioenergy != "ignore"){stop("2nd generation bioenergy setting not supported")}
    
    if(missing=="fill"){
      
      # check for countries/years where no data is reported from FAO and fill with proxy
      noData       <- where(dimSums(toolIso2CellCountries(x), dim=3)==0)$true$individual
      proxyMapping <- c(ATF="ISL", ESH="MAR", FLK="ISL", GRL="ISL" , PSE="ISR", SGS="ISL", SJM="NOR", SSD="SDN")
      for(i in row(noData)[,1]) x[noData[i,"ISO"], noData[i,"Year"],]  <- x[proxyMapping[noData[i,"ISO"]], noData[i,"Year"],] 
      
      # check for countries/years/croptypes where no data is reported from FAO and fill with default values
      noData       <- where(dimSums(x, dim=3.2)==0)$true$individual
      meanValues   <- dimSums(x * dimSums(aggregation, dim="MAG"), dim="ISO")/dimSums(aggregation, dim=c("ISO","MAG"))
      for(i in row(noData)[,1]) x[noData[i,"ISO"], noData[i,"Year"], noData[i,"LUH"]]  <- meanValues[, noData[i,"Year"], noData[i,"LUH"]] 
      
      # check constsistency
      if(any(round(dimSums(x, dim=3.2),4)!=1)) warning("Not all factors could been filled, even so 'missing' was set to 'fill'.")
    }  
    
  } else {stop("Share type not supported")}
  
  return(list(
    x=x,
    weight=NULL,
    unit=unit,
    description="Relation matrix for LUH croptype and MAgPIE croptype areas"))
}
