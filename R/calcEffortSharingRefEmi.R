#' @title calc Effort Sharing Reference Emissions
#' @description provides region specific Effort Sharing Reference Emissions
#'
#' @param subtype type of reference emissions used to define emission reduction target fo European Effort Sharing Decision: EEA_GHG, Eurostat_GHG, REMIND_GHG (deprecated) or REMIND_CO2.
#' @return 2005 reference emissions to calculate effort sharing decision targets
#' @author Renato Rodrigues
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EffortSharingRefEmi",subtype="Eurostat_GHG")
#' }
#' 

calcEffortSharingRefEmi <- function(subtype){
  
  if(subtype=="EEA_GHG"){
    
    e <- readSource("EEA_EuropeanEnvironmentAgency", subtype="ESR")[,2005,"Emi|GHG|ESR (Mt CO2-equiv/yr)"] 
    #e <- readSource("EEA_EuropeanEnvironmentAgency", subtype="sectoral")[,2005,"Emi|GHG|ESR (Mt CO2-equiv/yr)"]
    e[is.na(e)] <- 0
    description <- "Effort sharing reference 2005 emissions in Mt CO2-equiv from EEA data" 
    unit <- "Mt CO2-equiv"
    
  } else if(subtype=="Eurostat_GHG"){
    e <- readSource("Eurostat_EffortSharing",subtype="emissions")[,2005,] 
    description <- "Effort sharing reference 2005 emissions in Mt CO2-equiv from Eurostat" 
    unit <- "Mt CO2-equiv"
    
  } else if (subtype=="REMIND_GHG") {
    e_ES <- readSource("Eurostat_EffortSharing",subtype="emissions") 
    EU11map <- toolGetMapping("regionmapping_21_EU11.csv", type = "regional", where = "mappingfolder")
    e_REMIND <- as.magpie(data.frame(
       period=rep(2005,9),
       regionscode=c("DEU","ECE","ECS","ENC","ESC","ESW","EWN","FRA","UKI"),
       #REMIND_ESR_Emi=c(477.0574005,282.5551076,207.0889494,226.7403476,385.869115,341.4182047,313.5259699,395.4965811,394.6661034))) # GHG ESR 2005 REMIND emissions
       REMIND_ESR_Emi=c(469.902686605144,255.724675477596,166.732543751469,126.497826068766,363.185841688919,298.622613003952,308.269435719629,383.162590392638,409.297607369399))) # GHG ESR 2005 REMIND emissions (from REMIND)
    
    e <- toolAggregate(e_REMIND,EU11map[which(EU11map$RegionCode %in% getRegions(e_REMIND)), ][,c(1,2,3)],e_ES[EU11map[which(EU11map$RegionCode %in% getRegions(e_REMIND)), ]$CountryCode,2005,])
    e <- toolCountryFill(e,fill=0)
    description <- "Effort sharing reference 2005 emissions in Mt CO2-equiv from REMIND" 
    unit <- "Mt CO2-equiv"
    
  } else if (subtype=="REMIND_CO2") {
    e_ES <- readSource("Eurostat_EffortSharing",subtype="emissions") 
    EU11map <- toolGetMapping("regionmapping_21_EU11.csv", type = "regional")
    e_REMIND <- as.magpie(data.frame(
      period=rep(2005,9),
      regionscode=c("DEU","ECE","ECS","ENC","ESC","ESW","EWN","FRA","UKI"),
      REMIND_ESR_Emi=c(394.2864406,162.5137758,98.05365416,88.50179503,288.7617393,221.2791697,271.4385753,290.2774547,308.7078729))) # CO2 ESR 2005 REMIND emissions
    e <- toolAggregate(e_REMIND,EU11map[which(EU11map$RegionCode %in% getRegions(e_REMIND)), ][,c(1,2,3)],e_ES[EU11map[which(EU11map$RegionCode %in% getRegions(e_REMIND)), ]$CountryCode,2005,])
    e <- toolCountryFill(e,fill=0)
    description <- "Effort sharing reference 2005 emissions in Mt CO2 from REMIND" 
    unit <- "Mt CO2"
    
  }
  getNames(e) <- NULL
  
  return(list(x=e, weight=NULL,unit=unit,description=description)) 
}
