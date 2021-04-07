#' @title calc European Reference Emissions
#' @description provides European 2030 emission targets (for 2030)reference 2005)
#'
#' @return 2030 emission reductions tragets for 40%, 55% and 64% reductions in relation to 2005 values

#' @author Falk Benke and Renato Rodrigues
#' 
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EmiReference")
#' }
#' 

calcEmiReference <-  function(){
  
  eea.emi.ets <- readSource("EEA_EuropeanEnvironmentAgency", subtype="sectoral")[,,"Emi|GHG|ETS (Mt CO2-equiv/yr)"]
  eea.emi.ets[is.na(eea.emi.ets)] <- 0
  eea.emi.esd <- readSource("EEA_EuropeanEnvironmentAgency", subtype="sectoral")[,,"Emi|GHG|ESD (Mt CO2-equiv/yr)"] 
  eea.emi.esd[is.na(eea.emi.esd)] <- 0
  
  #Possible sources for 1990 emissions 
  #(1) Table ES. 3GHG emissions in million tonnes CO2equivalent (excl. LULUCF)
  # https://www.eea.europa.eu//publications/european-union-greenhouse-gas-inventory-2020
  #eea.emi.total_no_lulucf <- 5658.7 #CO2 emissions include indirect CO2
  #eea.emi.total <- 5413 #Total (with net CO2 emissions/removals)
  # (2) EEA_EuropeanEnvironmentAgency
  #eea.emi.total <- readSource("EEA_EuropeanEnvironmentAgency", subtype="total") # it possibly includes more than ETS + ESD (bunkers? and lulucf?) # eea.emi.total[,1990,"Emi|GHGtot (Mt CO2-equiv/yr)"] (total EU 1990 -> 5721.371)
  # (3) EuropeanEnergyDatasheets
  eea.emi.total_EEA <- readSource("EuropeanEnergyDatasheets") # it matches the EEA sectoral, so it should include only ETS + ESD, however it is the closest one to the CRF values for 1990, source 1 (total EU 1990 -> 5652.164), so it is being used as the default for now
  # (4) EEA_sectoral, data only from 2005 onward
  #eea.emi.total_EEA <- setNames(eea.emi.ets + eea.emi.esd, "Emi|GHGtot without Bunkers and LULUCF (Mt CO2-equiv/yr)") 
  
  # ETS + ESD emission target reduction in relation to 1990 = 40% reduction by 2030
  
  
  out <- NULL
  out <- mbind(out,
               setNames(setYears(eea.emi.total_EEA[,1990,"Emi|GHGtot (Mt CO2-equiv/yr)"]*0.40,2030), "Emi|GHGtot|target|40% (Mt CO2-equiv/yr)"), # target without lulucf
               setNames(setYears(eea.emi.total_EEA[,1990,"Emi|GHGtot (Mt CO2-equiv/yr)"]*0.55,2030), "Emi|GHGtot|target|55% (Mt CO2-equiv/yr)"), # target without lulucf
               setNames(setYears(eea.emi.total_EEA[,1990,"Emi|GHGtot (Mt CO2-equiv/yr)"]*0.65,2030), "Emi|GHGtot|target|65% (Mt CO2-equiv/yr)")  # target without lulucf
               )
  
  # ESD emission target - per country ESD reduction target for 2030 = reduction of 30% by 2030 compared to 2005
  esd_target_perc <- readSource("Eurostat_EffortSharing",subtype="target")
  esd_target <- setYears(eea.emi.esd[,2005,],NULL)*(1+esd_target_perc)
  esd_target[esd_target==0] <- NA
  out <- mbind(out,
               setNames(esd_target[,2030,], "Emi|GHG|ES|target|40% (Mt CO2-equiv/yr)")
  )
  
  # ETS emission target (reduction of 2030 emissions by 43% compared to 2005)
  ets_target <- setYears(eea.emi.ets[,2005,]*(1-0.43),2030)
  ets_target[ets_target==0] <- NA
  out <- mbind(out,
               setNames(ets_target, "Emi|GHG|ETS|target|40% (Mt CO2-equiv/yr)")
  )
  
  return(list(x=out,weight=NULL,unit="Mt CO2-equiv/yr",description="Emission reduction targets"))
}