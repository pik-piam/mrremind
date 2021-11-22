#' @title calc European Reference Emissions
#' @description provides European 2030 emission targets in relation to 1990 and 2005 emissions
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
  
  #eea.emi.ets <- readSource("EEA_EuropeanEnvironmentAgency", subtype="sectoral")[,,"Emi|GHG|ETS (Mt CO2-equiv/yr)"]
  eea.emi.ets <- setNames(dimSums(readSource("EEA_EuropeanEnvironmentAgency", subtype="ETS")[,,c("2_ Verified emissions.20-99 All stationary installations","3_ Estimate to reflect current ETS scope for allowances and emissions.20-99 All stationary installations")]),"Emi|GHG|ETS (Mt CO2-equiv/yr)")
  eea.emi.ets[is.na(eea.emi.ets)] <- 0
  #eea.emi.esr <- readSource("EEA_EuropeanEnvironmentAgency", subtype="sectoral")[,,"Emi|GHG|ESR (Mt CO2-equiv/yr)"] 
  eea.emi.esr <- readSource("EEA_EuropeanEnvironmentAgency", subtype="ESR")[,,"Emi|GHG|ESR (Mt CO2-equiv/yr)"] 
  eea.emi.esr[is.na(eea.emi.esr)] <- 0
  
  #Possible sources for 1990 emissions 
  #(1) Table ES. 3GHG emissions in million tonnes CO2equivalent (excl. LULUCF)
  # https://www.eea.europa.eu//publications/european-union-greenhouse-gas-inventory-2020
  #eea.emi.total_no_lulucf <- 5658.7 #CO2 emissions include indirect CO2
  #eea.emi.total <- 5413 #Total (with net CO2 emissions/removals)
  # (2) EEA_EuropeanEnvironmentAgency
  #eea.emi.total <- readSource("EEA_EuropeanEnvironmentAgency", subtype="total") # it possibly includes more than ETS + ESR (bunkers? and lulucf?) # eea.emi.total[,1990,"Emi|GHGtot (Mt CO2-equiv/yr)"] (total EU 1990 -> 5721.371)
  # (3) EuropeanEnergyDatasheets
  eea.emi.total_EEA <- readSource("EuropeanEnergyDatasheets") # it matches the EEA sectoral, so it should include only ETS + ESR, however it is the closest one to the CRF values for 1990, source 1 (total EU 1990 -> 5652.164), so it is being used as the default for now
  # (4) EEA_sectoral, data only from 2005 onward
  #eea.emi.total_EEA <- setNames(eea.emi.ets + eea.emi.esr, "Emi|GHGtot without Bunkers and LULUCF (Mt CO2-equiv/yr)") 
  
  # ETS + ESR emission target reduction in relation to 1990 = 40% reduction by 2030
  
  
  out <- NULL
  out <- mbind(out,
               setNames(setYears(eea.emi.total_EEA[,1990,"Emi|GHGtot (Mt CO2-equiv/yr)"]*(1-0.40),2030), "Emi|GHGtot|target|40% (Mt CO2-equiv/yr)"), # target without lulucf
               setNames(setYears(eea.emi.total_EEA[,1990,"Emi|GHGtot (Mt CO2-equiv/yr)"]*(1-0.55),2030), "Emi|GHGtot|target|55% (Mt CO2-equiv/yr)"), # target without lulucf
               setNames(setYears(eea.emi.total_EEA[,1990,"Emi|GHGtot (Mt CO2-equiv/yr)"]*(1-0.65),2030), "Emi|GHGtot|target|65% (Mt CO2-equiv/yr)")  # target without lulucf
               )
  
  # ESR emission target - per country ESR reduction target for 2030 = reduction of 40% by 2030 compared to 2005
  esr_target_perc <- readSource("Eurostat_EffortSharing",subtype="target")
  esr_target <- setYears(eea.emi.esr[,2005,],NULL)*(1+esr_target_perc)
  esr_target[esr_target==0] <- NA
  out <- mbind(out,
               setNames(esr_target[,2030,], "Emi|GHG|ESR|target|40% (Mt CO2-equiv/yr)")
  )
  
  # ETS emission target (reduction of 2030 emissions by 61% compared to 2005)
  ets_target <- setYears(eea.emi.ets[,2005,]*(1-0.61),2030)
  ets_target[ets_target==0] <- NA
  out <- mbind(out,
               setNames(ets_target, "Emi|GHG|ETS|target|61% (Mt CO2-equiv/yr)")
  )
  
  return(list(x=out,weight=NULL,unit="Mt CO2-equiv/yr",description="Emission reduction targets"))
}
