#' Efficiency paths for CES variables beyond calibration CES nest
#' 
#' Returns the Efficiency paths for Macro variables
#' 
#' @author Antoine Levesque
#' @importFrom quitte removeColNa interpolate_missing_periods_ inline.data.frame getVars

calcEnergyEffPaths <- function() {
  #----- Functions ------------------
  
  #----- READ-IN DATA ------------------
 fe = calcOutput("FEdemand",subtype = "FE_for_Eff",aggregate = F)
 ue = calcOutput("FEdemand",subtype = "UE_for_Eff",aggregate = F)
 #----- PARAMETERS ------------------
 t0 = 2005
 
  #----- PROCESS DATA ------------------
 
 ue_names = getNames(ue)
 fe = fe[,,ue_names]
 
 output = ue/fe
 
 # Fill NAs and Os, and replace with 50% efficiency for fully empty entries
 output = output %>% as.quitte() %>%
   mutate_(value = ~ifelse((is.na(value) | value == 0),NA,value)) %>%
   group_by_(~scenario,~region,~item) %>%
   mutate_(tmp = ~all(is.na(value)), value = ~ifelse(tmp,0.5,value)) %>%  # , value = ifelse(all(is.na(value)),0.5,value) overwrites entries if !all(is.na)
   select_(~-tmp) %>% ungroup() %>%
   interpolate_missing_periods_(list(period = getYears(output,T)),expand.values = T) %>%
   removeColNa() %>%
   as.magpie()
 

 weight_fe = fe
 weight_fe[weight_fe == 0 ] <- 1e-6
 
  return(list(x=output,weight=weight_fe,
              unit = "energy efficiency paths",
              description = "Efficiency pathways for CES variables outside the calibration CES nest"))
}
