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
   mutate(value = ifelse((is.na(.data$value) | .data$value == 0), NA,
                         .data$value)) %>%
   group_by(!!!syms(c('scenario', 'region', 'item'))) %>%
   mutate(tmp = all(is.na(.data$value)),
          value = ifelse(.data$tmp, 0.5, .data$value)) %>%  # , value = ifelse(all(is.na(value)),0.5,value) overwrites entries if !all(is.na)
   select(-'tmp') %>% ungroup() %>%
   interpolate_missing_periods_(list(period = getYears(output,T)),expand.values = T) %>%
   removeColNa() %>%
   select('scenario', 'region', 'period', 'item', 'value') %>%
   as.magpie(spatial = 2, temporal = 3, data = 5)


 weight_fe = fe
 weight_fe[weight_fe == 0 ] <- 1e-6

  return(list(x=output,weight=weight_fe,
              unit = "energy efficiency paths",
              description = "Efficiency pathways for CES variables outside the calibration CES nest"))
}
