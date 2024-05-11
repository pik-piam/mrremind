#' calculate exogenuous FE and ES demand pathways
#' @description  prepare data for exogenuous FE and ES demand pathways that do not come from EDGE models but from other sources and/or scenario literature.
#' REMIND can be fixed to those demand pathways if the switch cm_exogDem_scen is activated.
#'
#' @return A [`magpie`][magclass::magclass] object.
#' @author Felix Schreyer



calcExogDemScen <- function() {

  ### ARIADNE steel and cement production trajectories for Germany from FORECAST

  # read scenario data from Ariadne IIASA database
  AriadneDB.data <- readSource("AriadneDB")

  # which variables, scenarios and models from ariadne DB to select
  ariadne.vars <- c("Production|Steel|Primary", "Production|Steel|Secondary", "Production|Non-Metallic Minerals|Cement")
  ariadne.scens <- c("8Gt_Bal v2","8Gt_EnSec")
  ariadne.model <- c("FORECAST v1_0")


  # select only specified variables and scenarios for Germany,
  # convert from Mt production to Gt production
  ariadne.ind.demand <- mselect(AriadneDB.data["DEU",,]*1e-3,
                                variable = ariadne.vars,
                                scenario = ariadne.scens,model = ariadne.model)

  # map ariadne scenarios and variables to scenario and production factor names used for input data in REMIND
  mapping.scens <- data.frame( scen.ariadne = ariadne.scens,
                               scen.remind = c("ariadne_bal",
                                               "ariadne_ensec"))

  mapping.vars <- data.frame( vars.ariadne = ariadne.vars,
                              vars.remind = c("ue_steel_primary",
                                              "ue_steel_secondary",
                                              "ue_cement"))

  ariadne.ind.demand <- collapseNames(ariadne.ind.demand)


  ariadne.ind.demand <- toolAggregate(x = ariadne.ind.demand,
                       rel = mapping.scens,
                       dim=3.1)

  ariadne.ind.demand <- toolAggregate(x = ariadne.ind.demand,
                       rel = mapping.vars,
                       dim=3.2)

  # rearrange to ouptut format
  years <- c(2005, 2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2070, 2080, 2090, 2100)
  out <- new.magpie(getRegions(AriadneDB.data),
                    years = years,
                    names = getNames(ariadne.ind.demand),
                    fill = 0)

  # take data from ariadne DB only between 2025 and 2050
  ariadne.period <- seq(2025,2050,5)
  out["DEU",paste0("y",ariadne.period),] <- ariadne.ind.demand["DEU",paste0("y",ariadne.period),]

  # constant production after 2050
  # out["DEU",paste0("y",c(2055, 2060, 2070, 2080, 2090, 2100)),] <-  out["DEU","y2050",]

  ### end Ariadne trajectories

  x <- out


  return(list(
    x = x,
    weight = NULL,
    unit = c("Gt steel/yr for ue_steel_primary and ue_steel_secondary, Gt cement/yr for ue_cement"),
    description = "Exogenous demand scenarios that REMIND can be fixed to using cm_exogDem_scen"
  ))

}
