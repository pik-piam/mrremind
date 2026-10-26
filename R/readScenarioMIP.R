#' Read ScenarioMIP R10 data
#'
#' @param subtype "FGasScen": F-gas emissions (HFC species, CF4, C2F6, C6F14, SF6, total F-gases) of the scenarios
#' "Very Low - SSP1" and "High - SSP5" of the models MESSAGEix-GLOBIOM-GAINS 2.1-M-R12 (named MESSAGEix) and IMAGE 3.4
#' (named IMAGE), to be used as the basis of scenarios for f_emiFgas.
#' "FGasSpecies": ScenarioMIPFGasSpecies.csv, the table of the F-gas species (smip_variable, remind_variable,
#' edgar_gas, ar6_gas, type) linking ScenarioMIP, REMIND, EDGAR7 and AR6GWP names; returned as a data.frame
#' (list(x, class = "data.frame") for madrat), read it with `convert = FALSE`.
#' @return magpie object with R10 regions, years and the dimensions model.scenario.variable. HFC species and PFC/SF6 are
#' in kt of each gas per year, "Emissions|F-Gases" in Mt CO2-equiv/yr.
#' @author Gabriel Abrahao
#' @importFrom quitte read.quitte
readScenarioMIP <- function(subtype = "FGasScen") {
  if (subtype == "FGasSpecies") {
    return(list(x = read.csv("ScenarioMIPFGasSpecies.csv", stringsAsFactors = FALSE), class = "data.frame"))
  }
  stopifnot(subtype == "FGasScen")
  # The full database is large, so a copy filtered to the F-gas variables of the MESSAGEix model is accepted as well.
  # Both have the same layout (model, scenario, region, variable, unit and one column per year), and read.quitte reads
  # either and puts the years in long format
  file <- "ScenarioMIP_v0.1_R10_FGasScen.csv"
  if (file.exists("ScenarioMIP_v0.1_R10.xlsx")) file <- "ScenarioMIP_v0.1_R10.xlsx"

  # The F-gas variables of the ScenarioMIP database that are read, by their full names.
  # Units in the database: the HFC species, CF4, C2F6, C6F14 and SF6 in kt of each gas per year, the total in
  # Mt CO2-equiv per year
  fgasVariables <- c(
    "Emissions|HFC|HFC125",    # HFC-125, Annex F Group I
    "Emissions|HFC|HFC134a",   # HFC-134a, Annex F Group I
    "Emissions|HFC|HFC143a",   # HFC-143a, Annex F Group I
    "Emissions|HFC|HFC227ea",  # HFC-227ea, Annex F Group I
    "Emissions|HFC|HFC245fa",  # HFC-245fa, Annex F Group I
    "Emissions|HFC|HFC32",     # HFC-32, Annex F Group I
    "Emissions|HFC|HFC43-10",  # HFC-43-10mee, Annex F Group I
    "Emissions|HFC|HFC23",     # HFC-23, Annex F Group II
    "Emissions|CF4",           # PFC-14
    "Emissions|C2F6",          # PFC-116, not reported by the MESSAGEix model (only IMAGE)
    "Emissions|C6F14",         # PFC-51-14, not reported by the MESSAGEix model (only IMAGE)
    "Emissions|SF6",
    "Emissions|F-Gases"        # total F-gases in Mt CO2-equiv/yr (there is no "Emissions|HFC" subtotal in the database)
  )
  # The models and scenarios that are read, by their full names. The filtered csv only has these, and the full database
  # has many more, so naming them here makes both files give the same result. The models get short names in the
  # result: a dot in a name would be taken as a separator between dimensions by magclass, and these names have dots
  models <- c(
    MESSAGEix = "MESSAGEix-GLOBIOM-GAINS 2.1-M-R12",   # does not report C2F6, C6F14, and has no HFC-23 emissions
    IMAGE = "IMAGE 3.4"                                # reports HFC-23, C2F6 and C6F14
  )
  scenarios <- c(
    "Very Low - SSP1",         # strong mitigation, the basis of the Kigali scenarios in calcFGas
    "High - SSP5"              # high emissions, not used so far
  )
  # The database has no value for the years a scenario does not report (for example 2026-2029, only every fifth year
  # after 2025 is filled), so dropping the NA values is what defines the years of each scenario and variable
  d <- read.quitte(file, drop.na = TRUE, filter.function = function(x) {
    x[x$model %in% models & x$scenario %in% scenarios & x$variable %in% fgasVariables, ]
  })
  d$model <- names(models)[match(d$model, models)]
  d <- d[, c("region", "period", "model", "scenario", "variable", "value")]
  names(d)[names(d) == "period"] <- "year"      # the time set is called year in the other functions that use this data
  as.magpie(d, spatial = "region", temporal = "year")
}
