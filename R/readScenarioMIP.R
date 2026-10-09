#' Read ScenarioMIP R10 data
#'
#' @param subtype "FGasScen": F-gas emissions (HFC species, CF4, C2F6, C6F14, SF6, total F-gases) of the
#' MESSAGEix-GLOBIOM-GAINS 2.1-M-R12 scenarios, to be used as the basis of scenarios for f_emiFgas.
#' "FGasSpecies": ScenarioMIPFGasSpecies.csv, the table of the F-gas species (smip_variable, remind_variable,
#' edgar_gas, ar6_gas, type) linking ScenarioMIP, REMIND, EDGAR7 and AR6GWP names; returned as a data.frame
#' (list(x, class = "data.frame") for madrat), read it with `convert = FALSE`.
#' @return magpie object with R10 regions, years and the dimensions scenario.variable. HFC species and PFC/SF6 are in kt
#' of each gas per year, "Emissions|F-Gases" in Mt CO2-equiv/yr.
#' @author Gabriel Abrahao
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect matches
readScenarioMIP <- function(subtype = "FGasScen") {
  if (subtype == "FGasSpecies") {
    return(list(x = read.csv("ScenarioMIPFGasSpecies.csv", stringsAsFactors = FALSE), class = "data.frame"))
  }
  stopifnot(subtype == "FGasScen")
  # The full database is large, so a copy filtered to the F-gas variables of the MESSAGEix model is accepted as well.
  # Both have the same layout: model, scenario, region, variable, unit and one column per year
  if (file.exists("ScenarioMIP_v0.1_R10.xlsx")) {
    d <- read_excel("ScenarioMIP_v0.1_R10.xlsx", sheet = "data")
  } else {
    d <- read.csv("ScenarioMIP_v0.1_R10_FGasScen.csv", check.names = FALSE)
  }
  names(d) <- tolower(names(d))
  # The F-gas variables of the ScenarioMIP database that are read, by their full names (no pattern matching on purpose).
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
  d <- d[d$model == "MESSAGEix-GLOBIOM-GAINS 2.1-M-R12" & d$variable %in% fgasVariables, ]
  missingVariables <- setdiff(fgasVariables, unique(d$variable))
  if (length(missingVariables) > 0) {
    message("readScenarioMIP: not in the data for this model: ", paste(missingVariables, collapse = ", "))
  }
  d <- pivot_longer(d, matches("^[0-9]{4}$"), names_to = "year", values_to = "value")
  # The database has no value for the years a scenario does not report (for example 2026-2029, only every fifth year
  # after 2025 is filled), so dropping the NA values is what defines the years of each scenario and variable
  d <- d[!is.na(d$value), c("region", "year", "scenario", "variable", "value")]
  as.magpie(d, spatial = "region", temporal = "year")
}
