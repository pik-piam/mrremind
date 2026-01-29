#' Read ExpertGuess
#'
#' Read-in data that are based on expert guess
#'
#' @md
#' @param subtype Type of data that should be read. One of
#'   - `biocharPrices`: Biochar price assumptions over time. Assumptions based on
#'      collection of current bulk sale prices (see Dorndorf et al (submitted)) (Tabea Dorndorf)
#'   - `capacityFactorGlobal`: Global capacity factors for all REMIND technologies (Renato Rodrigues)
#'   - `capacityFactorRules`: Capacity factor rules for selected H12 regions and REMIND technologies
#'   - `ccsBounds`: CCS bounds indicating the if a country is expected to do CCS in the
#'      foreseeable future (Jessica Strefler)
#'   - `co2prices`: CO2 prices (Robert Pietzcker)
#'   - `costsTradePeFinancial`: primary energy tradecosts (financial costs on import,
#'      export and use) (Nicolas Bauer)
#'   - `deltacapoffset`: ??? (Robert Pietzcker)
#'   - `gridFactor`: Estimates distribution of electricity demands per region (Robert Pietzcker)
#'   - `ies`: intertemporal elasticity of substitution (Nicolas Bauer)
#'   - `prtp`: pure rate of time preference (Nicolas Bauer)
#'   - `subConvergenceRollback`: Subsidy convergence level in rollback scenario in US$2017 (Nicolas Bauer)
#'   - `taxConvergence`: Tax convergence level in US$2017 (Nicolas Bauer)
#'   - `taxConvergenceRollback`: Tax convergence level in rollback scenario in US$2017 (Nicolas Bauer)
#'   - `tradeConstraints`: parameter by Nicolas Bauer (2024) for the region specific trade
#'      constraints, values different to 1 activate constraints and the value is used as
#'      effectiveness to varying degrees such as percentage numbers (Nicolas Bauer)
#'   - `tradecost`: old REMIND data for PE tradecosts (energy losses on import) (?)
#'
#' @return magpie object of the data
#' @author Lavinia Baumstark, Falk Benke
#' @examples
#' \dontrun{
#' a <- readSource(type = "ExpertGuess", subtype = "ies")
#' }
#'
#' @importFrom dplyr bind_rows filter pull select
#'
readExpertGuess <- function(subtype) {

  if (subtype == "biocharPrices") {

    out <- readxl::read_xlsx("biocharPrices_v0.1.xlsx", sheet = "pricePath") %>%
      as.magpie()

  } else if (subtype == "capacityFactorGlobal") {
    out <- read.csv("capacity-factors-global_REMIND_3.6.1.csv", sep = ";") %>%
      as.magpie(datacol = 2)

  } else if (subtype == "capacityFactorRules") {

    out <- read.csv("capacity-factor-rules_v1.0.csv", sep = ";") %>%
      as.magpie(datacol = 4)

  } else if (subtype == "ccsBounds") {

    out <- read.csv("CCSbounds.csv", sep = ";") %>%
      select("country" = "CountryCode", "value" = "Value") %>%
      as.magpie()

  } else if (subtype == "co2prices") {

    out <- read.csv("co2prices-2025-09.csv", sep = ";") %>%
      select(-"Country", -"RegionCode") %>%
      as.magpie()

    out[is.na(out)] <- 0

  } else if (subtype ==  "costsTradePeFinancial") {

    out <- read.csv("pm_costsTradePeFinancial_v1.1.csv", sep = ";", skip = 2) %>%
      as.magpie(spatial = 1, temporal = 0, datacol = 3) %>%
      collapseNames()

  } else if (subtype == "deltacapoffset") {

    out <- read.csv("p_adj_deltacapoffset_v1.0.0.csv", sep = ";") %>%
      as.magpie()
  } else if (subtype == "gridFactor") {

    out <- read.csv("homogenous_regions_for_grids_v1.0.csv", sep = ";") %>%
      dplyr::select("country" = "CountryCode", "value" = "grid.factor") %>%
      as.magpie(datacol = 2)

  } else if (subtype == "ies") {

    out <- read.csv("ies.csv", sep = ";") %>%
      select("country" = "CountryCode", "value" = "Value") %>%
      as.magpie()

    getYears(out) <- "2005"

  } else if (subtype == "prtp") {

    out <- read.csv("prtp.csv", sep = ";") %>%
      select("country" = "CountryCode", "value" = "Value") %>%
      as.magpie()

    getYears(out) <- "2005"

  } else if (subtype == "subConvergenceRollback") {

    out <- read.csv("sub_convergence_rollback_v1.0.csv", sep = ";",
                    col.names = c("Year", "Region", "sector", "FE", "value")) %>%
      as.magpie(datacol = 5)

  } else if (subtype == "taxConvergence") {

    out <- read.csv("tax_convergence_v1.0.csv", sep = ";") %>%
      as.magpie(datacol = 4)

  } else if (subtype == "taxConvergenceRollback") {

    out <- read.csv("tax_convergence_rollback_v1.0.csv", sep = ";", col.names = c("Year", "Region", "FE", "value")) %>%
      as.magpie(datacol = 4)

  } else if (subtype == "tradeConstraints") {

    out <- read.csv("tradeConstraints.csv", sep = ";") %>%
      as.magpie()

  } else if (subtype == "tradecost") {

    out <- read.csv("LueckenDiss_TradeCost.csv", sep = ";", row.names = 1) %>%
      as.magpie()
  }

  return(out)
}
