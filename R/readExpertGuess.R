#' Read ExpertGuess
#'
#' Read-in data that are based on expert guess
#'
#' @md
#' @param subtype Type of data that should be read. One of
#'   - `capacityFactorGlobal`: Global capacity factors for all REMIND technologies
#'   - `capacityFactorRules`: Capacity factor rules for selected H12 regions and REMIND technologies
#'   - `subConvergenceRollback`: Subsidy convergence level in rollback scenario
#'   - `taxConvergence`: Tax convergence level in US$2005
#'   - `taxConvergenceRollback`: Tax convergence level in rollback scenario
#'   - `costsTradePeFinancial`
#'   - `CCSbounds`
#'   - `Steel_Production`: Steel production estimates
#'   - `industry_max_secondary_steel_share`: Maximum share of secondary steel
#'     production in total steel production and years between which a linear
#'     convergence from historic to target shares is to be applied.
#'   - `cement_production_convergence_parameters`: convergence year and level
#'     (relative to global average) to which per-capita cement demand converges
#'   - `ies`
#'   - `prtp`
#'   - `tradeContsraints`: parameter by Nicolas Bauer (2024) for the region
#'      specific trade constraints, values different to 1 activate constraints
#'      and the value is used as effectiveness to varying degrees such as percentage numbers
#' @return magpie object of the data
#' @author Lavinia Baumstark
#' @examples
#' \dontrun{
#' a <- readSource(type = "ExpertGuess", subtype = "ies")
#' }
#'
#' @importFrom dplyr bind_rows filter pull select
#'
readExpertGuess <- function(subtype) {
  a <- switch(subtype,
    "ies"                   = read.csv("ies.csv", sep = ";"),
    "prtp"                  = read.csv("prtp.csv", sep = ";"),
    "CCSbounds"             = read.csv("CCSbounds.csv", sep = ";"),
    "co2prices"             = read.csv("co2prices-2024-11.csv", sep = ";"),
    "costsTradePeFinancial" = read.csv("pm_costsTradePeFinancial_v1.1.csv", sep = ";", skip = 2),
    "tradeConstraints"      = read.csv("tradeConstraints.csv", sep = ";")
  )

  if (subtype %in% c("ies", "prtp", "CCSbounds", "co2prices")) {
    a$RegionCode <- NULL
    a$Country <- NULL
    out <- as.magpie(a)
    out[is.na(out)] <- 0
  }

  if (subtype %in% c("ies", "prtp")) {
    getYears(out) <- "2005"
  }

  if (subtype == "costsTradePeFinancial") {
    out <- as.magpie(a, spatial = 1, temporal = 0, datacol = 3)
    out <- collapseNames(out)
  }

  if (subtype == "Steel_Production") {
    out <- readr::read_csv(
      file = "Steel_Production.csv",
      comment = "#",
      show_col_types = FALSE
    ) %>%
      quitte::madrat_mule()
  }

  if (subtype == "industry_max_secondary_steel_share") {
    out <- readr::read_csv(
      file = "industry_max_secondary_steel_share.csv",
      comment = "#",
      show_col_types = FALSE
    ) %>%
      quitte::madrat_mule()
  }

  if (subtype == "cement_production_convergence_parameters") {
    out <- readr::read_csv(
      file = "cement_production_convergence_parameters.csv",
      col_types = "cdi",
      comment = "#"
    )

    out <- bind_rows(
      out %>%
        filter(!is.na(.data$region)),
      out %>%
        utils::head(n = 1) %>%
        filter(is.na(.data$region)) %>%
        select(-"region") %>%
        tidyr::expand_grid(region = toolGetMapping(
          name = "regionmapping_21_EU11.csv",
          type = "regional", where = "mappingfolder"
        ) %>%
          pull("RegionCode") %>%
          unique() %>%
          sort() %>%
          setdiff(out$region))
    ) %>%
      quitte::madrat_mule()
  }

  if (subtype == "tradeConstraints") {
    out <- as.magpie(a)
  }

  if (subtype == "capacityFactorGlobal") {

    out <- read.csv("capacity-factors-global_REMIND_3.4.0.csv", sep = ";") %>%
      as.magpie(datacol = 2)

  } else if (subtype == "capacityFactorRules") {

    out <- read.csv("capacity-factor-rules_v1.0.csv", sep = ";") %>%
      as.magpie(datacol = 4)

  } else if (subtype == "subConvergenceRollback") {

    out <- read.csv("sub_convergence_rollback_v1.0.csv", sep = ";", col.names = c("Year", "Region", "sector", "FE", "value")) %>%
      as.magpie(datacol = 5)

  } else if (subtype == "taxConvergence") {

    out <- read.csv("tax_convergence_v1.0.csv", sep = ";") %>%
      as.magpie(datacol = 4)

  } else if (subtype == "taxConvergenceRollback") {

    out <- read.csv("tax_convergence_rollback_v1.0.csv", sep = ";", col.names = c("Year", "Region", "FE", "value")) %>%
      as.magpie(datacol = 4)
  }

  return(out)
}
