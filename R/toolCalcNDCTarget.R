#' Calculate NDC emission target levels from country target formulations
#'
#' @description Loads reference emissions, LULUCF projections and GDP data,
#' translates the different NDC target formulations (Percentage Reduction,
#' Absolute Reduction, Absolute, Intensity Reduction) into an absolute emission
#' level in the metric "Emi|GHG|w/o Bunkers|w/o Land-Use Change" (Mt CO2eq/yr),
#' and disaggregates the EU aggregate target to member states by GDP weight.
#'
#' @param targets a data frame with one row per country x target-year with columns:
#'   \describe{
#'     \item{iso3c}{ISO3 country code or "EUR" for the EU aggregate}
#'     \item{year}{integer target year}
#'     \item{type}{target type: "Percentage Reduction", "Absolute Reduction",
#'       "Absolute", or "Intensity Reduction"}
#'     \item{scope}{emission scope: "incl LULUCF" or "excl LULUCF"}
#'     \item{target_value}{numeric target value (fraction for Percentage/Intensity,
#'       Mt CO2eq for Absolute types; reductions encoded as negative numbers)}
#'     \item{reference_year}{integer year of the reference/BAU emissions}
#'     \item{reference_level}{explicit reference level in Mt CO2eq/yr, or NA to
#'       derive from historical data in the target's own scope}
#'   }
#' @return a data frame with columns \code{iso3c}, \code{year}, and
#'   \code{Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)}, with the EU
#'   aggregate disaggregated to individual member states
#' @author Felix Schreyer, Rahel Mandaroux
#' @seealso [convertPBL_NDC()], [readPBL_NDC()]
#' @importFrom dplyr filter select mutate left_join rename bind_rows pull if_else coalesce group_by ungroup case_when
toolCalcNDCTarget <- function(targets) {
  inclName <- "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)"

  # 1. Load reference emissions, LULUCF projections, and GDP data ----
  emiRef <- calcOutput("EmiTargetReference", aggregate = FALSE)
  refIncl <- quitte::as.quitte(emiRef[, , inclName]) %>%
    select("iso3c" = "region", "refYear" = "period", "refIncl" = "value") %>%
    mutate("refYear" = as.integer(as.character(.data$refYear)))
  refExcl <- quitte::as.quitte(emiRef[, , "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"]) %>%
    select("iso3c" = "region", "refYear" = "period", "refExcl" = "value") %>%
    mutate("refYear" = as.integer(as.character(.data$refYear)))

  lulucf2030 <- readSource("IIASALanduse", subtype = "forecast2030")
  lulucf2035 <- readSource("IIASALanduse", subtype = "forecast2035")
  # India 2035 was added manually and is missing from the LULUCF source; reuse 2030 value
  lulucf2035["IND", "y2035", ] <- setYears(lulucf2030["IND", "y2030", ], "y2035")
  lulucf <- quitte::as.quitte(mbind(lulucf2030, lulucf2035), na.rm = FALSE) %>%
    select("iso3c" = "region", "year" = "period", "lulucf" = "value") %>%
    mutate("year" = as.integer(as.character(.data$year)))

  gdp <- collapseDim(calcOutput("GDP", scenario = "SSP2", aggregate = FALSE))
  gdpDf <- quitte::as.quitte(gdp) %>%
    select("iso3c" = "region", "gdpYear" = "period", "gdp" = "value") %>%
    mutate("gdpYear" = as.integer(as.character(.data$gdpYear)))

  # 2. EU reference level: summed 1990 emissions (incl. LULUCF) across REMIND EUR region ----
  regionmapping <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")
  eurCountries <- regionmapping %>%
    filter(.data$RegionCode == "EUR") %>%
    pull(.data$CountryCode)
  euReference1990 <- sum(emiRef[eurCountries, 1990, inclName], na.rm = TRUE)

  # 3. Add reference levels, LULUCF in-scope values, and GDP ratios to target data ----
  #
  # Variables assembled per country x target-year:
  #
  #   reference_level  Emission level in the target's own scope (incl. or excl. LULUCF).
  #                    Taken from the NDC source sheet when provided; otherwise filled from
  #                    historical UNFCCC/CEDS data for the stated reference year and scope.
  #
  #   lulucf_inscope   Target-year LULUCF emissions when scope = "incl LULUCF", else 0.
  #                    Subtracted so the output is consistently expressed excl. LULUCF.
  #
  #   gdp_ratio        GDP(target year) / GDP(reference year) for Intensity Reduction
  #                    targets; 1 for all other types.
  CountryTargetData <- targets %>%
    filter(.data$iso3c != "EUR") %>%
    left_join(refIncl, by = c("iso3c", "reference_year" = "refYear")) %>%
    left_join(refExcl, by = c("iso3c", "reference_year" = "refYear")) %>%
    mutate(
      "reference_level" = coalesce(
        .data$reference_level,
        if_else(.data$scope == "incl LULUCF", .data$refIncl, .data$refExcl)
      )
    ) %>%
    left_join(lulucf, by = c("iso3c", "year")) %>%
    mutate("lulucf_inscope" = if_else(.data$scope == "incl LULUCF", coalesce(.data$lulucf, 0), 0)) %>%
    mutate("refYearRounded" = round(.data$reference_year / 5) * 5) %>%
    left_join(gdpDf, by = c("iso3c", "year" = "gdpYear")) %>%
    rename("gdpTarget" = "gdp") %>%
    left_join(gdpDf, by = c("iso3c", "refYearRounded" = "gdpYear")) %>%
    rename("gdpRef" = "gdp") %>%
    mutate("gdp_ratio" = if_else(.data$type == "Intensity Reduction", .data$gdpTarget / .data$gdpRef, 1))

  # EU: fixed 1990 reference (incl. LULUCF) and official -310 Mt CO2eq/yr LULUCF sink
  EUTargetData <- targets %>%
    filter(.data$iso3c == "EUR") %>%
    mutate("reference_level" = euReference1990, "lulucf_inscope" = -310, "gdp_ratio" = 1)

  TargetData <- bind_rows(CountryTargetData, EUTargetData)

  # 4. Apply the target-type-specific formula to get absolute emission levels ----
  TargetsCalculated <- TargetData %>%
    mutate(
      "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)" = case_when(
        # Absolute: target value is the emission level itself
        .data$type == "Absolute" ~ .data$target_value - .data$lulucf_inscope,
        # Absolute Reduction: reduction (negative) added to reference level
        .data$type == "Absolute Reduction" ~ .data$reference_level + .data$target_value - .data$lulucf_inscope,
        # Percentage Reduction: fractional reduction applied to reference level
        .data$type == "Percentage Reduction" ~ .data$reference_level * (1 + .data$target_value) - .data$lulucf_inscope,
        # Intensity Reduction: reference intensity scaled with GDP growth
        # Note: The basic formula is: 
        # Emi_targetyear / GDP_targetyear = Emi_referenceyear / GDP_referenceyear * (1 + target_value)
        # Below, it is solved for Emi_targetyear, and the LULUCF in-scope value is subtracted 
        # to get the output in the excl. LULUCF metric.
        .data$type == "Intensity Reduction" ~ .data$reference_level * (1 + .data$target_value) *
          .data$gdp_ratio - .data$lulucf_inscope
      )
    ) %>%
    select("iso3c", "year", "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)")

  # 5. Disaggregate EUR aggregate to member states by GDP weight ----
  euYears <- TargetsCalculated %>%
    filter(.data$iso3c == "EUR") %>%
    pull(.data$year) %>%
    unique()

  if (length(euYears) > 0) {
    gdpEurWeights <- gdpDf %>%
      filter(.data$iso3c %in% eurCountries, .data$gdpYear %in% euYears) %>%
      group_by(.data$gdpYear) %>%
      mutate("weight" = .data$gdp / sum(.data$gdp, na.rm = TRUE)) %>%
      ungroup() %>%
      rename("memberState" = "iso3c", "year" = "gdpYear") %>%
      select("memberState", "year", "weight")

    euDisaggregated <- TargetsCalculated %>%
      filter(.data$iso3c == "EUR") %>%
      left_join(gdpEurWeights, by = "year") %>%
      mutate(
        "iso3c" = .data$memberState,
        "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)" =
          .data$`Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)` * .data$weight
      ) %>%
      select("iso3c", "year", "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)")

    TargetsCalculated <- bind_rows(filter(TargetsCalculated, .data$iso3c != "EUR"), euDisaggregated)
  }

  return(TargetsCalculated)
}
