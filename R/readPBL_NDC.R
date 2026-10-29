#' Read NDC emission targets from the PBL ELEVATE scenario protocol
#'
#' @description Reads NDC (Nationally Determined Contributions) emission target data
#' collected by PBL in the ELEVATE scenario protocol. Two subtypes are available:
#' - `detailed`: country-specific target formulations for major emitters (sheet
#'   "NDC details major emitters"). Targets are classified by target type and emission
#'   scope. The translation into absolute emission levels happens in [convertPBL_NDC()].
#' - `reduced`: pre-computed 2030 and 2035 emission levels for all other countries
#'   (sheet "NDC emission levels"), taken in the "excl LULUCF" metric.
#'
#' @param subtype one of "detailed" or "reduced"
#' @author Felix Schreyer, Rahel Mandaroux
#' @seealso [convertPBL_NDC()], [toolCalcNDCTarget()]
#' @importFrom dplyr rename filter mutate case_when if_else coalesce arrange distinct select bind_rows
#' @importFrom tidyr pivot_longer
readPBL_NDC <- function(subtype) { # nolint: object_name_linter.
  PBLfile <- "ELEVATE T6.3 Scenario Protocol NDC and LTS information v3.xlsx"

  if (subtype == "detailed") {
    # 1. Read detailed target formulations of major emitters ----
    raw <- readxl::read_excel(PBLfile, sheet = "NDC details major emitters", progress = FALSE) %>%
      suppressMessages() %>%
      rename(
        "iso3c" = "ISO-3",
        "year" = "Target Year",
        "conditionality" = "Conditionality",
        "originalIndicator" = "Original Target Indicator",
        "modelIndicator" = "Model Target Indicator",
        "valueMin" = "Target Value Min",
        "valueMax" = "Target Value Max",
        "targetUnit" = "Target Unit",
        "targetType" = "Target type",
        "reference" = "Reference",
        "referenceLevel" = "Reference level"
      )

    # keep only the emission targets (drop capacity, renewable share, forest etc. targets)
    emissionIndicators <- c(
      "GHG emissions (excl LULUCF)",
      "GHG emissions (incl LULUCF)",
      "CO2 emissions intensity (tCO2/GDP) (incl LULUCF)",
      "Emissions intensity (CO2e/GDP) (incl LULUCF)"
    )

    # 2. Classify target type, emission scope and select the conditional target value ----

    # There are 4 target types:
    # 1. Absolute Target -> target value is the absolute level of emissions in MtCO2eq/yr
    # 2. Absolute Reduction -> target value is the absolute reduction in MtCO2eq/yr
    # 3. Percentage Reduction -> target value is the percentage reduction relative to a reference year
    # 4. Intensity Reduction -> target value is the percentage reduction in emissions intensity of GDP relative to a reference year

    # There are 2 emissions scopes supported:
    # 1. incl LULUCF -> target on total GHG including LULUCF
    # 2. excl LULUCF -> target on total GHG excluding LULUCF

    # Currently only conditional NDC targets are supported.

    prepared <- raw %>%
      filter(.data$originalIndicator %in% emissionIndicators) %>%
      mutate(
        "unit" = trimws(.data$targetUnit),
        # target type following the four NDC target formulations
        "type" = case_when(
          .data$unit == "%" & grepl("intensity", .data$modelIndicator, ignore.case = TRUE) ~ "Intensity Reduction",
          .data$unit == "%" ~ "Percentage Reduction",
          .data$targetType == "Target level" ~ "Absolute",
          .data$targetType == "Reduction level" ~ "Absolute Reduction"
        ),
        # emission scope relative to which the target is formulated
        "scope" = if_else(grepl("excl LULUCF", .data$originalIndicator), "excl LULUCF", "incl LULUCF"),
        # if a target range is given, use the most ambitious value
        # (lowest for reductions, highest for absolute levels)
        "target_value" = if_else(
          .data$targetType == "Target level",
          coalesce(.data$valueMin, .data$valueMax),
          coalesce(.data$valueMax, .data$valueMin)
        ) * case_when(.data$unit == "%" ~ 1 / 100, grepl("Gt", .data$unit) ~ 1000, TRUE ~ 1),
        "reference_level" = suppressWarnings(as.numeric(.data$referenceLevel)),
        "reference_year" = suppressWarnings(as.numeric(.data$reference)),
        # prefer explicit conditional targets over unconditional ones
        "priority" = if_else(.data$conditionality == "Conditional", 1, 2)
      ) %>%
      # one target per country and year (the conditional row if available)
      arrange(.data$iso3c, .data$year, .data$priority) %>%
      distinct(.data$iso3c, .data$year, .keep_all = TRUE) %>%
      select("iso3c", "year", "type", "scope", "target_value", "reference_level", "reference_year")

    # 3. Manual corrections to emission scope and reference assumptions ----
    prepared <- prepared %>%
      mutate(
        # China 2030 CO2 intensity target is reported relative to emissions excl. LULUCF
        "scope" = if_else(.data$iso3c == "CHN" & .data$year == 2030, "excl LULUCF", .data$scope),
        # China 2035 target is relative to peak-year emissions; assume 15500 Mt CO2eq incl. LULUCF
        # (2025 peak of ~16439 Mt excl. LULUCF minus ~1 Gt LULUCF sink)
        "scope" = if_else(.data$iso3c == "CHN" & .data$year == 2035, "incl LULUCF", .data$scope),
        "reference_level" = if_else(.data$iso3c == "CHN" & .data$year == 2035, 15500, .data$reference_level),
        # Saudi Arabia reduction is relative to 2019 emissions, not BAU
        "reference_year" = if_else(.data$iso3c == "SAU", 2019, .data$reference_year),
        # India emission intensity targets are interpreted as excl. LULUCF
        "scope" = if_else(.data$iso3c == "IND", "excl LULUCF", .data$scope)
      )

    # add the announced India 2035 target of -47% GHG intensity (not yet in the PBL sheet)
    # https://www.climatechangenews.com/2026/03/25/india-sets-achievable-green-electricity-and-emissions-instensity-targets/
    india2035 <- prepared %>%
      filter(.data$iso3c == "IND", .data$year == 2030) %>%
      mutate("year" = 2035, "target_value" = -0.47)
    prepared <- bind_rows(prepared, india2035)

    # 4. Return classified target formulations as magclass object ----
    # the target type and scope labels are kept as name dimensions of the magclass;
    out <- prepared %>%
      quitte::revalue.levels(iso3c = c("EU" = "EUR")) %>%
      pivot_longer(
        cols = c("target_value", "reference_level", "reference_year"),
        names_to = "variable", values_to = "value"
      ) %>%
      as.magpie(spatial = "iso3c", temporal = "year")

    return(out)
  } else if (subtype == "reduced") {
    # 1. Read pre-computed emission levels (excl LULUCF) for all countries ----
    reduced <- readxl::read_excel(PBLfile, sheet = "NDC emission levels", skip = 2, progress = FALSE) %>%
      select(
        "iso3c" = .data$...2,
        "y2030" = .data$`excl LULUCF...5`,
        "y2035" = .data$`excl LULUCF...7`
      ) %>%
      filter(!is.na(.data$iso3c)) %>%
      # one row per country and target year
      pivot_longer(
        cols = c("y2030", "y2035"),
        names_to = "year",
        values_to = "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"
      ) %>%
      mutate("year" = as.integer(sub("y", "", .data$year))) %>%
      filter(!is.na(.data$`Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)`)) %>%
      suppressMessages()

    out <- as.magpie(reduced, spatial = "iso3c", temporal = "year")
    return(out)
  } else {
    stop("Invalid subtype for readPBL_NDC, please use 'detailed' or 'reduced'.")
  }
}
