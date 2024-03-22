#' Calculate REMIND variables from IEA World Energy Outlook data. TODO
#'
#' @param year 2021 (full paid date) or 2023 (free data).
#' @author Falk Benke
#' @importFrom dplyr select mutate left_join case_when
#' @importFrom madrat toolGetMapping
#' @importFrom magclass as.magpie
#' @importFrom stats aggregate
#' @export

calcIEA_WorldEnergyOutlook <- function(year) { # nolint

  mapping <- toolGetMapping("Mapping_IEA_WEO_2021_complete.csv",
    type = "reportingVariables", where = "mrremind"
  ) %>%
    filter(!is.na(.data$REMIND), .data$REMIND != "") %>%
    mutate(
      "WEO" := paste0(.data$WEO, " (", .data$Unit_WEO, ")"),
      "Conversion" := as.numeric(.data$Conversion)
    ) %>%
    select("variable" = "WEO", "REMIND", "Conversion", "unit" = "Unit_WEO", "Unit_REMIND")

  mapping$variable <- trimws(mapping$variable)

  dataGlo <- readSource("IEA_WorldEnergyOutlook", subtype = "2021-global")
  dataReg <- readSource("IEA_WorldEnergyOutlook", subtype = "2021-region")

  .mapToRemind <- function(data) {

    # copy over Stated Policies Scenario for 2010 - 2020 to other scenarios
    for (s in getNames(data, dim = 1)) {
      data[, c("y2010", "y2019", "y2020"), s] <-
        data[, c("y2010", "y2019", "y2020"), "Stated Policies Scenario"][, , getNames(data[, , s], dim = 2)]
    }

    data <- as.data.frame(data) %>%
      as_tibble() %>%
      select(
        "region" = "Region", "scenario" = "Data1", "variable" = "Data2",
        "year" = "Year", "value" = "Value"
      ) %>%
      mutate("scenario_short" = case_when( # nolint
        scenario == "Stated Policies Scenario" ~ "SPS",
        scenario == "Announced pledges scenario" ~ "APS",
        scenario == "Announced Pledges Scenario" ~ "APS",
        scenario == "Sustainable Development Scenario" ~ "SDS",
        scenario == "Net Zero Emissions by 2050 Scenario" ~ "Net2050"
      ))

    x <- left_join(data, mapping, by = "variable", relationship = "many-to-many") %>%
      filter(.data$REMIND != "") %>%
      mutate(
        "value" = .data$value * .data$Conversion,
        "REMIND" = paste0(.data$REMIND, " (", .data$Unit_REMIND, ")"),
        "model" = paste0("IEA WEO 2021 ", .data$scenario_short)
      ) %>%
      select("region", "year", "model", "variable" = "REMIND", "value")

    x <- aggregate(value ~ region + year + model + variable, x, sum) %>%
      as.magpie(spatial = 1, temporal = 2, data = 5) %>%
      toolCountryFill(fill = NA, verbosity = 2)

    return(x)

  }

  dataGlo <- .mapToRemind(dataGlo)
  dataReg <- .mapToRemind(dataReg)

  .calcAdditionalVars <- function(x, glo) {

    if (glo) {
      x <- add_columns(x, "Cap|Electricity|Biomass|w/o CC (GW)", dim = 3.2)
      x[, , "Cap|Electricity|Biomass|w/o CC (GW)"] <-
        x[, , "Cap|Electricity|Biomass (GW)"] - x[, , "Cap|Electricity|Biomass|w/ CC (GW)"]

      x <- add_columns(x, "Cap|Electricity|Coal (GW)", dim = 3.2)
      x[, , "Cap|Electricity|Coal (GW)"] <-
        x[, , "Cap|Electricity|Coal|w/o CC (GW)"] + x[, , "Cap|Electricity|Coal|w/ CC (GW)"]

      x <- add_columns(x, "Cap|Electricity|Solar (GW)", dim = 3.2)
      x[, , "Cap|Electricity|Solar (GW)"] <-
        x[, , "Cap|Electricity|Solar|CSP (GW)"] + x[, , "Cap|Electricity|Solar|PV (GW)"]

      x <- add_columns(x, "Cap|Electricity|Fossil (GW)", dim = 3.2)
      x[, , "Cap|Electricity|Fossil (GW)"] <-
        x[, , "Cap|Electricity|Fossil|w/o CC (GW)"] + x[, , "Cap|Electricity|Fossil|w/ CC (GW)"]

      x <- add_columns(x, "Cap|Electricity|Gas (GW)", dim = 3.2)
      x[, , "Cap|Electricity|Gas (GW)"] <-
        x[, , "Cap|Electricity|Gas|w/o CC (GW)"] + x[, , "Cap|Electricity|Gas|w/ CC (GW)"]

      x <- add_columns(x, "SE|Electricity|Solar (EJ/yr)", dim = 3.2)
      x[, , "SE|Electricity|Solar (EJ/yr)"] <-
        x[, , "SE|Electricity|Solar|PV (EJ/yr)"] + x[, , "SE|Electricity|Solar|CSP (EJ/yr)"]
    }

    # correct PE|Nuclear and PE
    # PE Nuclear is usually reported in direct equivalents, values from IEA are
    # roughly 3 times higher than the REMIND ones
    x[, , "PE (EJ/yr)"] <- x[, , "PE (EJ/yr)"] - x[, , "PE|Nuclear (EJ/yr)"]
    x[, , "PE|Nuclear (EJ/yr)"] <- x[, , "PE|Nuclear (EJ/yr)"] / 3
    x[, , "PE (EJ/yr)"] <- x[, , "PE (EJ/yr)"] + x[, , "PE|Nuclear (EJ/yr)"]

    return(x)
  }


  dataGlo <- .calcAdditionalVars(dataGlo, glo = TRUE)
  dataReg <- .calcAdditionalVars(dataReg, glo = FALSE)

  .customAggregate <- function(x, rel, to = NULL, glo) {
    x <- toolAggregate(x, rel = rel, to = to)

    if ("GLO" %in% getRegions(x)) {
      x <- x["GLO", , , invert = TRUE]

      glo <- dimSums(glo, dim = 1, na.rm = TRUE)

      out <- new.magpie(
        cells_and_regions = union(getItems(x, dim = 1), "GLO"),
        years = union(getYears(x), getYears(glo)),
        names = union(getNames(x), getNames(glo)),
        fill = NA,
        sets = names(dimnames(x))
      )

      out[getItems(x, dim = 1), getYears(x), getNames(x)] <- x
      out["GLO", getYears(glo), getNames(glo)] <- glo

      return(out)
    } else {
      return(x)
    }
  }

  return(list(
    x = dataReg,
    weight = NULL,
    unit = c("GW", "EJ/yr", "Mt CO2/yr"),
    aggregationFunction = .customAggregate,
    aggregationArguments = list(glo = dataGlo),
    description = "IEA World Energy Outlook values as REMIND variables"
  ))
}
