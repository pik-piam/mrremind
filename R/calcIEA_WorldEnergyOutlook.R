#' Calculate REMIND variables from IEA World Energy Outlook data.
#'
#' @param version of the WEO data, either 'default' (full paid version) or
#' 'latest' (free, restricted, up-to-date dataset)
#' @author Falk Benke
#' @importFrom dplyr select mutate left_join case_when
#' @importFrom stats aggregate
#' @export

calcIEA_WorldEnergyOutlook <- function(version = "latest") { # nolint

  if (!(version %in% c("default", "latest"))) {
    stop("Invalid parameter `version`. Must be either 'default' or 'latest'")
  }

  refYear <- if (version == "default") 2021 else 2023

  dataGlo <- readSource("IEA_WorldEnergyOutlook", subtype = paste0(refYear, "-global"))
  dataReg <- readSource("IEA_WorldEnergyOutlook", subtype = paste0(refYear, "-region"))

  .mapToRemind <- function(data) {

    if (refYear == 2021) {
      # copy over Stated Policies Scenario for 2010 - 2020 to other scenarios
      for (s in getNames(data, dim = 1)) {
        data[, c("y2010", "y2019", "y2020"), s] <-
          data[, c("y2010", "y2019", "y2020"), "Stated Policies Scenario"][, , getNames(data[, , s], dim = 2)]
      }
    }

    data <- as.data.frame(data) %>%
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

    mapping <- toolGetMapping(
      "Mapping_IEA_WEO_2021_complete.csv",
      type = "reportingVariables", where = "mrremind"
    ) %>%
      filter(!is.na(.data$REMIND), .data$REMIND != "") %>%
      mutate(
        "from" := paste0(trimws(.data$WEO), " (", .data$Unit_WEO, ")"),
        "to" := paste0(trimws(.data$REMIND), " (", .data$Unit_REMIND, ")"),
        "conversion" := as.numeric(.data$Conversion)
      ) %>%
      select("from", "to", "conversion")

    x <- left_join(data, mapping, by = c("variable" = "from"), relationship = "many-to-many") %>%
      filter(.data$to != "") %>%
      mutate(
        "value" = .data$value * .data$conversion,
        "model" = paste0("IEA WEO ", refYear, " ", .data$scenario_short)
      ) %>%
      select("region", "year", "model", "variable" = "to", "value")

    x <- aggregate(value ~ region + year + model + variable, x, sum) %>%
      as.magpie(spatial = 1, temporal = 2, data = 5) %>%
      toolCountryFill(fill = NA, verbosity = 2)

    return(x)

  }

  dataGlo <- .mapToRemind(dataGlo)
  dataReg <- .mapToRemind(dataReg)


  # do additional calculations on full dataset for 2021 (won't work for incomplete 2023 data)
  if (refYear == 2021) {

    .calcAdditionalVars <- function(x) {

      # correct PE|Nuclear and PE
      # PE Nuclear is usually reported in direct equivalents, values from IEA are
      # roughly 3 times higher than the REMIND ones
      x[, , "PE (EJ/yr)"] <- x[, , "PE (EJ/yr)"] - x[, , "PE|Nuclear (EJ/yr)"]
      x[, , "PE|Nuclear (EJ/yr)"] <- x[, , "PE|Nuclear (EJ/yr)"] / 3
      x[, , "PE (EJ/yr)"] <- x[, , "PE (EJ/yr)"] + x[, , "PE|Nuclear (EJ/yr)"]

      return(x)
    }

    dataGlo <- .calcAdditionalVars(dataGlo)
    dataReg <- .calcAdditionalVars(dataReg)

    dataGlo <- add_columns(dataGlo, "Cap|Electricity|Biomass|w/o CC (GW)", dim = 3.2)
    dataGlo[, , "Cap|Electricity|Biomass|w/o CC (GW)"] <-
      dataGlo[, , "Cap|Electricity|Biomass (GW)"] - dataGlo[, , "Cap|Electricity|Biomass|w/ CC (GW)"]

    dataGlo <- add_columns(dataGlo, "Cap|Electricity|Coal (GW)", dim = 3.2)
    dataGlo[, , "Cap|Electricity|Coal (GW)"] <-
      dataGlo[, , "Cap|Electricity|Coal|w/o CC (GW)"] + dataGlo[, , "Cap|Electricity|Coal|w/ CC (GW)"]

    dataGlo <- add_columns(dataGlo, "Cap|Electricity|Solar (GW)", dim = 3.2)
    dataGlo[, , "Cap|Electricity|Solar (GW)"] <-
      dataGlo[, , "Cap|Electricity|Solar|CSP (GW)"] + dataGlo[, , "Cap|Electricity|Solar|PV (GW)"]

    dataGlo <- add_columns(dataGlo, "Cap|Electricity|Fossil (GW)", dim = 3.2)
    dataGlo[, , "Cap|Electricity|Fossil (GW)"] <-
      dataGlo[, , "Cap|Electricity|Fossil|w/o CC (GW)"] + dataGlo[, , "Cap|Electricity|Fossil|w/ CC (GW)"]

    dataGlo <- add_columns(dataGlo, "Cap|Electricity|Gas (GW)", dim = 3.2)
    dataGlo[, , "Cap|Electricity|Gas (GW)"] <-
      dataGlo[, , "Cap|Electricity|Gas|w/o CC (GW)"] + dataGlo[, , "Cap|Electricity|Gas|w/ CC (GW)"]

    dataGlo <- add_columns(dataGlo, "SE|Electricity|Solar (EJ/yr)", dim = 3.2)
    dataGlo[, , "SE|Electricity|Solar (EJ/yr)"] <-
      dataGlo[, , "SE|Electricity|Solar|PV (EJ/yr)"] + dataGlo[, , "SE|Electricity|Solar|CSP (EJ/yr)"]

  }

  # includes values from the original source for global region instead of calculating
  # it as the sum of all regions (as regions are incomplete)
  .customAggregate <- function(x, rel, to = NULL, glo) {
    x <- toolAggregate(x, rel = rel, to = to)

    if ("GLO" %in% getItems(x, dim = 1)) {
      x <- x["GLO", , , invert = TRUE]

      glo <- dimSums(glo, dim = 1, na.rm = FALSE)

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
