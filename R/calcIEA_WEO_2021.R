#' Calculate REMIND variables from IEA WEO 2021 data
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#' @param subtype Either "global" or "region". On global level, the source offers
#' more variables than on regional level, but the data should not be used on sub-
#' global level due to its coarse disaggregation.
#' @param isValidation indicates if result will be used in validation (as opposed to generating input data)
#' @author Falk Benke
#' @importFrom dplyr select mutate left_join case_when
#' @importFrom madrat toolGetMapping
#' @importFrom magclass as.magpie
#' @importFrom rlang sym
#' @importFrom stats aggregate
#' @export

calcIEA_WEO_2021 <- function(subtype, isValidation = FALSE) { # nolint

  mapping <- toolGetMapping("Mapping_IEA_WEO_2021_complete.csv", type = "reportingVariables", where = "mappingfolder") %>%
    filter(!is.na(!!sym("REMIND")), !!sym("REMIND") != "") %>%
    mutate(
      !!sym("WEO") := paste0(!!sym("WEO"), " (", !!sym("Unit_WEO"), ")"), # nolint
      !!sym("Conversion") := as.numeric(!!sym("Conversion")) # nolint
    ) %>%
    select("variable" = "WEO", "REMIND", "Conversion", "unit" = "Unit_WEO", "Unit_REMIND")

  mapping$variable <- trimws(mapping$variable)

  data <- readSource("IEA_WEO_2021", subtype = subtype)

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
    mutate(!!sym("scenario_short") := case_when( # nolint
      scenario == "Stated Policies Scenario" ~ "SPS",
      scenario == "Announced pledges scenario" ~ "APS",
      scenario == "Announced Pledges Scenario" ~ "APS",
      scenario == "Sustainable Development Scenario" ~ "SDS",
      scenario == "Net Zero Emissions by 2050 Scenario" ~ "Net2050"
    ))

  x <- left_join(
    data,
    mapping,
    by = "variable"
  ) %>%
    filter(!!sym("REMIND") != "") %>%
    mutate(
      !!sym("value") := !!sym("value") * !!sym("Conversion"),
      !!sym("REMIND") := paste0(!!sym("REMIND"), " (", !!sym("Unit_REMIND"), ")"), # nolint
      !!sym("model") := paste0("IEA WEO 2021 ", !!sym("scenario_short")) # nolint
    ) %>%
    select("region", "year", "model", "variable" = "REMIND", "value")

  x <- aggregate(value ~ region + year + model + variable, x, sum) %>%
    as.magpie(spatial = 1, temporal = 2, data = 5) %>%
    toolCountryFill(fill = NA)

  if (subtype == "global") {
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

  if (isValidation) {
    x <- add_dimension(x, dim = 3.1, add = "scenario", nm = "historical")
  }

  return(list(
    x = x,
    weight = NULL,
    unit = c("GW", "EJ/yr", "Mt CO2/yr"),
    description = "IEA WEO 2021 values as REMIND variables"
  ))
}
