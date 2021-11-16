#' Calculate REMIND variables from IEA WEO 2021 data
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#' @importFrom dplyr select mutate left_join
#' @importFrom madrat toolGetMapping
#' @importFrom magclass as.magpie
#' @importFrom rlang sym
#' @export


calcIEA_WEO_2021 <- function() {
  mapping <- toolGetMapping("Mapping_IEA_WEO_2021.csv", type = "reportingVariables") %>%
    filter(!is.na(!!sym("REMIND")), !!sym("REMIND") != "") %>%
    mutate(!!sym("WEO") := paste0(!!sym("WEO"), " (", !!sym("Unit_WEO"), ")")) %>%
    select("variable" = "WEO", "REMIND", "Conversion", "unit" = "Unit_WEO", "Unit_REMIND")

  mapping$variable <- trimws(mapping$variable)

  data <- readSource("IEA_WEO_2021")

  data <- as.data.frame(data) %>%
    as_tibble() %>%
    select(
      "region" = "Region", "scenario" = "Data1", "variable" = "Data2",
      "year" = "Year", "value" = "Value"
    )

  x <- left_join(
    data,
    mapping,
    by = "variable"
  ) %>%
    filter(!!sym("REMIND") != "") %>%
    mutate(
      !!sym("value") := ifelse(
        is.na(!!sym("value")), 0, !!sym("value") * !!sym("Conversion")
      ),
      !!sym("REMIND") := paste0(!!sym("REMIND"), " (", !!sym("Unit_REMIND"), ")"),
      !!sym("model") := paste0("IEA WEO 2020 ", !!sym("scenario"))
    ) %>%
    select("region", "year", "model", "variable" = "REMIND", "value")

  x <- as.magpie(x, spatial = 1, temporal = 2, data = 5)

  x <- add_columns(x, "Cap|Electricity|Biomass|w/o CCS (GW)", dim = 3.2)
  x[, , "Cap|Electricity|Biomass|w/o CCS (GW)"] <- x[, , "Cap|Electricity|Biomass (GW)"] - x[, , "Cap|Electricity|Biomass|w/ CCS (GW)"]

  x <- add_columns(x, "Cap|Electricity|Coal (GW)", dim = 3.2)
  x[, , "Cap|Electricity|Coal (GW)"] <- x[, , "Cap|Electricity|Coal|w/o CCS (GW)"] + x[, , "Cap|Electricity|Coal|w/ CCS (GW)"]

  x <- add_columns(x, "Cap|Electricity|Solar (GW)", dim = 3.2)
  x[, , "Cap|Electricity|Solar (GW)"] <- x[, , "Cap|Electricity|Solar|CSP (GW)"] + x[, , "Cap|Electricity|Solar|PV (GW)"]

  x <- add_columns(x, "Cap|Electricity|Fossil (GW)", dim = 3.2)
  x[, , "Cap|Electricity|Fossil (GW)"] <- x[, , "Cap|Electricity|Fossil|w/o CCS (GW)"] + x[, , "Cap|Electricity|Fossil|w/ CCS (GW)"]

  x <- add_columns(x, "Cap|Electricity|Gas (GW)", dim = 3.2)
  x[, , "Cap|Electricity|Gas (GW)"] <- x[, , "Cap|Electricity|Gas|w/o CCS (GW)"] + x[, , "Cap|Electricity|Gas|w/ CCS (GW)"]

  return(list(
    x = x,
    unit = c("GW", "EJ/yr", "Mt CO2/yr"),
    description = "IEA WEO 2021 values as REMIND variables"
  ))
}
