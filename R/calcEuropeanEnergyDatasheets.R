#' Calculate REMIND variables from European Energy Datasheets
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#' @param subtype data subtype. Either "EU28" (older data from Jun 2020, including GBR) or "EU27" (data from Jun 2021)
#'
#' @author Falk Benke
#'
#' @importFrom dplyr select mutate left_join
#' @importFrom madrat toolGetMapping
#' @importFrom magclass as.magpie
#' @importFrom rlang sym
#' @importFrom stats aggregate na.exclude

calcEuropeanEnergyDatasheets <- function(subtype) {
  if (!subtype %in% c("EU27", "EU28")) {
    stop("Invalid subtype. Must be either EU27 or EU28")
  }

  if (subtype == "EU27") {
    data <- readSource("EuropeanEnergyDatasheets", subtype = "EU27")

    data <- as.data.frame(data) %>%
      as_tibble() %>%
      select(
        "region" = "Region", "variable" = "Data1",
        "year" = "Year", "value" = "Value"
      )

    mapping <- toolGetMapping("Mapping_EuropeanEnergyDatasheets.csv", type = "reportingVariables", where = "mappingfolder") %>%
      filter(!is.na(!!sym("REMIND")), !!sym("REMIND") != "") %>%
      mutate(!!sym("Conversion") := as.numeric(!!sym("Conversion"))) %>%
      select("variable" = "EED", "REMIND", "Conversion")

    mapping$variable <- trimws(mapping$variable)
    mapping$REMIND <- trimws(mapping$REMIND)

    x <- left_join(
      data,
      mapping,
      by = "variable"
    ) %>%
      filter(!!sym("REMIND") != "") %>%
      mutate(
        !!sym("value") := !!sym("value") * !!sym("Conversion")
      ) %>%
      select("region", "year", "variable" = "REMIND", "value")

    x <- aggregate(value ~ region + year + variable, x, sum, na.action = na.exclude) %>%
      as.magpie() %>%
      toolCountryFill(fill = NA, verbosity = 2)

    # get GBR data from older versions of the source, as it is no longer updated
    eurostat.gbr <- readSource("EuropeanEnergyDatasheets", subtype = "EU28")["GBR", , ]
    x["GBR", getItems(eurostat.gbr, dim = 2), ] <- eurostat.gbr
  } else {
    x <- readSource("EuropeanEnergyDatasheets", subtype = "EU28")
  }
  return(list(
    x = x,
    weight = NULL,
    unit = c("EJ/yr", "Mt CO2/yr", "Mt CO2eq/yr", "GW", "million"),
    description = "European Energy Datasheets data in REMIND variables"
  ))
}
