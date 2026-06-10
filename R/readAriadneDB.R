#' Ariadne database scenario data
#' @description  This reads in FORECAST industry production data for Germany used in the Ariadne scenarios
#'
#' @return A [`magpie`][magclass::magclass] object.
#' @author Felix Schreyer
#' @importFrom dplyr filter mutate
#' @importFrom tidyr gather
#' @importFrom quitte getVars as.quitte
#'
readAriadneDB <- function() {
  # read in FORECAST v5 file (sheet "Sheet1")
  data_file <- "FORECAST_Ariadne_v5_Szenarien.xlsx"

  # read source file and convert to quitte format
  data <- readxl::read_excel(
    data_file,
    sheet = "Sheet1"
  ) %>%
    gather("period", "value", -"Model", -"Scenario", -"Region", -"Variable", -"Unit") %>%
    as.quitte()

  # only retain scenario and variable variation
  # only use production and gross value added variables
  # convert to magpie object
  out <- data %>%
    filter(.data$variable %in% c(
      grep("Production\\|", getVars(data), value = T),
      grep("Gross Value Added\\|", getVars(data), value = T)
    )) %>%
    select("period", "region", "scenario", "variable", "value") %>%
    as.data.frame() %>%
    as.magpie(temporal = 1, spatial = 2, datacol = 5)


  return(out)
}
