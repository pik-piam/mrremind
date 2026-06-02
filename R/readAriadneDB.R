#' Ariadne database scenario data
#' @description  Scenario data from the Ariadne modeling intercomparison project for Germany.
#' See README in input file for more details.
#'
#' @return A [`magpie`][magclass::magclass] object.
#' @author Felix Schreyer
#' @importFrom dplyr filter mutate
#'
readAriadneDB <- function() {

  # read in FORECAST v5 file (sheet "Sheet1")
  data_file <- file.path(
    "C:/work/scripts/inputdata/sources/AriadneDB",
    "FORECAST_Ariadne_v5_Szenarien.xlsx"
  )

  data <- readxl::read_excel(
    data_file,
    sheet = "Sheet1",
    col_types = "text",
    .name_repair = "minimal"
  ) %>%
    # try to coerce numeric columns (periods) to numeric if they exist
    readr::type_convert() %>%
    # normalize column names to lowercase to match expected names (model, scenario, region, variable, unit)
    dplyr::rename_with(tolower)

  # rearrange and convert to magclass object
  out <- data %>%
    tidyr::gather("period", "value", -"model", -"scenario", -"region", -"variable", -"unit") %>%
    dplyr::filter(!is.na(.data$value)) %>%
    # ensure period is numeric year (remove leading y if present)
    dplyr::mutate( period = as.integer(stringr::str_replace(.data$period, "^y", ""))) %>%
    as.magpie(temporal = 6, spatial = 3, datacol = 7)

  return(out)
}
