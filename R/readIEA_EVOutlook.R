#' Read IEA EV Outlook
#'
#' @author Falk Benke
readIEA_EVOutlook <- function() {

  data <- readxl::read_xlsx(
    path = file.path("data", "2025", "EV Data Explorer 2025.xlsx"),
    sheet = "GEVO_EV_2025"
  )

  data <- data %>%
    mutate(
      "variable" = sub("\\|NA", "", paste0(.data$parameter, "|", .data$mode, "|", .data$powertrain)),
      "value" = as.numeric(.data$value),
      "unit" = ifelse(is.na(.data$unit), "no", .data$unit)
    ) %>%
    select("region" = "region_country", "year", "scenario" = "category", "variable", "unit", "value")

  # assume that real duplicates can be removed
  data <- distinct(data)

  # entries with varying values are summed up
  data <- aggregate(value ~ region + year + scenario + variable + unit, data, sum)

  as.magpie(data, spatial = 1)
}
