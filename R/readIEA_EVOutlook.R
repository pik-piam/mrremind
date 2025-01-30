#' Read IEA EV Outlook
#'
#' @author Falk Benke
readIEA_EVOutlook <- function() {
  data <- utils::read.csv2(file.path("data", "2024", "IEA Global EV Data 2024.csv"), sep = ",")

  data <- data %>%
    mutate(
      "variable" = sub("\\|NA", "", paste0(.data$parameter, "|", .data$mode, "|", .data$powertrain)),
      "value" = as.numeric(.data$value),
      "unit" = ifelse(is.na(.data$unit), "no", .data$unit)
    ) %>%
    select("region", "year", "scenario" = "category", "variable", "unit", "value")

  as.magpie(data, spatial = 1)
}
