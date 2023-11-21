#' Read IEA EV Outlook
#'
#' @author Falk Benke
readIEA_EVOutlook <- function() {
  data <- read.csv2("data/2023/IEA Global EV Data 2023.csv", sep = ",")

  data <- data %>%
    mutate(
      !!sym("variable") := sub("\\|NA", "", paste0(!!sym("parameter"), "|", !!sym("mode"), "|", !!sym("powertrain"))),
      !!sym("value") := as.numeric(!!sym("value")),
      !!sym("unit") := ifelse(is.na(!!sym("unit")), "no", !!sym("unit"))
    ) %>%
    select("region", "year", "scenario" = "category", "variable", "unit", "value")

  x <- as.magpie(data, spatial = 1)


  return(x)
}
