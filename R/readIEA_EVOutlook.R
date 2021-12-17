readIEA_EVOutlook <- function() {
  data <- NULL
  for (f in list.files(path = "data")) {
    data <- rbind(data, read.csv2(paste0("data/",f), sep = ","))
  }

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
