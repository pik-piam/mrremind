#' Read Climate Trace
#'
#' Read-in an Climate Trace csv files as magclass object for CO2, CH4 and N2O
#'
#' @return magpie object of the ClimateTrace data with historical emissions
#' by subsector and country
#' @author Pascal Weigmann
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "ClimateTrace")
#' }
#'
#' @importFrom dplyr mutate rename select
#' @importFrom tidyr %>%
readClimateTrace <- function() {
  #setwd(paste0(getConfig("sourcefolder"), "/ClimateTrace"))
  gases <- c("CO2", "CH4", "N2O", "F-Gases")

  data <- data.frame()
  for (gas in gases) {
    files <- list.files(gas, full.names = TRUE)

    # read in each file corresponding to one subsector
    for (file in files) {
      var_data <- read.csv(file) %>%
        mutate(period = substr(.data$start_time, 1, 4),
               region = .data$iso3_country,
               variable = .data$subsector,
               value = .data$emissions_quantity) %>%
        select(gas, variable, region, period, value)
      data <- rbind(data, var_data)
    }
  }

  # remove 2025 data point as it doesn't contain data for a full year yet
  data <- data[data$period != 2025, ]
  x <- as.magpie(data)

  return(x)
}
