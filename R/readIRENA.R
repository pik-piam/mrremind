#' Read IRENA
#'
#' Read-in an IRENA csv file as magclass object
#'
#' @param subtype data subtype. Either "Capacity" or "Generation"
#' @return magpie object of the IRENA data with historical electricity renewable
#' capacities (MW) or generation levels (GWh)
#' @author Renato Rodrigues
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource(type = "IRENA", subtype = "Capacity")
#' }
#'
#' @importFrom dplyr mutate rename
#' @importFrom readr read_csv

readIRENA <- function(subtype) {
  if (subtype == "Capacity") {
    # Reading renewables electricity capacity values in MW from csv
    data <- read.csv2("2022/Capacity.csv", sep = ";", skip = 2)
  } else if (subtype == "Generation") {
    # Reading renewables electricity generation values in GWh from csv
    data <- read.csv2("2022/Generation.csv", sep = ";", skip = 2)
  } else {
    stop("Not a valid subtype!")
  }

  # data in wide format
  data <- reshape2::melt(data,
    id.vars = c(1, 2),
    variable.name = "years", value.name = "value"
  ) %>%
    mutate("value" = as.numeric(!!sym("value"))) %>%
    suppressWarnings()

  # rearrange column order to more readable format: year, country, tech, value (capacity or generation)
  data <- data[, c(3, 1, 2, 4)]

  # replacing X by y on years prefix
  data$years <- gsub("X", "y", data$years)

  # fix name of  "country" column to the value it used to have before 9678353
  data <- data %>%
    rename(`Country/area` = 2)

  # creating capacity or generation magpie object
  x <- as.magpie(data, temporal = 1, spatial = 2, datacol = 4)

  return(x)
}
