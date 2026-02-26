#' Nuclear data from world-nuclear.org
#'
#' @description  Data on currently operating and under-construction nuclear power
#' plants, reactors planned and proposed, electricity generation from nuclear
#' @author Christoph Bertram, Pascal Weigmann

readIAEA <- function() {

  x_2020 <- read.csv("April2020.csv", sep = ",", skip = 2, check.names = FALSE)
  x_2020$YEAR <- 2020

  x_2025 <- read.csv("November2025.csv", sep = ",", skip = 2, check.names = FALSE)
  x_2025$YEAR <- 2025

  x_2020$COUNTRY <- gsub(pattern = "UAE",
                         replacement = "United Arab Emirates",
                         x = x_2020$COUNTRY)
  x_2025$COUNTRY <- gsub(pattern = "UAE",
                         replacement = "United Arab Emirates",
                         x = x_2025$COUNTRY)

  x <- dplyr::bind_rows(x_2020, x_2025)

  # rearranging columns so magpie object can be easily created
  x <- x[, c(1, 13, 2:12)]
  x <- as.magpie(x, spatial = 1, temporal = 2, datacol = 3)

  x <- add_columns(x, addnm = "Taiwan", dim = 1, fill = NA)

  # Data for Taiwan in 2020 is given separately and not included in the list
  x["Taiwan", "y2020", "REACTORS OPERABLE (MWe net)"] <- 3719
  x["Taiwan", "y2020", "REACTORS OPERABLE (No)"] <- 4
  x["Taiwan", "y2020", "NUCLEAR ELECTRICITY GENERATION (billion kWh)"] <- 26.7

  # the number of countries in 2020 are 43, whereas in 2016 and 2018 are 51.
  # The no. of countries in the former are fewer because many of the
  # planned or proposed plants in 2016 and 2018 have been shelved.
  # Thus, all these countries get  "0" for all columns with values.
  x[is.na(x)] <- 0

  return(x)
}
