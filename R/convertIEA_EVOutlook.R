#' Convert IEA EV Outlook
#'
#' @param x a magclass object returned from `readIEA_EVOutlook()`
#' @author Falk Benke

convertIEA_EVOutlook <- function(x) {

  data <- x[c(
    "World", "Rest of the world", "Europe", "EU27",
    "Africa", "Asia Pacific", "Central and South America",
    "Middle East and Caspian", "North America"
  ), , invert = TRUE]
  getItems(data, dim = 1) <- toolCountry2isocode(getItems(data, dim = 1))
  data <- toolCountryFill(data, fill = NA, verbosity = 2)

  # set 0s in other CHA countries than China to approximate CHA as China
  data[c("HKG", "MAC", "TWN"), , ] <- 0

  return(data)
}
