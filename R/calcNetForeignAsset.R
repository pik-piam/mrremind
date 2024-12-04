calcNetForeignAsset <- function() {
  # Read in Current account balance from the IMF WEO
  x <- readSource("IMF")[, , "Current account balance [Billions U_S_ dollars]"]

  ### Allocate global current account to the countries
  # Calculate global sum which is not 0
  xSum <- -dimSums(x, dim = 1, na.rm = TRUE)
  # Calculate global absolute share of current account
  xShare <- abs(x) / dimSums(abs(x), dim = 1, na.rm = TRUE)
  # Calculate additional value for each country
  xRest <- xShare * xSum
  # Add global rest to the countries
  x <- x + xRest

  # Sum over the years until 2005
  x <- dimSums(x[, getYears(x, as.integer = TRUE) <= 2005, ], dim = 2)

  # Delete dimensions that are same and not in the GAMS-code
  getNames(x) <- NULL
  getYears(x) <- NULL

  # Convert billion into trillion
  x <- x / 1000

  list(x = x, weight = NULL, unit = "trillion current US$MER", description = "Net foreign asset")
}
