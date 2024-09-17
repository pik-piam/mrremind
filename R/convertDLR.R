convertDLR <- function(x) {
  # fill all missing countries with 0
  x <- toolCountryFill(x, fill = 0, verbosity = 2)
  return(x)
}
