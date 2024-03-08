convertILO <- function(x){
  getRegions(x) <- toolCountry2isocode(getRegions(x))
  x <- toolCountryFill(x, fill = 0, verbosity = 2)
}
