#' Nuclear data from world-nuclear.org
#' @description  Data on currently operating and under-construction nuclear power plants, reactors planned and proposed,
#' electricity generation from nuclear
#' @author Christoph Bertram
 #' @param x MAgPIE object to be converted

convertIAEA <- function(x) {

      # remove world data
      x <- x["WORLD",,,invert=TRUE]
      # Data for Taiwan in 2020 is given separately and not included in the list
      x["Taiwan","y2020","REACTORS OPERABLE (MWe net)"] <- 3719
      x["Taiwan","y2020","REACTORS OPERABLE (No)"] <- 4
      x["Taiwan","y2020","NUCLEAR ELECTRICITY GENERATION (billion kWh)"] <- 26.7
      # rename countries into ISO
      getRegions(x) <- toolCountry2isocode(getRegions(x))
      # fill missing countries
      x <- toolCountryFill(x, fill = 0, verbosity = 2)
      x[is.na(x)] <- 0

      return(x)
}
