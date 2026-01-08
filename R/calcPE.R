#' Computes Primary Energy variables from IEA Energy Balances
#'
#' @param ieaVersion Release version of IEA data, either 'default' (vetted and used in REMIND)
#' or 'latest'.
#' @return a magclass object
calcPE <- function(ieaVersion = "default") {

  data <- calcOutput("IO", subtype = "input", corrected = TRUE,
                     ieaVersion = ieaVersion, aggregate = FALSE)

  mapping <- toolGetMapping(type = "sectoral",
                            name = "structuremappingIO_reporting.csv",
                            where = "mrremind", returnPathOnly = TRUE)
  target <- c("input")

  ### calculate data
  map <- utils::read.csv2(mapping, stringsAsFactors = FALSE, na.strings = "")
  # delete NAs rows
  map <- map[c("io", target)] %>% stats::na.omit()

  # select data that have names
  map <- map[map$io %in% getNames(data), ]
  x <- data[, , map$io]
  # aggregate from the IO names to the reporting names.
  x <- madrat::toolAggregate(x, map, dim = 3, from = "io", to = "input")
  # rename entries of data to match the reporting names
  getNames(x) <- paste0(getNames(x), " (EJ/yr)")

  # add loss to electricity
  x[, , "PE|Coal|Electricity (EJ/yr)"] <- x[, , "PE|Coal|Electricity (EJ/yr)"] +
    x[, , "PE|Coal|Electricity|Loss (EJ/yr)"]
  x[, , "PE|Biomass|Electricity (EJ/yr)"] <- x[, , "PE|Biomass|Electricity (EJ/yr)"] +
    x[, , "PE|Biomass|Electricity|Loss (EJ/yr)"]
  x[, , "PE|Gas|Electricity (EJ/yr)"] <- x[, , "PE|Gas|Electricity (EJ/yr)"] +
    x[, , "PE|Gas|Electricity|Loss (EJ/yr)"]
  x <- x[, , c("PE|Coal|Electricity|Loss (EJ/yr)", "PE|Biomass|Electricity|Loss (EJ/yr)",
               "PE|Gas|Electricity|Loss (EJ/yr)"), invert = TRUE]

  # add more variables
  x <- mbind(x, setNames(dimSums(x[, , "PE|", pmatch = TRUE], dim = 3), "PE (EJ/yr)"))
  x <- mbind(x, setNames(dimSums(x[, , "PE|Coal", pmatch = TRUE], dim = 3), "PE|Coal (EJ/yr)"))
  x <- mbind(x, setNames(dimSums(x[, , "PE|Oil", pmatch = TRUE], dim = 3), "PE|Oil (EJ/yr)"))
  x <- mbind(x, setNames(dimSums(x[, , "PE|Gas", pmatch = TRUE], dim = 3), "PE|Gas (EJ/yr)"))
  x <- mbind(x, setNames(dimSums(x[, , "PE|Biomass", pmatch = TRUE], dim = 3), "PE|Biomass (EJ/yr)"))

  return(list(x = x,
              weight = NULL,
              unit = "EJ",
              description = paste0("IEA Primary Energy Data based on ", toolGetIEAYear(ieaVersion),
                                   " version of IEA Energy Balances")))
}
