#' Calculates FE historical from IEA energy balances, projections from EDGE, and historical
#' values from IEA WEO 2019
#' @author Lavinia Baumstark, Aman Malik
#' @importFrom dplyr %>%
#' @importFrom quitte inline.data.frame
#' @importFrom stats na.omit
#' @param source "IEA" or "IEA_WEO"
#' @param scenario_proj "SSP2" by default unless overwritten


calcFE <- function(source = "IEA", scenario_proj = "SSP2") {
  #------ READ-IN DATA----------------------------------------
  if (source == "IEA") {
    data <- calcOutput("IO", subtype = "output", aggregate = FALSE)

    # remove period where only 0s appear
    data <- data[, 2021, , invert = T]

    mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_reporting.csv", returnPathOnly = TRUE, where = "mappingfolder")
    target <- c("output")
    map <- read.csv2(mapping, stringsAsFactors = FALSE, na.strings = "")
    # delete NAs rows
    map <- map[c("io", target)] %>% na.omit()

    # Change the column name of the mapping
    colnames(map) <- gsub("io", "names_in", colnames(map))

    # Give description
    descript <- "IEA Final Energy Data based on 2017 version of IEA Energy Balances"

    #------ PROCESS DATA ------------------------------------------
    # select data that have names
    x <- data[, , map$names_in]
    # rename entries of data to match the reporting names
    getNames(x) <- paste0(map$output, " (EJ/yr)")

    # add more variables
    x <- mbind(x, setNames(dimSums(x[, , "FE|", pmatch = TRUE], dim = 3), "FE (EJ/yr)"))
    x <- mbind(x, setNames(dimSums(x[, , "FE|", pmatch = TRUE][, , "Electricity", pmatch = TRUE], dim = 3), "FE|Electricity (EJ/yr)"))
    x <- mbind(x, setNames(dimSums(x[, , "FE|", pmatch = TRUE][, , "Gases", pmatch = TRUE], dim = 3), "FE|Gases (EJ/yr)"))
    x <- mbind(x, setNames(dimSums(x[, , "FE|", pmatch = TRUE][, , "Heat", pmatch = TRUE], dim = 3), "FE|Heat (EJ/yr)"))
    x <- mbind(x, setNames(dimSums(x[, , "FE|", pmatch = TRUE][, , "Liquids", pmatch = TRUE], dim = 3), "FE|Liquids (EJ/yr)"))
    x <- mbind(x, setNames(dimSums(x[, , "FE|", pmatch = TRUE][, , "Solids", pmatch = TRUE], dim = 3), "FE|Solids (EJ/yr)"))

    # aggregate CHP and nonCHP electricity
    x <- mbind(x, setNames(x[, , "SE|Electricity|Coal|CHP (EJ/yr)"] +
      x[, , "SE|Electricity|Coal|nonCHP (EJ/yr)"], "SE|Electricity|Coal (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "SE|Electricity|Gas|CHP (EJ/yr)"] +
      x[, , "SE|Electricity|Gas|nonCHP (EJ/yr)"], "SE|Electricity|Gas (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "SE|Electricity|Biomass|CHP (EJ/yr)"] +
      x[, , "SE|Electricity|Biomass|nonCHP (EJ/yr)"], "SE|Electricity|Biomass (EJ/yr)"))

    # aggregate CHP and HP heat
    x <- mbind(x, setNames(x[, , "SE|Heat|Coal|CHP (EJ/yr)"] +
      x[, , "SE|Heat|Coal|HP (EJ/yr)"], "SE|Heat|Coal (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "SE|Heat|Gas|CHP (EJ/yr)"] +
      x[, , "SE|Heat|Gas|HP (EJ/yr)"], "SE|Heat|Gas (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "SE|Heat|Biomass|CHP (EJ/yr)"] +
      x[, , "SE|Heat|Biomass|HP (EJ/yr)"], "SE|Heat|Biomass (EJ/yr)"))

    # rename Diesel/Petrol to LDV/non-LDV
    x <- mbind(x, setNames(x[, , "FE|Transport|Liquids|Diesel|Biomass (EJ/yr)"], "FE|Transport|non-LDV|Liquids|Biomass (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "FE|Transport|Liquids|Diesel|Fossil (EJ/yr)"], "FE|Transport|non-LDV|Liquids|Fossil (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "FE|Transport|Liquids|Petrol|Biomass (EJ/yr)"], "FE|Transport|LDV|Liquids|Biomass (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "FE|Transport|Liquids|Petrol|Fossil (EJ/yr)"], "FE|Transport|LDV|Liquids|Fossil (EJ/yr)"))
    x <- x[, , c(
      "FE|Transport|Liquids|Diesel|Biomass (EJ/yr)",
      "FE|Transport|Liquids|Diesel|Fossil (EJ/yr)",
      "FE|Transport|Liquids|Petrol|Biomass (EJ/yr)",
      "FE|Transport|Liquids|Petrol|Fossil (EJ/yr)"
    ), invert = TRUE]

    # aggregate LDV and non-LDV to Liquids|Biomass/Fossil
    x <- mbind(x, setNames(x[, , "FE|Transport|LDV|Liquids|Biomass (EJ/yr)"] +
      x[, , "FE|Transport|non-LDV|Liquids|Biomass (EJ/yr)"], "FE|Transport|Liquids|Biomass (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "FE|Transport|LDV|Liquids|Fossil (EJ/yr)"] +
      x[, , "FE|Transport|non-LDV|Liquids|Fossil (EJ/yr)"], "FE|Transport|Liquids|Fossil (EJ/yr)"))

    # aggregate biomass and fossil data
    x <- mbind(x, setNames(x[, , "FE|Buildings|Gases|Biomass (EJ/yr)"] +
      x[, , "FE|Buildings|Gases|Fossil (EJ/yr)"], "FE|Buildings|Gases (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "FE|Industry|Gases|Biomass (EJ/yr)"] +
      x[, , "FE|Industry|Gases|Fossil (EJ/yr)"], "FE|Industry|Gases (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "FE|Transport|Gases|Biomass (EJ/yr)"] +
      x[, , "FE|Transport|Gases|Fossil (EJ/yr)"], "FE|Transport|Gases (EJ/yr)"))

    x <- mbind(x, setNames(x[, , "FE|Buildings|Liquids|Biomass (EJ/yr)"] +
      x[, , "FE|Buildings|Liquids|Fossil (EJ/yr)"], "FE|Buildings|Liquids (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "FE|Industry|Liquids|Biomass (EJ/yr)"] +
      x[, , "FE|Industry|Liquids|Fossil (EJ/yr)"], "FE|Industry|Liquids (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "FE|Transport|Liquids|Biomass (EJ/yr)"] +
      x[, , "FE|Transport|Liquids|Fossil (EJ/yr)"], "FE|Transport|Liquids (EJ/yr)"))

    x <- mbind(x, setNames(x[, , "FE|Buildings|Solids|Biomass (EJ/yr)"] +
      x[, , "FE|Buildings|Solids|Fossil (EJ/yr)"], "FE|Buildings|Solids (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "FE|Industry|Solids|Biomass (EJ/yr)"] +
      x[, , "FE|Industry|Solids|Fossil (EJ/yr)"], "FE|Industry|Solids (EJ/yr)"))

    # add stationary
    x <- mbind(x, setNames(x[, , "FE|Buildings|Electricity (EJ/yr)"] +
      x[, , "FE|Industry|Electricity (EJ/yr)"], "FE|Stationary|Electricity (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "FE|Buildings|Gases (EJ/yr)"] +
      x[, , "FE|Industry|Gases (EJ/yr)"], "FE|Stationary|Gases (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "FE|Buildings|Heat (EJ/yr)"] +
      x[, , "FE|Industry|Heat (EJ/yr)"], "FE|Stationary|Heat (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "FE|Buildings|Liquids (EJ/yr)"] +
      x[, , "FE|Industry|Liquids (EJ/yr)"], "FE|Stationary|Liquids (EJ/yr)"))
    x <- mbind(x, setNames(x[, , "FE|Buildings|Solids (EJ/yr)"] +
      x[, , "FE|Industry|Solids (EJ/yr)"], "FE|Stationary|Solids (EJ/yr)"))

    # add total for buildings
    x <- mbind(x, setNames(x[, , "FE|Buildings|Liquids (EJ/yr)"]
    + x[, , "FE|Buildings|Gases (EJ/yr)"]
      + x[, , "FE|Buildings|Solids (EJ/yr)"]
      + x[, , "FE|Buildings|Heat (EJ/yr)"]
      + x[, , "FE|Buildings|Electricity (EJ/yr)"], "FE|Buildings (EJ/yr)"))
    # add total for industry
    x <- mbind(x, setNames(x[, , "FE|Industry|Liquids (EJ/yr)"]
    + x[, , "FE|Industry|Gases (EJ/yr)"]
      + x[, , "FE|Industry|Solids (EJ/yr)"]
      + x[, , "FE|Industry|Heat (EJ/yr)"]
      + x[, , "FE|Industry|Electricity (EJ/yr)"], "FE|Industry (EJ/yr)"))
    # add total for transport
    x <- mbind(x, setNames(x[, , "FE|Transport|Liquids (EJ/yr)"]
    + x[, , "FE|Transport|Gases (EJ/yr)"]
      + x[, , "FE|Transport|Electricity (EJ/yr)"], "FE|Transport (EJ/yr)"))
  } else if (source == "IEA_WEO") {
    data <- readSource(type = "IEA_WEO", subtype = "FE")
    regions <- toolGetMapping(getConfig()[1], where = "mappingfolder", type = "regional")
    # regions <- unique(regions$RegionCode)

    # gdp of all countries in 2015
    gdp <- calcOutput("GDPPast", years = 2015, aggregate = FALSE)

    # if 2015 gdp of a country is 90% of the GDP of the region to which it belongs
    # include result. If not, display it as NA

    var <- getNames(data)[1]
    data_new <- new.magpie(getRegions(data), years = getYears(data), names = getNames(data), fill = NA)
    for (i in regions$CountryCode) {
      if (!is.na(data[i, "y2010", var]) & gdp[i, , ] > 0.9 * dimSums(gdp[regions[regions$RegionCode == regions[regions$CountryCode == i, ]$RegionCode, ]$CountryCode, , ], dim = 1)) {
        data_new[i, , ] <- data[i, , ]
        countries <- regions[regions$RegionCode == regions[regions$CountryCode == i, ]$RegionCode, ]$CountryCode
        data_new[setdiff(countries, i), , ] <- 0 # countries other than the "main" country
        # get zero value so that aggregation can be done
      }
    }

    data <- data_new

    data <- data[, , ] * 4.1868e-2 # Mtoe to EJ
    # data <- collapseNames(data)
    # vars <- c("Coal","Oil","Gas")
    # data <- data[,,vars,pmatch=T]
    # converting to remind convention
    getNames(data) <- gsub(pattern = "Final Energy", replacement = "FE", x = getNames(data))
    getNames(data) <- paste0(getNames(data), " (EJ/yr)")

    x <- data
    x <- collapseNames(x)
    descript <- "Final Energy data from IEA World Energy Outlook 2019"
  }

  return(list(
    x = x, weight = NULL, unit = "EJ",
    description = descript
  ))
}
