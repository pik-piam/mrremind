#' Calculates FE historical from IEA energy balances
#' @author Lavinia Baumstark, Aman Malik
#' @param ieaVersion Release version of IEA data, either 'default' (vetted and used in REMIND)
#' or 'latest'.
calcFE <- function(ieaVersion = "default") {
  #------ READ-IN DATA----------------------------------------

  data <- calcOutput("IO",
    subtype = "output_reporting", corrected = TRUE,
    ieaVersion = ieaVersion, aggregate = FALSE
  )

  mapping <- toolGetMapping(
    type = "sectoral",
    name = "structuremappingIO_reporting.csv",
    where = "mrremind", returnPathOnly = TRUE
  )

  map <- utils::read.csv2(mapping, stringsAsFactors = FALSE, na.strings = "")
  # delete NAs rows
  map <- map[c("io", "output")] %>% stats::na.omit()

  # Change the column name of the mapping
  colnames(map) <- gsub("io", "names_in", colnames(map))

  # Give description
  descript <- paste0(
    "IEA Final Energy Data based on ",
    toolGetIEAYear(ieaVersion),
    " version of IEA Energy Balances"
  )

  #------ PROCESS DATA ------------------------------------------
  # select data that have names

  #  TODO: temporary check, fix warnings and remove
  if (length(setdiff(map$names_in, getNames(data))) > 0) {
    items <- setdiff(map$names_in, getNames(data))
    warning("mappings without data in calcIO found :", items)
  }

  x <- data[, , intersect(getNames(data), map$names_in)]
  map <- map[map$names_in %in% getNames(x), ]

  # rename entries of data to match the reporting names
  getNames(x) <- paste0(map$output, " (EJ/yr)")

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
  x <- mbind(x, setNames(x[, , "FE|Transport|Liquids|Fossil (EJ/yr)"] +
    x[, , "FE|Transport|Liquids|Biomass (EJ/yr)"], "FE|Transport|Liquids (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Gases|Fossil (EJ/yr)"] +
    x[, , "FE|Transport|Gases|Biomass (EJ/yr)"], "FE|Transport|Gases (EJ/yr)"))

  # new aggregations based on more detailed categories from IEA:

  ## first change name of existing variables, to allow later comparison to the previous results:

  x <- mbind(x, setNames(x[, , "FE|Transport|Electricity (EJ/yr)"], "FE|TransportOldCalc|Electricity (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Gases|Biomass (EJ/yr)"], "FE|TransportOldCalc|Gases|Biomass (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Gases|Fossil (EJ/yr)"], "FE|TransportOldCalc|Gases|Fossil (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Gases (EJ/yr)"], "FE|TransportOldCalc|Gases (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Bunkers (EJ/yr)"], "FE|TransportOldCalc|Bunkers (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Liquids|Biomass (EJ/yr)"], "FE|TransportOldCalc|Liquids|Biomass (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Liquids|Fossil (EJ/yr)"], "FE|TransportOldCalc|Liquids|Fossil (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Liquids (EJ/yr)"], "FE|TransportOldCalc|Liquids (EJ/yr)"))

  ## then calculate the previous totals
  # add total for transport
  x <- mbind(x, setNames(
    x[, , "FE|TransportOldCalc|Liquids (EJ/yr)"]
    + x[, , "FE|TransportOldCalc|Gases (EJ/yr)"]
      + x[, , "FE|TransportOldCalc|Electricity (EJ/yr)"], # there seemed to be no solids variable in the previous "historical" version
    "FE|TransportOldCalc (EJ/yr)"
  ))
  # add transport w/o Bunkers
  x <- mbind(x, setNames(x[, , "FE|TransportOldCalc (EJ/yr)"]
  - x[, , "FE|TransportOldCalc|Bunkers (EJ/yr)"], "FE|TransportOldCalc|w/o Bunkers (EJ/yr)"))

  ## then remove the variables with plain names that will now be recalculated from the disaggregated values
  x <- x[, , c(
    "FE|Transport|Electricity (EJ/yr)",
    "FE|Transport|Gases|Fossil (EJ/yr)",
    "FE|Transport|Gases|Biomass (EJ/yr)",
    "FE|Transport|Gases (EJ/yr)",
    "FE|Transport|Liquids|Fossil (EJ/yr)",
    "FE|Transport|Liquids|Biomass (EJ/yr)",
    "FE|Transport|Liquids (EJ/yr)",
    "FE|Transport|Bunkers (EJ/yr)"
  ), invert = TRUE]

  ## domestic aviation
  x <- mbind(x, setNames(x[, , "FE|Transport|DomAv|Liquids|Fossil (EJ/yr)"], "FE|Transport|Pass|Domestic Aviation|Liquids|Fossil (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|DomAv|Liquids|Fossil (EJ/yr)"], "FE|Transport|Pass|Domestic Aviation|Liquids (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|DomAv|Liquids|Fossil (EJ/yr)"], "FE|Transport|Pass|Domestic Aviation (EJ/yr)"))

  ## domestic navigation
  x <- mbind(x, setNames(x[, , "FE|Transport|DomNav|Liquids|Diesel|Fossil (EJ/yr)"] +
    x[, , "FE|Transport|DomNav|Liquids|Petrol|Fossil (EJ/yr)"], "FE|Transport|Freight|Domestic Shipping|Liquids|Fossil (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|DomNav|Liquids|Diesel|Biomass (EJ/yr)"] +
    x[, , "FE|Transport|DomNav|Liquids|Petrol|Biomass (EJ/yr)"], "FE|Transport|Freight|Domestic Shipping|Liquids|Biomass (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Freight|Domestic Shipping|Liquids|Fossil (EJ/yr)"] +
    x[, , "FE|Transport|Freight|Domestic Shipping|Liquids|Biomass (EJ/yr)"], "FE|Transport|Freight|Domestic Shipping|Liquids (EJ/yr)"))

  x <- mbind(x, setNames(x[, , "FE|Transport|DomNav|Gases|Fossil (EJ/yr)"], "FE|Transport|Freight|Domestic Shipping|Gases|Fossil (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|DomNav|Gases|Biomass (EJ/yr)"], "FE|Transport|Freight|Domestic Shipping|Gases|Biomass (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Freight|Domestic Shipping|Gases|Fossil (EJ/yr)"] +
    x[, , "FE|Transport|Freight|Domestic Shipping|Gases|Biomass (EJ/yr)"], "FE|Transport|Freight|Domestic Shipping|Gases (EJ/yr)"))

  x <- mbind(x, setNames(x[, , "FE|Transport|Freight|Domestic Shipping|Gases (EJ/yr)"] +
    x[, , "FE|Transport|Freight|Domestic Shipping|Liquids (EJ/yr)"], "FE|Transport|Freight|Domestic Shipping (EJ/yr)"))

  ## Road
  x <- mbind(x, setNames(x[, , "FE|Transport|Road|Liquids|Diesel|Fossil (EJ/yr)"] +
    x[, , "FE|Transport|Road|Liquids|Petrol|Fossil (EJ/yr)"], "FE|Transport|Road|Liquids|Fossil (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Road|Liquids|Diesel|Biomass (EJ/yr)"] +
    x[, , "FE|Transport|Road|Liquids|Petrol|Biomass (EJ/yr)"], "FE|Transport|Road|Liquids|Biomass (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Road|Liquids|Fossil (EJ/yr)"] +
    x[, , "FE|Transport|Road|Liquids|Biomass (EJ/yr)"], "FE|Transport|Road|Liquids (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Road|Gases|Fossil (EJ/yr)"] +
    x[, , "FE|Transport|Road|Gases|Biomass (EJ/yr)"], "FE|Transport|Road|Gases (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Road|Gases (EJ/yr)"] + x[, , "FE|Transport|Road|Liquids (EJ/yr)"] +
    x[, , "FE|Transport|Road|Electricity (EJ/yr)"], "FE|Transport|Road (EJ/yr)"))

  ## Rail
  x <- mbind(x, setNames(x[, , "FE|Transport|Rail|Liquids|Diesel|Fossil (EJ/yr)"], "FE|Transport|Rail|Liquids|Fossil (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Rail|Liquids|Diesel|Biomass (EJ/yr)"], "FE|Transport|Rail|Liquids|Biomass (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Rail|Liquids|Fossil (EJ/yr)"] +
    x[, , "FE|Transport|Rail|Liquids|Biomass (EJ/yr)"], "FE|Transport|Rail|Liquids (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Rail|Solids|Fossil (EJ/yr)"] +
    x[, , "FE|Transport|Rail|Solids|Biomass (EJ/yr)"], "FE|Transport|Rail|Solids (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Rail|Gases|Fossil (EJ/yr)"], "FE|Transport|Rail|Gases (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Rail|Gases (EJ/yr)"] + x[, , "FE|Transport|Rail|Liquids (EJ/yr)"] +
    x[, , "FE|Transport|Rail|Electricity (EJ/yr)"] + x[, , "FE|Transport|Rail|Solids (EJ/yr)"], "FE|Transport|Rail (EJ/yr)"))


  ## Pipeline
  x <- mbind(x, setNames(x[, , "FE|Transport|Pipeline|Liquids|Diesel|Fossil (EJ/yr)"] +
    x[, , "FE|Transport|Pipeline|Liquids|Petrol|Fossil (EJ/yr)"], "FE|Transport|Pipeline|Liquids|Fossil (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Pipeline|Liquids|Fossil (EJ/yr)"], "FE|Transport|Pipeline|Liquids (EJ/yr)"))

  x <- mbind(x, setNames(x[, , "FE|Transport|Pipeline|Gases|Fossil (EJ/yr)"] +
    x[, , "FE|Transport|Pipeline|Gases|Biomass (EJ/yr)"], "FE|Transport|Pipeline|Gases (EJ/yr)"))

  x <- mbind(x, setNames(x[, , "FE|Transport|Pipeline|Gases (EJ/yr)"] + x[, , "FE|Transport|Pipeline|Liquids (EJ/yr)"] +
    x[, , "FE|Transport|Pipeline|Electricity (EJ/yr)"], "FE|Transport|Pipeline (EJ/yr)"))

  ## NotSpecified
  x <- mbind(x, setNames(x[, , "FE|Transport|NotSpecified|Liquids|Diesel|Fossil (EJ/yr)"], "FE|Transport|NotSpecified|Liquids|Fossil (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|NotSpecified|Liquids|Diesel|Biomass (EJ/yr)"], "FE|Transport|NotSpecified|Liquids|Biomass (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|NotSpecified|Liquids|Fossil (EJ/yr)"] +
    x[, , "FE|Transport|NotSpecified|Liquids|Biomass (EJ/yr)"], "FE|Transport|NotSpecified|Liquids (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|NotSpecified|Gases|Fossil (EJ/yr)"] +
    x[, , "FE|Transport|NotSpecified|Gases|Biomass (EJ/yr)"], "FE|Transport|NotSpecified|Gases (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|NotSpecified|Gases (EJ/yr)"] + x[, , "FE|Transport|NotSpecified|Liquids (EJ/yr)"] +
    x[, , "FE|Transport|NotSpecified|Electricity (EJ/yr)"], "FE|Transport|NotSpecified (EJ/yr)"))

  ## Bunker details
  x <- mbind(x, setNames(x[, , "FE|Transport|Bunkers|IntAv|Liquids (EJ/yr)"], "FE|Transport|Bunkers|Pass|International Aviation|Liquids (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Bunkers|IntAv|Liquids (EJ/yr)"], "FE|Transport|Bunkers|Pass|International Aviation (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Bunkers|IntAv|Liquids (EJ/yr)"], "FE|Transport|Bunkers|Pass|Liquids (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Bunkers|IntAv|Liquids (EJ/yr)"], "FE|Transport|Bunkers|Pass (EJ/yr)"))


  x <- mbind(x, setNames(x[, , "FE|Transport|Bunkers|IntNav|Liquids (EJ/yr)"], "FE|Transport|Bunkers|Freight|International Shipping|Liquids (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Bunkers|IntNav|Liquids (EJ/yr)"], "FE|Transport|Bunkers|Freight|International Shipping (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Bunkers|IntNav|Liquids (EJ/yr)"], "FE|Transport|Bunkers|Freight|Liquids (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Bunkers|IntNav|Liquids (EJ/yr)"], "FE|Transport|Bunkers|Freight (EJ/yr)"))


  # check aggregations

  ## fossil per carrier
  x <- mbind(x, setNames(
    x[, , "FE|Transport|Freight|Domestic Shipping|Liquids|Fossil (EJ/yr)"]
    + x[, , "FE|Transport|NotSpecified|Liquids|Fossil (EJ/yr)"]
      + x[, , "FE|Transport|Pass|Domestic Aviation|Liquids|Fossil (EJ/yr)"]
      + x[, , "FE|Transport|Rail|Liquids|Fossil (EJ/yr)"]
      + x[, , "FE|Transport|Road|Liquids|Fossil (EJ/yr)"],
    "FE|Transport|w/o Bunkers|Liquids|Fossil (EJ/yr)"
  ))

  x <- mbind(x, setNames(
    x[, , "FE|Transport|NotSpecified|Gases|Fossil (EJ/yr)"]
    + x[, , "FE|Transport|Freight|Domestic Shipping|Gases|Fossil (EJ/yr)"]
      + x[, , "FE|Transport|Rail|Gases|Fossil (EJ/yr)"]
      + x[, , "FE|Transport|Road|Gases|Fossil (EJ/yr)"],
    "FE|Transport|w/o Bunkers|Gases|Fossil (EJ/yr)"
  ))

  x <- mbind(x, setNames(x[, , "FE|Transport|Rail|Solids|Fossil (EJ/yr)"], "FE|Transport|w/o Bunkers|Solids|Fossil (EJ/yr)"))

  ## biomass per carrier
  x <- mbind(x, setNames(
    x[, , "FE|Transport|Freight|Domestic Shipping|Liquids|Biomass (EJ/yr)"]
    + x[, , "FE|Transport|NotSpecified|Liquids|Biomass (EJ/yr)"]
      + x[, , "FE|Transport|Rail|Liquids|Biomass (EJ/yr)"]
      + x[, , "FE|Transport|Road|Liquids|Biomass (EJ/yr)"],
    "FE|Transport|w/o Bunkers|Liquids|Biomass (EJ/yr)"
  ))

  x <- mbind(x, setNames(
    x[, , "FE|Transport|NotSpecified|Gases|Biomass (EJ/yr)"]
    + x[, , "FE|Transport|Freight|Domestic Shipping|Gases|Biomass (EJ/yr)"]
      + x[, , "FE|Transport|Road|Gases|Biomass (EJ/yr)"],
    "FE|Transport|w/o Bunkers|Gases|Biomass (EJ/yr)"
  ))

  x <- mbind(x, setNames(x[, , "FE|Transport|Rail|Solids|Biomass (EJ/yr)"], "FE|Transport|w/o Bunkers|Solids|Biomass (EJ/yr)"))

  ## biomass + fossil per carrier

  x <- mbind(x, setNames(
    x[, , "FE|Transport|w/o Bunkers|Liquids|Fossil (EJ/yr)"]
    + x[, , "FE|Transport|w/o Bunkers|Liquids|Biomass (EJ/yr)"],
    "FE|Transport|w/o Bunkers|Liquids (EJ/yr)"
  ))

  x <- mbind(x, setNames(
    x[, , "FE|Transport|w/o Bunkers|Gases|Fossil (EJ/yr)"]
    + x[, , "FE|Transport|w/o Bunkers|Gases|Biomass (EJ/yr)"],
    "FE|Transport|w/o Bunkers|Gases (EJ/yr)"
  ))

  x <- mbind(x, setNames(
    x[, , "FE|Transport|NotSpecified|Electricity (EJ/yr)"]
    + x[, , "FE|Transport|Rail|Electricity (EJ/yr)"]
      + x[, , "FE|Transport|Road|Electricity (EJ/yr)"],
    "FE|Transport|w/o Bunkers|Electricity (EJ/yr)"
  ))

  x <- mbind(x, setNames(x[, , "FE|Transport|Rail|Solids (EJ/yr)"], "FE|Transport|w/o Bunkers|Solids (EJ/yr)"))

  ## total

  x <- mbind(x, setNames(
    x[, , "FE|Transport|Freight|Domestic Shipping (EJ/yr)"]
    + x[, , "FE|Transport|NotSpecified (EJ/yr)"]
      + x[, , "FE|Transport|Pass|Domestic Aviation (EJ/yr)"]
      + x[, , "FE|Transport|Rail (EJ/yr)"]
      + x[, , "FE|Transport|Road (EJ/yr)"],
    "FE|Transport|w/o Bunkers (EJ/yr)"
  ))

  # check aggregates with bunkers
  x <- mbind(x, setNames(x[, , "FE|Transport|Bunkers|Pass|Liquids (EJ/yr)"]
  + x[, , "FE|Transport|Bunkers|Freight|Liquids (EJ/yr)"], "FE|Transport|Bunkers|Liquids (EJ/yr)"))

  x <- mbind(x, setNames(x[, , "FE|Transport|Bunkers|Pass (EJ/yr)"]
  + x[, , "FE|Transport|Bunkers|Freight (EJ/yr)"], "FE|Transport|Bunkers (EJ/yr)"))

  ## no biomass in bunkers

  x <- mbind(x, setNames(x[, , "FE|Transport|w/o Bunkers|Liquids|Biomass (EJ/yr)"], "FE|Transport|Liquids|Biomass (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|w/o Bunkers|Gases|Biomass (EJ/yr)"], "FE|Transport|Gases|Biomass (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|w/o Bunkers|Solids|Biomass (EJ/yr)"], "FE|Transport|Solids|Biomass (EJ/yr)"))

  ## only fossil liquids in in bunkers
  x <- mbind(x, setNames(x[, , "FE|Transport|w/o Bunkers|Liquids|Fossil (EJ/yr)"]
  + x[, , "FE|Transport|Bunkers|Liquids (EJ/yr)"], "FE|Transport|Liquids|Fossil (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|w/o Bunkers|Solids|Fossil (EJ/yr)"], "FE|Transport|Solids|Fossil (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|w/o Bunkers|Gases|Fossil (EJ/yr)"], "FE|Transport|Gases|Fossil (EJ/yr)"))

  x <- mbind(x, setNames(x[, , "FE|Transport|w/o Bunkers|Electricity (EJ/yr)"], "FE|Transport|Electricity (EJ/yr)"))

  x <- mbind(x, setNames(x[, , "FE|Transport|w/o Bunkers (EJ/yr)"]
  + x[, , "FE|Transport|Bunkers (EJ/yr)"], "FE|Transport (EJ/yr)"))

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
  x <- mbind(x, setNames(x[, , "FE|Transport|Solids|Biomass (EJ/yr)"] +
    x[, , "FE|Transport|Solids|Fossil (EJ/yr)"], "FE|Transport|Solids (EJ/yr)"))

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
  # add industry w/o Non-energy Use
  x <- mbind(x, setNames(x[, , "FE|Industry (EJ/yr)"]
  - x[, , "FE|Non-energy Use (EJ/yr)"], "FE|w/o Non-energy Use|Industry (EJ/yr)"))


  # add total
  x <- mbind(x, setNames(x[, , "FE|Transport (EJ/yr)"]
  + x[, , "FE|Industry (EJ/yr)"]
    + x[, , "FE|Buildings (EJ/yr)"], "FE (EJ/yr)"))

  # add more variables
  x <- mbind(x, setNames(dimSums(x[, , "FE|", pmatch = TRUE], dim = 3)
  - x[, , "FE|Transport|Bunkers (EJ/yr)"] - x[, , "FE|Non-energy Use (EJ/yr)"], "FEtestSumAll (EJ/yr)")) # these new dummy items blow up FE demand when simply summing over all FE| terms, so they need to be subtracted again
  x <- mbind(x, setNames(x[, , "FE (EJ/yr)"] - x[, , "FE|Non-energy Use (EJ/yr)"], "FE|w/o Non-energy Use (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE (EJ/yr)"] - x[, , "FE|Transport|Bunkers (EJ/yr)"], "FE|w/o Bunkers (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE (EJ/yr)"] - x[, , "FE|Transport|Bunkers (EJ/yr)"] - x[, , "FE|Non-energy Use (EJ/yr)"], "FE|w/o Non-energy Use w/o Bunkers (EJ/yr)"))
  x <- mbind(x, setNames(dimSums(x[, , "FE|", pmatch = TRUE][, , "Electricity", pmatch = TRUE], dim = 3), "FEtestSumAll|Electricity (EJ/yr)"))
  x <- mbind(x, setNames(dimSums(x[, , "FE|", pmatch = TRUE][, , "Gases", pmatch = TRUE], dim = 3), "FEtestSumAll|Gases (EJ/yr)"))
  x <- mbind(x, setNames(dimSums(x[, , "FE|", pmatch = TRUE][, , "Heat", pmatch = TRUE], dim = 3), "FEtestSumAll|Heat (EJ/yr)"))
  x <- mbind(x, setNames(dimSums(x[, , "FE|", pmatch = TRUE][, , "Liquids", pmatch = TRUE], dim = 3), "FEtestSumAll|Liquids (EJ/yr)"))
  x <- mbind(x, setNames(dimSums(x[, , "FE|", pmatch = TRUE][, , "Solids", pmatch = TRUE], dim = 3), "FEtestSumAll|Solids (EJ/yr)"))


  # add totals per carrier (automatic sum doesn't work anymore, as various sub-aggregates were added)
  x <- mbind(x, setNames(x[, , "FE|Transport|Electricity (EJ/yr)"]
  + x[, , "FE|Industry|Electricity (EJ/yr)"]
    + x[, , "FE|Buildings|Electricity (EJ/yr)"], "FE|Electricity (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Gases (EJ/yr)"]
  + x[, , "FE|Industry|Gases (EJ/yr)"]
    + x[, , "FE|Buildings|Gases (EJ/yr)"], "FE|Gases (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Industry|Heat (EJ/yr)"]
  + x[, , "FE|Buildings|Heat (EJ/yr)"], "FE|Heat (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Liquids (EJ/yr)"]
  + x[, , "FE|Industry|Liquids (EJ/yr)"]
    + x[, , "FE|Buildings|Liquids (EJ/yr)"], "FE|Liquids (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Solids (EJ/yr)"]
  + x[, , "FE|Industry|Solids (EJ/yr)"]
    + x[, , "FE|Buildings|Solids (EJ/yr)"], "FE|Solids (EJ/yr)"))

  # add further transport variables with names that are currently reported from EDGE-t
  x <- mbind(x, setNames(x[, , "FE|Transport (EJ/yr)"], "FE|Transport with bunkers (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Electricity (EJ/yr)"], "FE|Transport with bunkers|Electricity (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Gases (EJ/yr)"], "FE|Transport with bunkers|Gases (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Liquids (EJ/yr)"], "FE|Transport with bunkers|Liquids (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "FE|Transport|Solids (EJ/yr)"], "FE|Transport with bunkers|Solids (EJ/yr)"))


  return(list(
    x = x, weight = NULL, unit = "EJ",
    description = descript
  ))
}
