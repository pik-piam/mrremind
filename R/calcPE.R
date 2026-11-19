#' Computes Primary Energy variables from IEA Energy Balances
#'
#' @param ieaVersion Release version of IEA data, either 'default' (vetted and used in REMIND)
#' or 'latest'.
calcPE <- function(ieaVersion = "default") {

  x <- calcOutput("IO",
                  subtype = "input", corrected = TRUE,
                  ieaVersion = ieaVersion, aggregate = FALSE
  )

  mapping <- toolGetMapping(
    type = "sectoral",
    name = "structuremappingIO_reporting.csv",
    where = "mrenergy", returnPathOnly = TRUE
  )

  # TODO: fix outdated mappings once we drop support for IEA 2024
  map <- utils::read.csv2(mapping, stringsAsFactors = FALSE, na.strings = "") %>%
    select("io", "input") %>%
    filter(!is.na(.data$input))

  if (length(setdiff(map$io, getNames(x))) > 0) {
    message(paste0("outdated mappings in PE reporting", paste0(setdiff(map$io, getNames(x)), collapse = ", ")))
  }

  map <- map %>%
    filter(.data$io %in% getNames(x)) %>%
    mutate("input" = paste0(.data$input, " (EJ/yr)"))

  x <- toolAggregate(x[, , unique(map$io)], rel = map, dim = 3, from = "io", to = "input")

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
