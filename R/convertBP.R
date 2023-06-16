#' Disaggregates and cleans BP data.
#' @param x MAgPIE object to be converted
#' @param subtype Either "Emission", "Capacity", "Generation", "Production", "Consumption", "Trade Oil", "Trade Gas", "Trade Coal" or "Price"
#' @description Disaggregates historical data.
#' @return A [`magpie`][magclass::magclass] object.
#' @author Aman Malik, Falk Benke
#' @importFrom dplyr filter
#' @importFrom madrat getISOlist toolGetMapping toolCountryFill
#' @importFrom magclass magpply
#' @importFrom rlang sym
#'

convertBP <- function(x, subtype) {

  Region_name <- NULL
  ISO_code <- NULL
  ISO3.code <- NULL

  PE <- calcOutput("PE", aggregate = FALSE)

  .removeNaRegions <- function(x) {
    remove <- magpply(x, function(y) all(is.na(y)), MARGIN = 1)
    return(x[!remove, , ])
  }

  .removeNaYears <- function(x) {
    remove <- magpply(x, function(y) all(is.na(y)), MARGIN = 2)
    return(x[,!remove, ])
  }


  # disaggregate regions of x by mapping to iso countries belonging to that regions, but not listed in x (i.e. other countries)
  .disaggregate_regions <- function(x_in, regions) {

    x <- .removeNaRegions(x_in)
    x <- .removeNaYears(x)

    # full mapping of regions to iso countries
    mapping_full <- toolGetMapping("regionmappingBP_Full.csv", type = "regional", where = "mappingfolder")

    # iso countries in x
    ctry <- toolCountry2isocode(getItems(x, dim = 1), warn = F)
    ctry <- ctry[!is.na(ctry)]

    # mapping of regions to iso countries other than in ctry (i.e. other regions)
    mapping_regions <- mapping_full[mapping_full$Region_name %in% regions &
                                      !mapping_full$ISO3.code %in% ctry & mapping_full$ISO3.code != "SUN", ]

    weight = PE[mapping_regions$ISO3.code, 2014, "PE (EJ/yr)"]

    # disaggregation of other regions to iso countries
    x2 <- toolAggregate(x[regions, , ], rel = mapping_regions, weight = weight)
    x2 <- toolCountryFill(x2, fill = 0, verbosity = 2)
    x2[is.na(x2)] <- 0

    # iso countries in x that do not need to be disaggregated
    x1 <- x[regions, , invert = TRUE]
    getItems(x1, dim = 1) <- toolCountry2isocode(getItems(x1, dim = 1), warn = F)
    x1 <- toolCountryFill(x1, fill = 0, verbosity = 2)

    # combine the two objects
    x <- x1 + x2
    x <- add_columns(x, setdiff(getItems(x_in, dim = 2), getItems(x, dim = 2)), dim = 2)
    return(x)
  }


  .mergeReg <- function(x, from, to) {
    x1 <- new.magpie(to, getYears(x), getNames(x), fill = NA)
    for (n in getNames(x)) {
      tmp <- x[, , n]
      tmp <- .removeNaYears(tmp)
      x1[, getItems(tmp, dim = 2), n] <- dimSums(tmp[intersect(getItems(x, dim = 1), from), , ], dim = 1, na.rm = T)
    }

    return(mbind(x[from, , invert = T], x1))
  }

  getItems(x, dim = 1) <- gsub("\\bUS\\b", "USA", getItems(x, dim = 1))
  getItems(x, dim = 1) <- gsub(pattern = "China Hong Kong SAR", "Hong Kong", x = getItems(x, dim = 1))

  # for now, we exclude data from Sowjet Union (recorded until 1993)
  x <- x["USSR & Central Europe",,invert = T]
  x <- x["USSR",,invert = T]

  if (subtype == "Emission") {
    x <- .mergeReg(x, from = c("Central America", "Other Caribbean", "Other South America"), to = "S & C America")
    x <- .mergeReg(x, from = c("Other Europe"), to = "Europe")
    x <- .mergeReg(x, from = c("Other CIS"), to = "CIS")
    x <- .mergeReg(x, from = c("Other Middle East"), to = "Middle East")
    x <- .mergeReg(x, from = c("Eastern Africa", "Middle Africa", "Western Africa", "Other Northern Africa", "Other Southern Africa"), to = "Africa")
    x <- .mergeReg(x, from = c("Other Asia Pacific"), to = "Asia Pacific")

    regions <- c("Africa", "Asia Pacific", "CIS", "Europe", "Middle East", "S & C America")

    x <- .disaggregate_regions(x, regions)

  }

  else if (subtype == "Capacity") {

    x <- .mergeReg(x, from = c("Other Europe"), to = "Europe")
    x <- .mergeReg(x, from = c("Other Middle East"), to = "Middle East")
    x <- .mergeReg(x, from = c("Other Africa"), to = "Africa")
    x <- .mergeReg(x, from = c("Other S & Cent America"), to = "S & C America")
    x <- .mergeReg(x, from = c("Other Asia Pacific"), to = "Asia Pacific")
    x <- .mergeReg(x, from = c("Other CIS"), to = "CIS")

    regions <- c("Africa", "Asia Pacific", "CIS", "Europe", "Middle East", "S & C America")

    x <- mbind(
      .disaggregate_regions(x[, , "Capacity|Solar (MW)"], regions),
      .disaggregate_regions(x[, , "Capacity|Wind (MW)"], regions),
      .disaggregate_regions(x[, , "Capacity|Geothermal (MW)"], regions)
    )
  }

  else if (subtype == "Generation") {

    x <- .mergeReg(x, from = c("Other Africa", "Other Northern Africa", "Other Southern Africa",
                               "Middle Africa", "Eastern Africa", "Western Africa"), to = "Africa")
    x <- .mergeReg(x, from = c("Other South America", "Other Caribbean", "Central America",
                               "Other S & Cent America"), to = "S & C America")
    x <- .mergeReg(x, from = c("Other Asia Pacific"), to = "Asia Pacific")
    x <- .mergeReg(x, from = c("Other CIS"), to = "CIS")
    x <- .mergeReg(x, from = c("Other Europe"), to = "Europe")
    x <- .mergeReg(x, from = c("Other Middle East"), to = "Middle East")

    regions <- c("Africa", "Asia Pacific", "CIS", "Europe", "Middle East", "S & C America")

    x <- mbind(
      .disaggregate_regions(x[,,"Generation|Wind (TWh)"], regions),
      .disaggregate_regions(x[,,"Generation|Solar (TWh)"], regions),
      .disaggregate_regions(x[,,"Generation|Hydro (TWh)"], regions),
      .disaggregate_regions(x[,,"Generation|Geo_biomass (TWh)"], regions),
      .disaggregate_regions(x[,,"Generation|Nuclear (TWh)"], regions),
      .disaggregate_regions(x[,,"Generation|Electricity (TWh)"], regions),
      .disaggregate_regions(x[,,"Generation|Electricity|Renewable (EJ)"], regions),
      .disaggregate_regions(x[,,"Generation|Electricity|Gas (TWh)"], regions),
      .disaggregate_regions(x[,,"Generation|Electricity|Oil (TWh)"], regions),
      .disaggregate_regions(x[,,"Generation|Electricity|Coal (TWh)"], regions)
    )

  }

  else if (subtype == "Production") {

    regions <- c("Africa", "Asia Pacific", "CIS", "Europe", "Middle East", "S & C America")

    x <- .mergeReg(x, from = c("Other Europe"), to = "Europe")
    x <- .mergeReg(x, from = c("Other Middle East"), to = "Middle East")
    x <- .mergeReg(x, from = c("Other Africa"), to = "Africa")
    x <- .mergeReg(x, from = c("Other S & Cent America"), to = "S & C America")
    x <- .mergeReg(x, from = c("Other Asia Pacific"), to = "Asia Pacific")
    x <- .mergeReg(x, from = c("Other CIS"), to = "CIS")

    x <- mbind(
      .disaggregate_regions(x[,,"Oil Production (million t)"], regions),
      .disaggregate_regions(x[,,"Coal Production (EJ)"], regions),
      .disaggregate_regions(x[,,"Coal Production (t)"], regions),
      .disaggregate_regions(x[,,"Gas Production (EJ)"], regions)
    )
  }

  else if (subtype == "Consumption") {

    regions <- c("Africa", "Asia Pacific", "CIS", "Europe", "Middle East", "S & C America")

    x <- .mergeReg(x, from = c("Other Europe"), to = "Europe")
    x <- .mergeReg(x, from = c("Other Middle East"), to = "Middle East")
    x <- .mergeReg(x, from = c("Other Northern Africa", "Other Southern Africa", "Middle Africa", "Eastern Africa", "Western Africa"), to = "Africa")
    x <- .mergeReg(x, from = c("Other South America", "Other Caribbean", "Central America"), to = "S & C America")
    x <- .mergeReg(x, from = c("Other Asia Pacific"), to = "Asia Pacific")
    x <- .mergeReg(x, from = c("Other CIS"), to = "CIS")

    x <- mbind(
      .disaggregate_regions(x[,,"Primary Energy Consumption (EJ)"], regions),
      .disaggregate_regions(x[,,"Liquids Consumption (kb/d)"], regions),
      .disaggregate_regions(x[,,"Oil Consumption (EJ)"], regions),
      .disaggregate_regions(x[,,"Gas Consumption (EJ)"], regions),
      .disaggregate_regions(x[,,"Coal Consumption (EJ)"], regions),
      .disaggregate_regions(x[,,"Solar Consumption (EJ)"], regions),
      .disaggregate_regions(x[,,"Wind Consumption (EJ)"], regions),
      .disaggregate_regions(x[,,"Nuclear Consumption (EJ)"], regions),
      .disaggregate_regions(x[,,"Hydro Consumption (EJ)"], regions)
    )
  }

  else if (subtype == "Trade Oil") {

    getItems(x, dim = 1) <- gsub("S & Cent America", "S & C America", getItems(x, dim = 1))

    trade.export.oil <- .removeNaRegions(x[, , "Trade|Export|Oil (kb/d)"])
    trade.import.oil <- .removeNaRegions(x[, , "Trade|Import|Oil (kb/d)"])

    # step 1: resolve to more fine granual regions based on detailed Oil Trade Data for 2019 and 2020 available

    # reference data
    trade.ref.export.oil <- new.magpie(getItems(x, dim = 1), getYears(x), "Trade|Export|Oil (kb/d)")
    trade.ref.export.oil[, , "Trade|Export|Oil (kb/d)"] <- x[, , "Trade|Export|Oil|Crude (kb/d)"] + x[, , "Trade|Export|Oil|Product (kb/d)"]
    trade.ref.export.oil <- .removeNaRegions(trade.ref.export.oil)
    trade.ref.export.oil <- trade.ref.export.oil[, c(2019, 2020), ]

    trade.ref.import.oil <- new.magpie(getItems(x, dim = 1), getYears(x), "Trade|Import|Oil (kb/d)")
    trade.ref.import.oil[, , "Trade|Import|Oil (kb/d)"] <- x[, , "Trade|Import|Oil|Crude (kb/d)"] + x[, , "Trade|Import|Oil|Product (kb/d)"]
    trade.ref.import.oil <- .removeNaRegions(trade.ref.import.oil)
    trade.ref.import.oil <- trade.ref.import.oil[, c(2019, 2020), ]

    reg2detailReg <- toolGetMapping("regionmappingBP_Oil_Region_To_DetailReg.csv", type = "regional", where = "mappingfolder")
    reg2detailReg.export <- filter(reg2detailReg, !!sym("Type") == "Export")
    reg2detailReg.import <- filter(reg2detailReg, !!sym("Type") == "Import")

    from_regions <- intersect(reg2detailReg.export$BP_Region, getItems(trade.export.oil, dim = 1))
    to_regions <- intersect(reg2detailReg.export$BP_Region_Detail, getItems(trade.ref.export.oil, dim = 1))
    unchanged_regions <- setdiff(getItems(trade.export.oil, dim = 1), from_regions)
    trade.ref.export.split <- toolAggregate(trade.export.oil[from_regions, , ],
      rel = reg2detailReg.export,
      weight = trade.ref.export.oil[to_regions, 2020, ]
    )

    x1 <- new.magpie(c(to_regions, unchanged_regions), getYears(x), "Trade|Export|Oil (kb/d)")
    # for 2019 and 2020 we use the detailed data
    x1[, c(2019, 2020), ] <- trade.ref.export.oil
    # for 1980 - 2018 we split some regions into more fine granular regions by 2020 data
    x1[unchanged_regions, seq(1980, 2018, 1), ] <- trade.export.oil[unchanged_regions, seq(1980, 2018, 1), ]
    x1[to_regions, seq(1980, 2018, 1), ] <- trade.ref.export.split[, seq(1980, 2018, 1), , ]

    from_regions <- intersect(reg2detailReg.import$BP_Region, getItems(trade.import.oil, dim = 1))
    to_regions <- intersect(reg2detailReg.import$BP_Region_Detail, getItems(trade.ref.import.oil, dim = 1))
    unchanged_regions <- setdiff(getItems(trade.import.oil, dim = 1), from_regions)
    trade.ref.import.split <- toolAggregate(trade.import.oil[from_regions, , ],
      rel = reg2detailReg.import,
      weight = trade.ref.import.oil[to_regions, 2020, ]
    )

    x2 <- new.magpie(c(to_regions, unchanged_regions), getYears(x), "Trade|Import|Oil (kb/d)")
    # for 2019 and 2020 we use the detailed data
    x2[, c(2019, 2020), ] <- trade.ref.import.oil
    # for 1980 - 2018 we split some regions into more fine granular regions by 2020 data
    x2[unchanged_regions, seq(1980, 2018, 1), ] <- trade.import.oil[unchanged_regions, seq(1980, 2018, 1), ]
    x2[to_regions, seq(1980, 2018, 1), ] <- trade.ref.import.split[, seq(1980, 2018, 1), , ]

    x.trade <- new.magpie(getItems(x1, dim = 1), getYears(x1), c("Trade|Import|Oil (kb/d)", "Trade|Export|Oil (kb/d)"))
    x.trade[, , "Trade|Export|Oil (kb/d)"] <- x1
    x.trade[, , "Trade|Import|Oil (kb/d)"] <- x2

    # step 2: aggregate to regions in regionmappingBP_Full.csv, then resolve regions

    # TODO: for better precision, create a direct, more fine-granular mapping from trade regions to countries

    x.trade <- .mergeReg(x.trade, from = c("East & S Africa", "North Africa", "West Africa"), to = "Africa")
    x.trade <- .mergeReg(x.trade, from = c("Australasia", "Other Asia Pacific"), to = "Asia Pacific")
    x.trade <- .mergeReg(x.trade, from = c("Other CIS"), to = "CIS")
    x.trade <- .mergeReg(x.trade, from = c("Other Middle East"), to = "Middle East")

    oil_regions <- c("Africa", "Asia Pacific", "CIS", "Middle East", "S & C America", "Europe")
    x <- .disaggregate_regions(x.trade, oil_regions)
  }

  else if (subtype == "Trade Gas") {
    x <- .mergeReg(x, from = c("Other Asia", "OECD Asia"), to = "Asia Pacific")
    x <- .mergeReg(x, from = c("Other CIS"), to = "CIS")
    x <- .mergeReg(x, from = c("Other North America"), to = "North America")
    x <- .mergeReg(x, from = c("Other S & C America"), to = "S & C America")

    gas_regions <- c("Africa", "Asia Pacific", "CIS", "Middle East", "S & C America", "Europe", "North America")
    x <- .disaggregate_regions(x, gas_regions)
  }

  else if (subtype == "Trade Coal") {
    getItems(x, dim = 1) <- gsub("S & Cent America", "S & C America", getItems(x, dim = 1))

    trade.export.coal <- .removeNaRegions(x[, , "Trade|Export|Coal (EJ)"])
    trade.export.coal <- .mergeReg(trade.export.coal, from = c("Other Asia Pacific"), to = "Asia Pacific")
    trade.export.coal <- .mergeReg(trade.export.coal, from = c("Other CIS"), to = "CIS")
    trade.export.coal <- .mergeReg(trade.export.coal, from = c("Other Africa"), to = "Africa")

    coal_regions <- c("Africa", "Asia Pacific", "CIS", "Europe")
    trade.export.coal <- .disaggregate_regions(trade.export.coal, coal_regions)

    trade.import.coal <- .removeNaRegions(x[, , "Trade|Import|Coal (EJ)"])
    trade.import.coal <- .mergeReg(trade.import.coal, from = c("Other Asia Pacific"), to = "Asia Pacific")

    coal_regions <- c("Africa", "Asia Pacific", "CIS", "Middle East", "S & C America", "Europe")
    trade.import.coal <- .disaggregate_regions(trade.import.coal, coal_regions)

    x <- mbind(trade.export.coal, trade.import.coal)
  }

  else if (subtype == "Price") {
    x.price <- new.magpie(getISOlist(), getYears(x), getNames(x))
    x.price[getISOlist(), , ] <- x["GLO", , ]

    mapping <- toolGetMapping("regionmappingH12.csv", where = "mappingfolder")
    caz <- mapping[mapping$RegionCode == "CAZ", "CountryCode"]
    oas <- mapping[mapping$RegionCode == "OAS", "CountryCode"]
    eur <- setdiff(mapping[mapping$RegionCode == "EUR", "CountryCode"], "GBR")

    # specific region mapping for gas prices:
    # Japan -> JPN, Korea -> OAS, average (Netherlands, Germany) -> EUR, US-> USA, Can -> CAZ
    price.gas <- new.magpie(c(oas, eur, caz, "USA", "GBR", "JPN"), getYears(x.price), "Price|Natural Gas ($/mbtu)")
    price.gas["JPN", , "Price|Natural Gas ($/mbtu)"] <- x.price["JPN", , "Price|LNG|Japan|CIF ($/mbtu)"]
    price.gas[oas, , "Price|Natural Gas ($/mbtu)"] <- x.price[oas, , "Price|LNG|Japan|Korea Marker ($/mbtu)"]
    price.gas[eur, , "Price|Natural Gas ($/mbtu)"] <- (x.price[eur, , "Price|Natural Gas|Netherlands TTF DA Heren Index ($/mbtu)"] +
      x.price[eur, , "Price|Natural Gas|Avg German Import Price ($/mbtu)"]) / 2
    price.gas["GBR", , "Price|Natural Gas ($/mbtu)"] <- x.price["GBR", , "Price|Natural Gas|UK Heren NBP Index ($/mbtu)"]
    price.gas["USA", , "Price|Natural Gas ($/mbtu)"] <- x.price["USA", , "Price|Natural Gas|US Henry Hub ($/mbtu)"]
    price.gas[caz, , "Price|Natural Gas ($/mbtu)"] <- x.price[caz, , "Price|Natural Gas|Alberta ($/mbtu)"]
    price.gas <- toolCountryFill(price.gas, fill = 0, verbosity = 2)

    # specific region mapping for coal prices:
    # Japan steam spot -> JPN, Asian marker -> OAS, IND, NW EU -> EUR, US-> USA, Chn -> CHN
    price.coal <- new.magpie(c(oas, eur, "USA", "GBR", "JPN", "IND", "CHN"), getYears(x.price), "Price|Coal ($/t)")
    price.coal["JPN", , "Price|Coal ($/t)"] <- x.price["JPN", , "Price|Coal|Japan steam spot CIF price ($/t)"]
    price.coal[c(oas, "IND"), , "Price|Coal ($/t)"] <- x.price[c(oas, "IND"), , "Price|Coal|Asian marker price (t/$)"]
    price.coal[c(eur, "GBR"), , "Price|Coal ($/t)"] <- x.price[c(eur, "GBR"), , "Price|Coal|Northwest Europe marker price ($/t)"]
    price.coal["USA", , "Price|Coal ($/t)"] <- x.price["USA", , "Price|Coal|US Central Appalachian coal spot price index ($/t)"]
    price.coal["CHN", , "Price|Coal ($/t)"] <- x.price["CHN", , "Price|Coal|China Qinhuangdao spot price ($/t)"]
    price.coal <- toolCountryFill(price.coal, fill = 0, verbosity = 2)

    x <- mbind(x.price, price.gas, price.coal)
  }

  else {
    stop("Not a valid subtype!")
  }

  getSets(x) <- c("region", "year", "data")

  return(x)
}
