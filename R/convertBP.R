#' Disaggregates and cleans BP data.
#' @param x MAgPIE object to be converted
#' @param subtype Either "Capacity", "Generation", "Production", "Consumption", "Trade Oil", "Trade Gas", "Trade Coal"
#' @description Disaggregates historical - capacity, generation, and production data.
#' @return A magpie object with historical electricity capacities (in MW for Wind, Solar, and Geothermal),
#' Generation (in TWh for Nuclear, Hydro, Wind, Solar, and Geo Biomass), AND
#' Production (in EJ for Coal, Oil, and Gas, additionally in tonnes for coal)
#' @author Aman Malik, Falk Benke
#' @importFrom dplyr filter
#' @importFrom madrat magpply toolGetMapping toolCountryFill
#'
#'

convertBP <- function(x, subtype) {
  .removeNaRegions <- function(x) {
    remove <- magpply(x, function(y) all(is.na(y)), MARGIN = 1)
    return(x[!remove, , ])
  }

  # TODO: use everywhere
  .mergeReg <- function(x, from, to) {
    x1 <- mbind(
      new.magpie(to, getYears(x), getNames(x), fill = dimSums(x[from, , ], dim = 1, na.rm = T)),
      x[from, , invert = T]
    )
    return(x1)
  }

  # TODO: consider refactoring/removing redundancies
  # TODO: consider unifying/improving country mapping for Capacity, Generation, Production,

  Region_name <- NULL
  ISO_code <- NULL


  if (subtype == "Capacity") {

    # Regions to be disaggregated
    other_regions <- c(
      "Other Europe", "Other Middle East", "Other Africa",
      "Other S & Cent America", "Other Asia Pacific", "Other CIS"
    )

    .disaggegate_others <- function(x, variable) {
      mapping_capacity <- toolGetMapping("regionmappingBP_Full.csv", type = "regional")

      countries_africa <- mapping_capacity$ISO3.code[mapping_capacity$Region_name == "Africa"]
      countries_asia <- mapping_capacity$ISO3.code[mapping_capacity$Region_name == "Asia Pacific"]
      countries_middle_east <- mapping_capacity$ISO3.code[mapping_capacity$Region_name == "Middle East"]
      countries_sc_america <- mapping_capacity$ISO3.code[mapping_capacity$Region_name == "S & C America"]
      countries_europe <- mapping_capacity$ISO3.code[mapping_capacity$Region_name == "Europe"]
      countries_cis <- mapping_capacity$ISO3.code[mapping_capacity$Region_name == "CIS"]

      # removing countries where capacity = NA, not in the original database
      na_countries <- getRegions(x)[is.na(x[, 2020, variable])]

      # All countries with positive value of capacity in 2020 and not in other_regions
      country2iso <- toolCountry2isocode(getRegions(x[c(other_regions, na_countries), , , invert = TRUE]))

      # Finding all countries in "Other_*", i.e. n
      other_africa <- setdiff(countries_africa, country2iso)
      other_africa <- data.frame(Region_name = "Other Africa", ISO_code = other_africa)
      other_asia <- setdiff(countries_asia, country2iso)
      other_asia <- data.frame(Region_name = "Other Asia Pacific", ISO_code = other_asia)
      other_middle_east <- setdiff(countries_middle_east, country2iso)
      other_middle_east <- data.frame(Region_name = "Other Middle East", ISO_code = other_middle_east)
      other_sc_america <- setdiff(countries_sc_america, country2iso)
      other_sc_america <- data.frame(Region_name = "Other S & Cent America", ISO_code = other_sc_america)
      other_europe <- setdiff(countries_europe, country2iso)
      other_europe <- data.frame(Region_name = "Other Europe", ISO_code = other_europe)
      other_cis <- setdiff(countries_cis, country2iso)
      other_cis <- data.frame(Region_name = "Other CIS", ISO_code = other_cis)

      # Combining all countries (ISO coded) "others" in one file
      mainmappingfile <- rbind(other_cis, other_africa, other_asia, other_middle_east, other_sc_america, other_europe)
      mainmappingfile <- filter(mainmappingfile, ISO_code != "SUN")

      # Downscaling all "Other_*" into respective countries
      PE <- calcOutput("PE", aggregate = FALSE)[mainmappingfile$ISO_code, 2016, "PE (EJ/yr)"]

      output <- toolAggregate(x[other_regions, , variable], rel = mainmappingfile, weight = PE)
      output[is.na(output)] <- 0
      output <- toolCountryFill(output, fill = 0)
      return(output)
    }

    # Substituting certain country names
    getRegions(x) <- gsub("\\bUS\\b", "USA", getRegions(x))

    output_solar <- .disaggegate_others(x, variable = "Capacity|Solar (MW)")
    output_wind <- .disaggegate_others(x, variable = "Capacity|Wind (MW)")
    output_geothermal <- .disaggegate_others(x, variable = "Capacity|Geothermal (MW)")

    # Removing others and adding
    x <- x[other_regions, , invert = TRUE]
    getRegions(x) <- toolCountry2isocode(getRegions(x))
    x <- toolCountryFill(x, fill = 0)
    # set all NA to 0
    x[is.na(x)] <- 0

    x[, , "Capacity|Solar (MW)"] <- x[, , "Capacity|Solar (MW)"] + output_solar
    x[, , "Capacity|Wind (MW)"] <- x[, , "Capacity|Wind (MW)"] + output_wind
    x[, , "Capacity|Geothermal (MW)"] <- x[, , "Capacity|Geothermal (MW)"] + output_geothermal
  }

  if (subtype == "Generation") {

    # IMPORTANT NOTE: Generation Data has not been disaggregated for USSR before 1991.

    other_regions <- c(
      "Other Europe", "Other Middle East", "Other Africa",
      "Other S & Cent America", "Other Asia Pacific", "Other CIS"
    )

    getRegions(x) <- gsub("\\bUS\\b", "USA", getRegions(x))
    getRegions(x) <- gsub(pattern = "China Hong Kong SAR", "Hong Kong", x = getRegions(x))

    other_africas <- c("Other Northern Africa", "Other Southern Africa", "Middle Africa", "Eastern Africa", "Western Africa")
    x["Other Africa", , ] <- dimSums(x[c(other_africas, "Other Africa"), , ], na.rm = T, dim = 1)
    x <- x[other_africas, , invert = T]

    other_samicas <- c("Other South America", "Other Caribbean", "Central America")
    x["Other S & Cent America", , ] <- dimSums(x[c(other_samicas, "Other S & Cent America"), , ], dim = 1, na.rm = T)
    x <- x[other_samicas, , invert = T]

    # Downscaled regions in ISO3
    mapping <- toolGetMapping("regionmappingBP_Other.csv", type = "regional")

    PE <- calcOutput("PE", aggregate = FALSE)[unique(mapping$ISO.Code), 2016, "PE (EJ/yr)"]

    x_row <- toolAggregate(x[other_regions, , ], rel = mapping, weight = PE)
    x_row <- toolCountryFill(x_row, fill = 0)
    x_row[is.na(x_row)] <- 0

    x <- x[other_regions, , invert = TRUE]
    getRegions(x) <- toolCountry2isocode(getRegions(x))
    x <- toolCountryFill(x, fill = 0)
    x[is.na(x)] <- 0

    # Combine the two objects containing normal and disaggregated data
    x <- x + x_row
  }

  if (subtype == "Production") {
    other_regions <- c(
      "Other Europe", "Other Middle East", "Other Africa",
      "Other S & Cent America", "Other Asia Pacific", "Other CIS"
    )

    x <- x[c("OPEC", "Non-OPEC"), , invert = T]
    getRegions(x) <- gsub("\\bUS\\b", "USA", getRegions(x))

    mapping <- toolGetMapping("regionmappingBP_Other.csv", type = "regional")

    PE <- calcOutput("PE", aggregate = FALSE)[mapping$ISO.Code, 2016, "PE (EJ/yr)"]

    x_row <- toolAggregate(x = x[other_regions, , ], rel = mapping, weight = PE)
    x_row <- toolCountryFill(x_row, fill = 0)
    x_row[is.na(x_row)] <- 0

    x <- x[other_regions, , invert = TRUE]
    getRegions(x) <- toolCountry2isocode(getRegions(x))
    x <- toolCountryFill(x, fill = 0)
    x[is.na(x)] <- 0

    # Combine the two objects containing normal and disaggregated data
    x <- x + x_row
  }

  if (subtype == "Consumption") {

    # IMPORTANT NOTE: Generation Data has not been disaggregated for USSR before 1991.

    other_regions <- c(
      "Other Europe", "Other Middle East", "Other Africa",
      "Other S & Cent America", "Other Asia Pacific", "Other CIS"
    )

    getRegions(x) <- gsub("\\bUS\\b", "USA", getRegions(x))
    getRegions(x) <- gsub(pattern = "China Hong Kong SAR", "Hong Kong", x = getRegions(x))

    other_samicas <- c("Other South America", "Other Caribbean", "Central America")
    x <- mbind(
      new.magpie("Other S & Cent America", getYears(x), getNames(x), fill = dimSums(x[other_samicas, , ], dim = 1, na.rm = T)),
      x[other_samicas, , invert = T]
    )

    other_africas <- c("Other Northern Africa", "Other Southern Africa", "Middle Africa", "Eastern Africa", "Western Africa")
    x <- mbind(
      new.magpie("Other Africa", getYears(x), getNames(x), fill = dimSums(x[other_africas, , ], dim = 1, na.rm = T)),
      x[other_africas, , invert = T]
    )

    mapping <- toolGetMapping("regionmappingBP_Other.csv", type = "regional")

    PE <- calcOutput("PE", aggregate = FALSE)[unique(mapping$ISO.Code), 2016, "PE (EJ/yr)"]

    x_row <- toolAggregate(x[other_regions, , ], rel = mapping, weight = PE)
    x_row <- toolCountryFill(x_row, fill = 0)
    x_row[is.na(x_row)] <- 0

    x <- x[other_regions, , invert = TRUE]
    getRegions(x) <- toolCountry2isocode(getRegions(x))
    x <- toolCountryFill(x, fill = 0)
    x[is.na(x)] <- 0

    # Combine the two objects containing normal and disaggregated data
    x <- x + x_row
  }

  if (subtype == "Trade Oil") {
    
    getRegions(x) <- gsub("\\bUS\\b", "USA", getRegions(x))
    getRegions(x) <- gsub("S & Cent America", "S & C America", getRegions(x))

    # for now, we exclude exports from Sowjet Union (recorded until 1993)
    # TODO include exports from Sowjet Union until 1993 to ensure import-export balance between 1990 and 1993
    x <- x["USSR & Central Europe", , invert = T]

    trade.export.oil <- .removeNaRegions(x[, , "Trade|Export|Oil (kb/d)"])
    trade.import.oil <- .removeNaRegions(x[, , "Trade|Import|Oil (kb/d)"])

    # step 1: resolve to more fine granual regions based on detailed Oil Trade Data for 2019 and 2020

    # reference data
    trade.ref.export.oil <- new.magpie(getRegions(x), getYears(x), "Trade|Export|Oil (kb/d)")
    trade.ref.export.oil[, , "Trade|Export|Oil (kb/d)"] <- x[, , "Trade|Export|Oil|Crude (kb/d)"] + x[, , "Trade|Export|Oil|Product (kb/d)"]
    trade.ref.export.oil <- .removeNaRegions(trade.ref.export.oil)
    trade.ref.export.oil <- trade.ref.export.oil[, c(2019, 2020), ]
    
    trade.ref.import.oil <- new.magpie(getRegions(x), getYears(x), "Trade|Import|Oil (kb/d)")
    trade.ref.import.oil[, , "Trade|Import|Oil (kb/d)"] <- x[, , "Trade|Import|Oil|Crude (kb/d)"] + x[, , "Trade|Import|Oil|Product (kb/d)"]
    trade.ref.import.oil <- .removeNaRegions(trade.ref.import.oil)
    trade.ref.import.oil <- trade.ref.import.oil[, c(2019, 2020), ]

    reg2detailReg <- toolGetMapping("regionmappingBP_Oil_Region_To_DetailReg.csv", type = "regional")
    reg2detailReg.export <- filter(reg2detailReg, !!sym("Type") == "Export")
    reg2detailReg.import <- filter(reg2detailReg, !!sym("Type") == "Import")
    
    from_regions <- intersect(reg2detailReg.export$BP_Region, getRegions(trade.export.oil))
    to_regions <- intersect(reg2detailReg.export$BP_Region_Detail, getRegions(trade.ref.export.oil))
    unchanged_regions <- setdiff(getRegions(trade.export.oil), from_regions)
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
    
    from_regions <- intersect(reg2detailReg.import$BP_Region, getRegions(trade.import.oil))
    to_regions <- intersect(reg2detailReg.import$BP_Region_Detail, getRegions(trade.ref.import.oil))
    unchanged_regions <- setdiff(getRegions(trade.import.oil), from_regions)
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
    
    x.trade <- new.magpie(getRegions(x1), getYears(x1), c("Trade|Import|Oil (kb/d)", "Trade|Export|Oil (kb/d)"))
    x.trade[,,"Trade|Export|Oil (kb/d)"] <- x1
    x.trade[,,"Trade|Import|Oil (kb/d)"] <- x2
        
    # step 2: aggregate to regions in regionmappingBP_Full.csv, then resolve

    # TODO: for better precision, create a direct, more fine-granular mapping from trade regions to countries

    x.trade <- .mergeReg(x.trade, from = c("East & S Africa", "North Africa", "West Africa"), to = "Africa")
    x.trade <- .mergeReg(x.trade, from = c("Australasia", "Other Asia Pacific"), to = "Asia Pacific")
    x.trade <- .mergeReg(x.trade, from = c("Other CIS"), to = "CIS")
    x.trade <- .mergeReg(x.trade, from = c("Other Middle East"), to = "Middle East")

    # get iso countries in source
    ctry <- toolCountry2isocode(getRegions(x.trade), warn = F)
    ctry <- ctry[!is.na(ctry)]

    mapping_full <- toolGetMapping("regionmappingBP_Full.csv", type = "regional")
    oil_regions <- c("Africa", "Asia Pacific", "CIS", "Middle East", "S & C America", "Europe")
    
    # exclude countries from mapping that are explicitly listed in source
    mapping_full <- mapping_full[mapping_full$Region_name %in% oil_regions & !mapping_full$ISO3.code %in% ctry & mapping_full$ISO3.code != "SUN",]
    PE <- calcOutput("PE", aggregate = FALSE)[mapping_full$ISO3.code, 2016, "PE (EJ/yr)"]

    x2 <- toolAggregate(x.trade[oil_regions, , ], rel = mapping_full, weight = PE)
    x2 <- toolCountryFill(x2, fill = 0)
    x2[is.na(x2)] <- 0

    x1 <- x.trade[oil_regions, , invert = TRUE]
    getRegions(x1) <- toolCountry2isocode(getRegions(x1), warn = F)
    x1 <- toolCountryFill(x1, fill = 0)
    x1[is.na(x1)] <- 0

    # Combine the two objects containing normal and disaggregated data
    x <- x1 + x2
  }
  
  if ( subtype == "Trade Gas") {
    
    x <- .mergeReg(x, from = c("Other Asia", "OECD Asia"), to = "Asia Pacific")
    x <- .mergeReg(x, from = c("Other CIS"), to = "CIS")
    x <- .mergeReg(x, from = c("Other North America"), to = "North America")
    x <- .mergeReg(x, from = c("Other S & C America"), to = "S & C America")
    
    ctry <- toolCountry2isocode(getRegions(x), warn = F)
    ctry <- ctry[!is.na(ctry)]
    
    mapping_full <- toolGetMapping("regionmappingBP_Full.csv", type = "regional")
    gas_regions <- c("Africa", "Asia Pacific", "CIS", "Middle East", "S & C America", "Europe", "North America")
    
    # exclude countries from mapping that are explicitly listed in source
    mapping_full <- mapping_full[mapping_full$Region_name %in% gas_regions & !mapping_full$ISO3.code %in% ctry & mapping_full$ISO3.code != "SUN",]
    PE <- calcOutput("PE", aggregate = FALSE)[mapping_full$ISO3.code, 2016, "PE (EJ/yr)"]
    
    x2 <- toolAggregate(x[gas_regions, , ], rel = mapping_full, weight = PE)
    x2 <- toolCountryFill(x2, fill = 0)
    x2[is.na(x2)] <- 0
    
    x1 <- x[gas_regions, , invert = TRUE]
    getRegions(x1) <- toolCountry2isocode(getRegions(x1), warn = F)
    x1 <- toolCountryFill(x1, fill = 0)
    x1[is.na(x1)] <- 0
    
    # Combine the two objects containing normal and disaggregated data
    x <- x1 + x2
  }

  getSets(x) <- c("region", "year", "data")

  return(x)
}
