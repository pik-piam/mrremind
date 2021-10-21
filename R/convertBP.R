#' Disaggregates and cleans BP data.
#' @param x MAgPIE object to be converted
#' @param subtype Either "Capacity" or "Generation"
#' @description Disaggregates historical - capacity, generation, and production data.
#' @return A magpie object with historical electricity capacities (in MW for Wind, Solar, and Geothermal),
#' Generation (in TWh for Nuclear, Hydro, Wind, Solar, and Geo Biomass), AND
#' Production (in EJ for Coal, Oil, and Gas, additionally in tonnes for coal)
#' @author Aman Malik
#' @importFrom dplyr filter
#' @importFrom madrat toolGetMapping toolCountryFill
#'
#'

convertBP <- function(x, subtype) {

  Region_name <- NULL
  ISO_code <- NULL


  if (subtype == "Capacity") {

    # Regions to be disaggregated
    other_regions <- c(
      "Other Europe", "Other Middle East", "Other Africa",
      "Other S & Cent America", "Other Asia Pacific", "Other CIS"
    )

    .disaggegate_others <- function(x, variable) {
      mapping_capacity <- toolGetMapping("regionmappingBP_Capacity.csv", type = "regional")

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
    mapping <- toolGetMapping("regionmappingBP.csv", type = "regional")

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

    mapping <- toolGetMapping("regionmappingBP.csv", type = "regional")

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

    mapping <- toolGetMapping("regionmappingBP.csv", type = "regional")

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
  
  getSets(x) <- c("region", "year", "data")
  
  return(x)
}
