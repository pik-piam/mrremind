#' Calculate air pollutant emissions for a reference year, for use
#' in combination with GAINS data at different sectoral aggregations
#'
#' @param subtype total per pollutant ("total"), raw CEDS sectors ("sectorsCEDSraw"),
#' aggregated to CEDS16 ("sectorsCEDS16") or mapped to GAINS2025 sectors ("sectorsGAINS2025")
#' @param baseyear year to take as a reference from CEDS, ignored for the EDGAR2005 LUC CO2 emissions
#' @param outunits "Mt/yr" or "kt/yr"
#' @param namesformat "GAINS2025" or "REMIND" or "REMINDexo", the standard to use for pollutant names
#' @return magclass object
#' @author Gabriel Abrahao
#' @importFrom magclass getNames<- getYears<-

calcAirPollEmiRef <- function(
    subtype = "total", baseyear = 2015, outunits = "Mt/yr", namesformat = "GAINS2025", useyearmean = FALSE) {
  # Mapping from GAINS to CEDS2025 pollutant names
  polnamesmap <- c(
    "CO" = "co",
    "NOX" = "no2_n",
    "PM_BC" = "bc_c",
    "PM_OC" = "oc_c",
    "SO2" = "so2",
    "NH3" = "nh3_n",
    "VOC" = "nmvoc"
  )

  if (useyearmean) {
    useyears <- seq(baseyear - 2, baseyear + 2)
  } else {
    useyears <- baseyear
  }

  # Reading CEDS2025 emissions. TODO: Check units
  # Subsetting with polnamesmap already ensures the right order
  fullceds <- readSource("CEDS2025")[, useyears, polnamesmap]
  getItems(fullceds, 3.2) <- names(polnamesmap)
  getSets(fullceds)[getSets(fullceds) == "pollutant"] <- "species"

  if (useyearmean) {
    fullceds <- setYears(magpply(X = fullceds, FUN = mean, DIM = 2, na.rm = TRUE), baseyear)
  }

  # Converting units. GAINS seems to be in kt of each pollutant,
  # CEDS is in Mt, but NOx and NH3 are ostensibly in MtN
  if (outunits == "kt/yr") {
    fullceds[, , ] <- fullceds[, , ] * 1e3
  }
  # Assuming NO2, ktN to ktNO2
  fullceds[, , "NOX"] <- fullceds[, , "NOX"] * (14 + 16 + 16) / 14
  # ktN to ktNH3 (actually makes for a worse fit with GAINS)
  fullceds[, , "NH3"] <- fullceds[, , "NH3"] * (14 + 3) / 14

  fixPolNames <- function(mag, fmt = "REMIND") {
    # Mapping from GAINS2025 to REMIND (oldGAINS) pollutant names
    if (fmt == "REMIND") {
      polnamesmap <- c(
        "CO" = "CO",
        "NOx" = "NOX",
        "BC" = "PM_BC",
        "OC" = "PM_OC",
        "SO2" = "SO2",
        "NH3" = "NH3",
        "VOC" = "VOC"
      )
    } else if (fmt == "REMINDexo") {
      polnamesmap <- c(
        "CO" = "CO",
        "NOx" = "NOX",
        "BC" = "PM_BC",
        "OC" = "PM_OC",
        "SOx" = "SO2",
        "NH3" = "NH3",
        "NMVOC" = "VOC"
      )
    }
    mag <- mag[, , polnamesmap]
    getItems(mag, "species") <- names(polnamesmap)
    return(mag)
  }

  if (namesformat %in% c("REMIND", "REMINDexo")) {
    fullceds <- fixPolNames(fullceds, namesformat)
  }

  if (subtype == "total") {
    totceds <- dimSums(fullceds, dim = 3.1, na.rm = TRUE)

    out <- setYears(totceds)
    unit <- outunits
    desc <- paste0("Emissions in year ", baseyear)
  } else if (subtype == "sectorsCEDSraw") {
    out <- setYears(fullceds)
    unit <- outunits
    desc <- paste0("Emissions in year ", baseyear)
  } else if (subtype == "sectorsCEDS16") {
    # CEDS Sectoral mapping to aggregated CEDS sectors
    cedsecmap <- toolGetMapping(type = "sectoral", name = "mappingCEDS62toCEDS16.csv", where = "mrremind")

    fullceds16 <- toolAggregate(
      fullceds[, , "6B_Other-not-in-total", invert = TRUE], cedsecmap,
      from = "CEDS62", to = "CEDS16",
      weight = NULL, dim = 3.1
    )

    out <- setYears(fullceds16)
    unit <- outunits
    desc <- paste0("Emissions in year ", baseyear)
  } else if (subtype == "sectorsGAINS2025") {
    # CEDS Sectoral mapping to aggregated GAINS sectors
    gainsmap <- toolGetMapping(type = "sectoral", name = "mappingCEDS62toGAINSsectors.csv", where = "mrremind")

    # GAINS baseline scenario emissions and activities
    # Emissions seem to be in kt(pollutant)/yr, while activities have the units in seclist (mostly PJ or Mt/yr)
    # We need the "aggregated" sectoral aggregation, extended with a few sectors from "detailed"
    gainsemi_agg <- readSource("GAINS2025", subtype = "emissions", subset = "baseline.agg")
    gainsemi_det <- readSource("GAINS2025", subtype = "emissions", subset = paste0("baseline.", "det"))
    # Sectors to extend, if present in the detailed datasets
    extsectors <- c(
      "Waste_Solid_Industrial", "Waste_Solid_Municipal", "Waste_Water_Industrial", "Waste_Water_Municipal"
    )
    extsectors <- intersect(getItems(gainsemi_det, "sectorGAINS"), extsectors)
    gainsemi_ext <- mbind(gainsemi_agg, gainsemi_det[, , extsectors])

    # Recursively call the function with the "total" subtype, to disaggregate
    # the GAINS emissions used as weights into ISO countries
    totceds <- calcOutput("AirPollEmiRef", baseyear = baseyear, aggregate = FALSE)

    # Disaggregate GAINS emissions into ISO countries and subset the year in question
    regmap <- toolGetMapping(type = "regional", name = "regionmapping_GAINS2025.csv", where = "mrremind")
    gainsemi <- toolAggregate(
      gainsemi_ext[, baseyear, ], regmap,
      from = "gainscode", to = "CountryCode",
      weight = totceds, dim = 1, wdim = 1
    )
    getSets(gainsemi)[getSets(gainsemi) == "region"] <- "iso3"

    # Fill NAs with zeros in the GAINS emissions data
    gainsemi[is.na(gainsemi)] <- 0

    # If we're using a different pollutant names setup, fixPolNames should also work in gainsemi
    if(namesformat != "GAINS2025") {
      gainsemi <- fixPolNames(gainsemi)
    }

    # Rename the "sector dimension to match that used in other functions
    # that operate on GAINS
    getSets(fullceds)[getSets(fullceds) == "sector"] <- "sectorGAINS"


    # toolAggregate apparently gets confused if the mapping isn't
    # subset exactly to the sectors present in the data. For
    # some reason, just completing the data with complete_magpie
    # is not enough.
    # So here we loop through pollutant species and apply the mapping
    # separately, accumulating them in a list before joining
    # it all again
    allspecies <- getItems(fullceds, "species")

    # Store results in a list first, so the actual concatenation
    # can be done in one go
    listcedsgains <- list()
    for (usespecies in allspecies) {
      # Drop species dimension, add it back in the result later
      useceds <- collapseDim(fullceds[, , usespecies])
      usegains <- collapseDim(gainsemi[, , usespecies])

      # Subset the mapping for that particular pollutant
      usegainsmap <- gainsmap[gainsmap$CEDS62 %in% getItems(useceds, "sectorGAINS"), ]
      usegainsmap <- gainsmap[gainsmap$GAINS2025 %in% getItems(usegains, "sectorGAINS"), ]

      # And subset the arrays using the mapping
      useceds <- useceds[, , unique(usegainsmap$CEDS62)]
      usegains <- usegains[, , unique(usegainsmap$GAINS2025)]

      # Apply mapping to GAINS sectors, using GAINS emissions as weights.
      # As long as the mapping follows certain characteristics, mainly
      # being able to be described fully using the INTERMEDIARY sectors
      # in the mapping, this should ensure that:
      # - Aggregating emissions follow CEDS
      # - In each INTERMEDIARY secor, the proportion of GAINS emissions
      #   is preserved in the final output
      usecedsgains <- toolAggregate(
        useceds,
        usegainsmap[, c("CEDS62", "GAINS2025")],
        from = "CEDS62", to = "GAINS2025",
        weight = usegains,
        dim = 3.1,
        wdim = 3.1,
        zeroWeight = "allow"
      )

      # Add the pollutant species dimension
      usecedsgains <- add_dimension(usecedsgains, 3.2, "species", usespecies)

      # Append to the list
      listcedsgains[[length(listcedsgains) + 1]] <- usecedsgains
    }

    # Concatenate all species in one go
    fullcedsgains <- mbind(listcedsgains)

    # Ensure all pollutants have all the sectors, filled with zeros
    fullcedsgains <- complete_magpie(fullcedsgains, fill = 0)

    out <- setYears(fullcedsgains)
    unit <- outunits
    desc <- paste0("Emissions in year ", baseyear)
  }


  return(list(
    x = out,
    weight = NULL,
    unit = unit,
    description = desc
  ))
}
