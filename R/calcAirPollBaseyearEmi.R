#' Calculate air pollutant emissions for a reference year
#' at different sectoral aggregation levels
#' based on CEDS2025 and GAINS2025 data.
#'
#'
#' @param baseyear base year for which emissions are calculated
#' @param CEDS.5yearmean computes 5-year average around base year for CEDS2025
#'                ("TRUE"/"FALSE")
#' @param source "CEDS2025" or "GAINS2025"
#' @param outsectors total ("TOT"),
#'                   62 CEDS sectors ("CEDS"),
#'                   16 intermediary sectors used to link CEDS and GAINS ("INT"),
#'                   35 GAINS sectors ("GAINS"),
#'                   13 CMIP7 Harmonization sectors ("CMIP7")
#' @return magclass object
#' @author Gabriel Abrahao, Laurin Koehler-Schindler

calcAirPollBaseyearEmi <- function(baseyear = 2020, CEDS.5yearmean = TRUE,
                                   source = "CEDS2025", outsectors = "GAINS") {
  # READ CEDS2025 DATA AT CEDS62 sectoral aggregation ===========================
  # Define years to be read
  if (CEDS.5yearmean) {
    useyears <- seq(baseyear - 2, baseyear + 2)
  } else {
    useyears <- baseyear
  }
  # Mapping from CEDS2025 to REMIND (oldGAINS) air pollutant names
  polnamesmap <- c(
    "CO" = "co",
    "NOx" = "no2_n",
    "BC" = "bc_c",
    "OC" = "oc_c",
    "SO2" = "so2",
    "NH3" = "nh3_n",
    "VOC" = "nmvoc"
  )
  # READ CEDS2025 EMISSIONS
  fullceds <- readSource("CEDS2025")[, useyears, polnamesmap]
  # Change air pollutant names to REMIND (oldGAINS) and dimension name to "species"
  # Subsetting with polnamesmap ensured the right order
  getItems(fullceds, 3.2) <- names(polnamesmap)
  getSets(fullceds)[getSets(fullceds) == "pollutant"] <- "species"
  getSets(fullceds)[getSets(fullceds) == "iso3"] <- "region"

  # Compute 5-year average around base year
  if (CEDS.5yearmean) {
    fullceds <- setYears(magpply(X = fullceds, FUN = mean, DIM = 2, na.rm = TRUE), baseyear)
    getSets(fullceds)[getSets(fullceds) == "d2"] <- "year"
  }

  # NA HANDLING
  fullceds[is.na(fullceds)] <- 0

  # UNIT CONVERSION - TO BE CHECKED
  # CEDS is already in Mt, but NOx and NH3 are ostensibly in MtN
  # Assuming NO2, ktN to ktNO2
  fullceds[, , "NOx"] <- fullceds[, , "NOx"] * (14 + 16 + 16) / 14
  # ktN to ktNH3 (actually makes for a worse fit with GAINS)
  fullceds[, , "NH3"] <- fullceds[, , "NH3"] * (14 + 3) / 14

  # READ GAINS2025 BASELINE EMISSIONS ===========================================
  # AT GAINS2025 sectoral aggregation (35 mixed sectors) ========================

  fullgains <- readSource("GAINS2025final", subtype = "emissions")[, baseyear, "baseline.baseline"]
  # Remove ssp and scenario dimension
  fullgains <- collapseDim(fullgains, dim = c("ssp", "scenario"))
  getSets(fullgains)[getSets(fullgains) == "sectorGAINS"] <- "sector"

  # NA HANDLING
  fullgains[is.na(fullgains)] <- 0

  # UNIT CONVERSION - TO BE CHECKED
  # Converting units to Mt. GAINS seems to be in kt of each pollutant.
  fullgains[, , ] <- fullgains[, , ] / 1e3

  # =============================================================================
  # GET MAPPINGS ================================================================
  # =============================================================================

  gainsmap <- toolGetMapping(type = "sectoral", name = "mappingINTERMEDIARYtoGAINS2025toREMINDtoIAMC.csv", where = "mrremind")
  cedsmap <- toolGetMapping(type = "sectoral", name = "mappingCEDS62toINTERMEDIARY.csv", where = "mrremind")

  regmap <- toolGetMapping(type = "regional", name = "regionmapping_GAINS2025.csv", where = "mrremind")

  # =============================================================================
  # PART 1.A: SECTORAL AGGREGATION OF CEDS2025 EMISSIONS ========================
  # =============================================================================

  # CEDS sectors
  emiCEDS.sectCEDS <- fullceds

  # CEDS sectors to totals
  emiCEDS.sectTOT <- dimSums(emiCEDS.sectCEDS, dim = 3.1, na.rm = TRUE)

  # CEDS to INTERMEDIARY sectors
  # No weights required (many-to-one)
  emiCEDS.sectINT <- toolAggregate(
    emiCEDS.sectCEDS, cedsmap,
    from = "CEDS62", to = "INTERMEDIARY_CEDS_GAINS",
    weight = NULL, dim = "sector", wdim = NULL
  )

  # INTERMEDIARY to CMIP7 HARMONIZATION sectors
  # No weights required (many-to-one)
  emiCEDS.sectCMIP7 <- toolAggregate(
    emiCEDS.sectINT, cedsmap,
    from = "INTERMEDIARY_CEDS_GAINS", to = "CMIP7_HARMONIZATION",
    weight = NULL, dim = "sector", wdim = NULL
  )

  # =============================================================================
  # PART 1.B: SECTORAL AGGREGATION OF GAINS2025 EMISSIONS =======================
  # =============================================================================

  # GAINS sectors
  # Still at GAINS regions level
  emiGAINS.sectGAINS.regGAINS <- fullgains

  # GAINS sectors to totals
  # Still at GAINS regions level
  emiGAINS.sectTOT.regGAINS <- dimSums(emiGAINS.sectGAINS.regGAINS, dim = 3.1, na.rm = TRUE)

  # GAINS to INTERMEDIARY sectors
  # Still at GAINS regions level
  # No weights required (many-to-one)
  emiGAINS.sectINT.regGAINS <- toolAggregate(
    emiGAINS.sectGAINS.regGAINS, gainsmap,
    from = "GAINS2025", to = "INTERMEDIARY_CEDS_GAINS",
    weight = NULL, dim = "sector", wdim = NULL
  )

  # =============================================================================
  # PART 2.A: REGIONAL DISAGGREGATION OF GAINS2025 EMISSIONS ====================
  #         AT INTERMEDIARY SECTOR LEVEL ========================================
  #         Weights: Use CEDS emissions in intermediary sector. If zero
  #                  for the entire GAINS regions, use CEDS total emissions
  #                  instead.
  # =============================================================================

  # Default weights: CEDS emissions in intermediary sectors
  weights <- emiCEDS.sectINT

  # Replacement weights: Total CEDS emissions
  weights.sectTOT <- toolAddDimensions(emiCEDS.sectTOT,
    dimVals = getNames(emiCEDS.sectINT, dim = "sector"),
    dimName = "sector",
    dimCode = 3.1
  )
  weights.sectTOT <- weights.sectTOT[, , getItems(weights, dim = 3)]

  # Crease mask at ISO country and INTERMEDIARY sector level to check
  # if an ISO country belongs to a GAINS regions with zero CEDS emissions
  emiCEDS.sectINT.regGAINS <- toolAggregate(emiCEDS.sectINT, regmap,
    from = "CountryCode", to = "gainscode",
    weight = NULL, dim = "region", wdim = NULL
  )
  mask <- emiCEDS.sectINT.regGAINS == 0
  mask <- toolAggregate(mask, regmap,
    from = "gainscode", to = "CountryCode",
    weight = NULL, dim = "region"
  )
  mask <- magpiesort(mask)

  # Use replacement weights wherever mask is equal to 1
  weights[mask == 1] <- weights.sectTOT[mask == 1]
  # Keep only sectors for which weights are needed
  weights <- weights[, , getItems(emiGAINS.sectINT.regGAINS, dim = 3)]

  # Regional disaggregation
  emiGAINS.sectINT <- toolAggregate(emiGAINS.sectINT.regGAINS, regmap,
    from = "gainscode", to = "CountryCode",
    weight = weights, dim = "region", wdim = 1
  )

  # =============================================================================
  # PART 2.B: SECTORAL (DIS-)AGGREGATION OF GAINS2025 EMISSIONS =================
  #         AT ISO COUNTRY LEVEL ================================================
  # =============================================================================

  # INTERMEDIARY sectors to totals
  # No weights required (many-to-one)
  emiGAINS.sectTOT <- dimSums(emiGAINS.sectINT, dim = 3.1, na.rm = TRUE)

  # INTERMEDIARY CMIP7 HARMONIZATION sectors
  # No weights required (many-to-one)
  emiGAINS.sectCMIP7 <- toolAggregate(
    emiGAINS.sectINT, gainsmap,
    from = "INTERMEDIARY_CEDS_GAINS", to = "CMIP7_HARMONIZATION",
    weight = NULL, dim = "sector", wdim = NULL
  )

  # INTERMEDIARY sectors to GAINS sectors
  # Use GAINS emissions in GAINS regions as weights
  weights <- toolAggregate(
    emiGAINS.sectGAINS.regGAINS, regmap,
    from = "gainscode", to = "CountryCode",
    weight = NULL, dim = "region", wdim = NULL
  )

  # Weights are zero only if there are no emissions in a GAINS region "X" for
  # some sector.species combination "Y.Z".
  # Note that this implies that for the sector x species combination "Y.Z",
  # there are no emissions in all ISO countries belonging to GAINS region "X".
  # Thus, zero weights are only used when emissions are zero, and we can
  # safely allow it.
  emiGAINS.sectGAINS <- toolAggregate(
    emiGAINS.sectINT, gainsmap,
    from = "INTERMEDIARY_CEDS_GAINS", to = "GAINS2025",
    weight = weights, dim = "sector", wdim = 3.1,
    zeroWeight = "allow"
  )

  # =============================================================================
  # PART 3.A: SECTORAL DISAGGREGATION OF CEDS2025 EMISSIONS =====================
  #         TO GAINS2025 SECTORS ================================================
  #         Weights: Use GAINS emissions in ISO country. (Note that by 2.B,
  #                  the shares of GAINS sectors per INT sector are taken from
  #                  the GAINS region level.)
  #                  If zero for the entire intermediary sector,
  #                  use global GAINS emissions instead.
  #                  If global GAINS emissions also zero for the entire
  #                  intermediary sector, distribute equally.
  # =============================================================================

  # Select CEDS emissions in INT sectors with counterpart in GAINS,
  # i.e. removing NO GAINS Aircraft, NO GAINS Agriculture,
  # NO GAINS International Shipping, and NO GAINS zero
  emiCEDS.sectINTonlyGAINS <- mselect(emiCEDS.sectINT, sector = unique(gainsmap$INTERMEDIARY_CEDS_GAINS))

  # Default weights: GAINS emissions in GAINS sectors at ISO country level
  weights <- emiGAINS.sectGAINS

  # Replacement weights: GAINS emissions in GAINS sectors at global level
  emiGAINS.sectGAINS.global <- dimSums(emiGAINS.sectGAINS, dim = 1, na.rm = TRUE)
  weights.global <- toolAddDimensions(emiGAINS.sectGAINS.global,
    dimVals = getNames(emiGAINS.sectGAINS, dim = "region"),
    dimName = "region",
    dimCode = 1
  )
  weights.global <- weights.global[getItems(weights, dim = 1), , getItems(weights, dim = 3)]

  # Crease mask1 at ISO country and GAINS sector level to check
  # if a GAINS sector belongs to an INTERMEDIARY sector with
  # zero GAINS emissions in the respective ISO country.

  mask1 <- emiGAINS.sectINT == 0
  mask1 <- toolAggregate(mask1, gainsmap,
    from = "INTERMEDIARY_CEDS_GAINS", to = "GAINS2025",
    weight = NULL, dim = "sector"
  )
  mas1k <- mask1[getItems(weights, dim = 1), , getItems(weights, dim = 3)]

  # Crease mask2 at ISO country and GAINS sector level to check
  # if a GAINS sector belongs to an INTERMEDIARY sector with
  # zero GAINS emissions globally.

  emiGAINS.sectINT.global <- dimSums(emiGAINS.sectINT, dim = 1, na.rm = TRUE)
  mask2 <- emiGAINS.sectINT.global == 0
  mask2 <- toolAddDimensions(mask2,
    dimVals = getNames(emiGAINS.sectGAINS, dim = "region"),
    dimName = "region",
    dimCode = 1
  )
  mask2 <- toolAggregate(mask2, gainsmap,
    from = "INTERMEDIARY_CEDS_GAINS", to = "GAINS2025",
    weight = NULL, dim = "sector"
  )
  mask2 <- mask2[getItems(weights, dim = 1), , getItems(weights, dim = 3)]

  # Use replacement weights wherever mask1 is equal to 1
  weights[mask1 == 1] <- weights.global[mask1 == 1]
  # Use equal weights wherever mask2 is equal to 1
  weights[mask2 == 1] <- 1

  # Sectoral disaggregation
  emiCEDS.sectGAINS <- toolAggregate(
    emiCEDS.sectINTonlyGAINS, gainsmap,
    from = "INTERMEDIARY_CEDS_GAINS", to = "GAINS2025",
    weight = weights, dim = "sector", wdim = 3.1
  )

  # =============================================================================
  # PART 3.B: SECTORAL DISAGGREGATION OF GAINS2025 EMISSIONS ====================
  #         TO CEDS62 SECTORS ===================================================
  # =============================================================================

  # CURRENTLY NOT NEEDED
  # BUT COULD BE ADDED ANALOGOUSLY TO 3.A

  # =============================================================================
  # RETURN ======================================================================
  # =============================================================================

  if (source == "CEDS2025") {
    if (outsectors == "TOT") {
      out <- emiCEDS.sectTOT
      desc <- paste0("Total ", source, " emissions in year ", baseyear, ".")
    } else if (outsectors == "CEDS") {
      out <- emiCEDS.sectCEDS
      desc <- paste0(source, " emissions in year ", baseyear, " at level of 62 CEDS sectors.")
    } else if (outsectors == "INT") {
      out <- emiCEDS.sectINT
      desc <- paste0(source, " emissions in year ", baseyear, " at level of 15 intermediary sectors used to link CEDS and GAINS.")
    } else if (outsectors == "GAINS") {
      out <- emiCEDS.sectGAINS
      desc <- paste0(source, " emissions in year ", baseyear, " at level of 35 GAINS sectors.")
    } else if (outsectors == "CMIP7") {
      out <- emiCEDS.sectCMIP7
      desc <- paste0(source, " emissions in year ", baseyear, " at level of 13 CMIP7 Harmonization sectors.")
    } else {
      stop(paste0("Unknown sectoral aggregation: ", outsectors))
    }
  } else if (source == "GAINS2025") {
    if (outsectors == "TOT") {
      out <- emiGAINS.sectTOT
      desc <- paste0("Total ", source, " emissions in year ", baseyear, ".")
    } else if (outsectors == "CEDS") {
      stop("GAINS to CEDS mapping currently not available. See 3.B in the code.")
    } else if (outsectors == "INT") {
      out <- emiGAINS.sectINT
      desc <- paste0(source, " emissions in year ", baseyear, " at level of 16 intermediary sectors used to link CEDS and GAINS.")
    } else if (outsectors == "GAINS") {
      out <- emiGAINS.sectGAINS
      desc <- paste0(source, " emissions in year ", baseyear, " at level of 35 GAINS sectors.")
    } else if (outsectors == "CMIP7") {
      out <- emiGAINS.sectCMIP7
      desc <- paste0(source, " emissions in year ", baseyear, " at level of 13 CMIP7 Harmonization sectors.")
    } else {
      stop(paste0("Unknown sectoral aggregation: ", outsectors))
    }
  } else {
    stop(paste0("Unknown source: ", source))
  }

  return(list(
    x = out,
    weight = NULL,
    unit = "Mt/yr",
    description = desc
  ))
}
