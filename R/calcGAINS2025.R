#' Calculate air pollutant emission factors for all scenarios and SSPs
#' available from GAINS, at the level of GAINS sectors and for 2005-2100.
#'
#' This function is meant to be used to clean-up, fill gaps and smoothen
#' the GAINS data to obtain consistent timeseries of emission factors
#' from 2005 to 2100.
#' The actual generation REMIND-specific files happens in calcGAINS2025forREMIND.
#'
#' @return Emission factor timeseries for all scenarios from 2005 to 2100:
#'         magclass object with dimensions region, year, and
#'         ssp.scenario.sectorGAINS.species
#' @param weight_source Source of air pollutant reference emissions in 2020 that
#'         is used to derive the weights ("CEDS2025" or "GAINS2025")
#' @param outsectors Output sectoral aggregation ("GAINS2025" or "REMIND")
#' @param outunit Output unit for emission factors ("Mt/PJ" or "Tg/TWa")
#' @author Gabriel Abrahao, Laurin Koehler-Schindler
#'
calcGAINS2025 <- function(weight_source = "CEDS2025", outsectors = "GAINS2025", outunit = "Tg/TWa") {
  # ==============================================================================
  # Mappings and definitions =====================================================
  # ==============================================================================

  # conversion factors
  conv_Mt_per_PJ_to_Tg_per_TWa <- (1e12 * (365 * 24 * 60 * 60)) / 1e15

  # GAINS timesteps
  # historical
  htime <- seq(2005, 2025, 5)
  # baseline
  btime <- seq(2005, 2030, 5)
  # future
  ftime <- seq(2030, 2100, 5)

  # REMIND timesteps
  rtime <- c(seq(2005, 2055, 5), seq(2060, 2110, 10), 2130, 2150)

  # GAINS region mapping:
  # - Main calculations here happen at the GAINS region level as
  #   readGAINS2025 does not disaggregate its outputs.
  # - Disaggregation to country level happens at the end.
  regmap <- toolGetMapping(type = "regional", name = "regionmapping_GAINS2025.csv", where = "mrremind")

  # ==============================================================================
  # MAIN CALCULATION:
  # 1. Derive scenario-specific emission factors for 2030-2100.
  #    This includes removing incomplete timeseries, filling NAs,
  #    amd removing large spikes.
  # 2. Calculate baseline emission factors for 2005-2030 based on
  #    emissions and activities in the baseline scenario.
  #    This includes filling NAs, removing large spikes, and
  #    ensuring that emission factors in 2005-2025 are not below 2030 level.
  # 3. Combine 1. and 2. by rescaling baseline emission factors (1.) so that
  #    in 2030, it is equal to the scenario-specific emission factor (2.) of
  #    the SSP2-SMIPbySSP scenario. This preserves relative changes between
  #    timesteps and allows to smoothly concatenate the emission factors
  #    before and after 2030
  # ==============================================================================

  # ==============================================================================
  # 1. DERIVE SCENARIO-SPECIFIC EMISSION FACTORS FOR 2030-2100 ===================
  # ==============================================================================

  # Read in emission factors for all scenarios (except baseline, for which no
  # emission factors, but only emissions and activities, are available)
  inefs <- readSource("GAINS2025final", subtype = "emifacs")

  # Keep only future scenarios for 2030-2100
  futureefs <- inefs[, ftime, ]
  futureefs <- mselect(futureefs,
    ssp = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5", "MTFR", "SMIPVLLO"),
    scenario = c("SMIPbySSP", "SMIPVLLO", "MTFR", "SLE", "CLE")
  )

  # 1.1. HANDLE MISSING DATA AT START (2030-) ===================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: If emission factor is missing (NA) for 2030,
  # remove emission factors for all timesteps (i.e. set to NA)
  # Example 1:
  # BEFORE: x(2030)= NA, x(2035) = 5, x(2040) =5, ...
  # AFTER:  x(2030)= NA, x(2035) = NA, x(2040) = NA, ...

  mask <- setYears(futureefs[, 2030, ], NULL)
  mask[!is.na(mask)] <- 1
  futureefs <- futureefs * mask

  # 1.2. HANDLE MISSING DATA AT END (-2100) & ===================================
  # 1.3. LINEAR INTERPOLATION ===================================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: 1.2. If data is missing at the end, fill it with last available data
  #            point.
  #            1.3. If there are NAs in intermediate timesteps, use linear inter-
  #            polation to fill these.
  #            The linear interpolation should be done whenever data is available
  #            for an earlier and for a later timestep.
  #            Note that thanks to 1.1 and 1.2, there are no NAs at the start
  #            or at the end of the timeseries.
  # Example 1:
  # BEFORE: ... ,x(2090)= 10, x(2095) = NA, x(2100) = NA
  # AFTER:  ... ,x(2090)= 10, x(2095) = 10, x(2100) = 10
  # Example 2:
  # BEFORE: x(2030)= 2, x(2035) = 5, x(2040) = NA, x(2045) = NA, x(2050) = 20
  # AFTER:  x(2030)= 2, x(2035) = 5, x(2040) = 10, x(2045) = 15, x(2050) = 20

  for (r in getItems(futureefs, dim = 1)) {
    for (n in getNames(futureefs)) {
      tmp <- futureefs[r, , n]
      if (any(is.na(tmp)) && !all(is.na(tmp))) {
        futureefs[r, , n] <- toolFillYears(tmp[, which(!is.na(tmp)), ], years = getYears(tmp))
      }
    }
  }

  # 1.4. LIMIT RELATIVE INCREASE ========================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: Limit the relative increase between two consecutive timesteps to
  #            a factor of 2.
  #            Starting from t = 2030, the change from x(t) to x(t+5) should be
  #            checked. If x(t+5) > x(t) * 2, set x(t+5) = x(t) * 2.
  #            Then continue with the next timestep, i.e. check the change from
  #            the (possibly) updated value x(t+5) to x(t+10).
  # Example:
  # BEFORE: x(2030)= 1, x(2035) = 4, x(2040) = 3
  # AFTER:  x(2030)= 1, x(2035) = 2, x(2040) = 3

  # Function to limit the maximum relative increase to the next time step to 2
  smoothenTs_forward <- function(ts) {
    for (i in 1:(length(ts) - 1)) {
      if (ts[i + 1] > 2 * ts[i]) {
        ts[i + 1] <- 2 * ts[i]
      }
    }
    return(ts)
  }

  for (r in getItems(futureefs, dim = 1)) {
    for (n in getNames(futureefs)) {
      ts <- as.numeric(futureefs[r, , n])
      if (!all(is.na(ts))) {
        m <- max(tail(ts, -1) / head(ts, -1), na.rm = TRUE)
        if (m > 2) {
          futureefs[r, , n] <- smoothenTs_forward(ts)
        }
      }
    }
  }

  # 1.5. HANDLE NAs ==============================================================
  # ==== FUTURE EMISSION FACTORS FOR 2030 - 2100 =================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Observation: Following 1.1-1.4, each timeseries consists either
  #             (a) only of NAs, or (b) only of numeric entries. Moreover, if
  #             the 2030 emission factor is 0, it remains zero in following
  #             timesteps due to 1.4.
  # Procedure: Replace (a) timeseries of NAs by 0s.

  futureefs[is.na(futureefs)] <- 0.0

  # ==============================================================================
  # 2. CALCULATE BASELINE EMISSION FACTORS FOR 2005-2030 =========================
  # ==============================================================================

  # Read baseline emissions and activities =======================================
  baseemis <- readSource("GAINS2025final", subtype = "emissions")
  baseacts <- readSource("GAINS2025final", subtype = "activities")
  # Keep only timesteps between 2005 and 2030
  baseemis <- baseemis[, btime, ]
  baseacts <- baseacts[, btime, ]

  # 2.1. COMPUTE BASELINE EMISSION FACTORS =======================================
  baseefs <- baseemis / baseacts

  # 2.2 HANDLE MISSING DATA AT END (-2030) =======================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: If emission factor in 2030 is missing (NA),
  #            fill with emission factor from 2025.
  #            If emission factor in 2025 is also missing (NA),
  #            fill 2025 and 2030 with emission factor from 2020.
  #            If emission factor in 2020 is also missing (NA),
  #            remove emission factors for all timesteps (i.e. set to NA)
  # Example 1:
  # BEFORE: ... ,x(2015)= 3, x(2020) = 2, x(2025) = 3, x(2030) = NA
  # AFTER:  ... ,x(2015)= 3, x(2020) = 2, x(2025) = 3, x(2030) = 3
  # Example 2:
  # BEFORE: ... ,x(2015)= 3, x(2020) = NA, x(2025) = NA, x(2030) = NA
  # AFTER:  x(2005) = NA, ... ,x(2030) = NA

  # If emission factor in 2030 is missing (NA), fill with emission factor from 2025.
  baseefs[, 2030, ][is.na(baseefs[, 2030, ])] <- baseefs[, 2025, ][is.na(baseefs[, 2030, ])]

  # If emission factor in 2025 is also missing (NA), i.e. 2030 still NA,
  # fill 2025 and 2030 with emission factor from 2020.
  baseefs[, 2025, ][is.na(baseefs[, 2025, ])] <- baseefs[, 2020, ][is.na(baseefs[, 2025, ])]
  baseefs[, 2030, ][is.na(baseefs[, 2030, ])] <- baseefs[, 2020, ][is.na(baseefs[, 2030, ])]

  #  If emission factor in 2020 is also missing (NA), i.e. 2030 still NA,
  #  remove emission factors for all timesteps (i.e. set to NA)
  mask <- setYears(baseefs[, 2030, ], NULL)
  mask[!is.na(mask)] <- 1
  baseefs <- baseefs * mask

  # 2.3. HANDLE MISSING DATA AT START (2005-) =====================================
  # 2.4. LINEAR INTERPOLATION ===================================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: 2.3. If data is missing at the start, fill it with first available data
  #            point.
  #            2.4. If there are NAs in intermediate timesteps, use linear inter-
  #            polation to fill these.
  #            Note that thanks to 2.2, there are no NAs at the end of the timeseries.
  # Example 1:
  # BEFORE: x(2005) = NA, x(2010) = NA, x(2015) = 3, x(2020) = 2, ...
  # AFTER:  x(2005) = 3,  x(2010) = 3,  x(2015) = 3, x(2020) = 2, ...
  # Example 2:
  # BEFORE: x(2005)= 5, x(2010) = 5, x(2015) = NA, x(2020) = NA, x(2025) = 20, x(2030) = 15
  # BEFORE: x(2005)= 5, x(2010) = 5, x(2015) = 10, x(2020) = 15, x(2025) = 20, x(2030) = 15

  for (r in getItems(baseefs, dim = 1)) {
    for (n in getNames(baseefs)) {
      tmp <- baseefs[r, , n]
      if (any(is.na(tmp)) && !all(is.na(tmp))) {
        baseefs[r, , n] <- toolFillYears(tmp[, which(!is.na(tmp)), ], years = getYears(tmp))
      }
    }
  }

  # 2.5. ENSURE THAT EMISSION FACTOR BEFORE 2030 IS NEVER BELOW 2030 VALUE ======
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: If for some t < 2030, the emission factor ef(t) is smaller than
  #            the emission factor ef(2030), set ef(t) := ef(2030).
  # Example:
  # BEFORE: x(2005)= 0.9,   x(2010) = 1.1, x(2015) = 0.75, x(2020) = 1.5, x(2025) = 2,  x(2030) = 1
  # AFTER:  x(2005)= 1,     x(2010) = 1.1, x(2015) = 1,    x(2020) = 1.5, x(2025) = 2,  x(2030) = 1

  baseefs_2030_expanded <- baseefs[, 2030, ]
  baseefs_2030_expanded <- baseefs_2030_expanded[, rep(1, length(getYears(baseefs))), ]
  getYears(baseefs_2030_expanded) <- getYears(baseefs)

  mask <- baseefs < baseefs_2030_expanded
  mask[is.na(mask)] <- FALSE
  baseefs[mask] <- baseefs_2030_expanded[mask]

  # 2.6. LIMIT RELATIVE CHANGE ==================================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: Limit the relative increase between two consecutive timesteps
  #            to a factor of 2 and the relative decrease to 10.
  #            Starting from t = 2030, the change from x(t-5) to x(t) should be
  #            checked. If x(t-5) < x(t)/ 2, set x(t-5) = x(t) / 2.
  #            If x(t-5) > x(t) * 10, set x(t-5) = x(t) * 10.
  #            Then continue with the next timestep, i.e. check the change from
  #            the (possibly) updated value x(t-5) to x(t-10).
  # Note that in comparison to 1.4, this procedure is moving backwards in time.
  # Example:
  # BEFORE: x(2005)= 40,   x(2010) = 100, x(2015) = 50, x(2020) = 150, x(2025) = 12,  x(2030) = 10
  # AFTER:  x(2005)= 50,   x(2010) = 100, x(2015) = 60, x(2020) = 120, x(2025) = 12,  x(2030) = 10

  # Function to limit the maximum relative increase to the next time step to 2
  # and the maximum relative decrease to the next timestep to 10
  smoothenTs_backward <- function(ts) {
    for (i in (length(ts)):2) {
      if (ts[i - 1] < ts[i] / 2) {
        ts[i - 1] <- ts[i] / 2
      }
      if (ts[i - 1] > ts[i] * 10) {
        ts[i - 1] <- ts[i] * 10
      }
    }
    return(ts)
  }

  for (r in getItems(baseefs, dim = 1)) {
    for (n in getNames(baseefs)) {
      ts <- as.numeric(baseefs[r, , n])
      if (!all(is.na(ts))) {
        max <- max(head(ts, -1) / tail(ts, -1))
        min <- min(head(ts, -1) / tail(ts, -1))
        if ((!is.na(min) && min < 1 / 2) || (!is.na(max) && max > 10)) {
          baseefs[r, , n] <- smoothenTs_backward(ts)
        }
      }
    }
  }

  # 2.7. HANDLE NAs and ZEROs, AND RESCALE =======================================
  #      BASELINE EMISSION FACTORS FOR 2005 - 2030 ===============================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Observation: Following 2.1-2.6, each timeseries consists either
  #             (a) only of NAs, (b) only of 0s, or
  #             (c) only of non-zero entries.
  # Procedure: 1. Replace (a) timeseries of NAs and (b) timeseries of 0s
  #               by a constant timeseries of 1s. This implements the assumption
  #               that timeseries of NAs or 0s should be treated as constant for
  #               the timesteps 2005 - 2030 to enable concatenation with non-zero
  #               future emission factors. If future emission factors are zero,
  #               the multiplication in 3.1 will set the timeseries back to 0s.
  #            2. Rescale (c) non-zero timeseries so that 2030 value is equal
  #               to 1. This does not affect relative changes and facilitates
  #               concatenation with future emission factors.

  baseefs[is.na(baseefs)] <- 1
  baseefs[baseefs == 0] <- 1

  baseefs_2030_expanded <- baseefs[, 2030, ]
  baseefs_2030_expanded <- baseefs_2030_expanded[, rep(1, length(getYears(baseefs))), ]
  getYears(baseefs_2030_expanded) <- getYears(baseefs)

  baseefs_normalized <- baseefs / baseefs_2030_expanded

  # ==============================================================================
  # 3. COMBINE EMISSION FACTORS FROM 1. (2005-2030) AND FROM 2. (2030-2100)
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Note that at this stage,
  # a. baseline emission factors are all non-zero,
  # b. baseline emission factors (baseefs_normalized) are equal to 1 in 2030.
  # ==============================================================================

  # 3.1 SELECT SSP2.SMIPbySSP AS REFERENCE EMISSION FACTOR IN 2030 AND ===========
  #     RESCALE BASELINE EMISSION FACTORS ACCORDINGLY ============================

  refefs_2030_expanded <- mselect(futureefs, ssp = "SSP2", scenario = "SMIPbySSP")
  refefs_2030_expanded <- collapseDim(refefs_2030_expanded, dim = c("ssp", "scenario"))
  refefs_2030_expanded <- refefs_2030_expanded[, 2030, ]
  refefs_2030_expanded <- refefs_2030_expanded[, rep(1, length(getYears(baseefs_normalized))), ]
  getYears(refefs_2030_expanded) <- getYears(baseefs_normalized)

  baseefs_normalized <- collapseDim(baseefs_normalized, dim = c("ssp", "scenario"))

  baseefs_rescaled <- refefs_2030_expanded * baseefs_normalized

  # 3.2. COMBINE RESCALED BASEEFS AND FUTUREEFS ==================================
  # Remove 2030 timestep from baseefs_rescaled.
  # Expand baseefs_rescaled to contain all ssp x scenario combinations
  # of futureefs.
  # Combine rescaled baseline emission factors with future emission factors

  baseefs_rescaled <- baseefs_rescaled[, htime, ]

  baseefs_rescaled_expanded <- toolAddDimensions(
    toolAddDimensions(
      baseefs_rescaled, getItems(futureefs, "scenario"), "scenario", 3.1
    ), getItems(futureefs, "ssp"), "ssp", 3.1
  )

  baseefs_rescaled_expanded <- baseefs_rescaled_expanded[, , getNames(futureefs)]

  efs <- mbind(baseefs_rescaled_expanded, futureefs)

  # ==============================================================================
  # 4. MANUAL CHANGES TO EMISSION FACTORS ========================================
  # ==============================================================================

  # 4.1. Waste_Water_Municipal, NH3 ==============================================
  # GAINS2025 data only contains emission factors for WEST_EURO, CENT_EURO, and
  # RUSS_PLUS. In all three cases, the baseline emission factor is constant in time
  # and equal to 1. For CENT_EURO and WEST_EURO, scenario specific emission factors
  # remain close to 1. For RUSS_PLUS, it decreases substantially in some scenarios.
  # Reason for manual change: CEDS2025 contains relevant NH3 emissions
  # from Wastewater (5D_Wastewater-handling) also outside these regions.
  # Assumption: Set emission factor constant to 1 for all other regions.
  # Note that NH3 has zero cost in remind/modules/11_aerosols/exoGAINS/datainput.gms

  tmp_species <- "NH3"
  tmp_sector <- "Waste_Water_Municipal"
  tmp_names <- getNames(mselect(efs, sectorGAINS = tmp_sector, species = tmp_species))
  tmp_regions <- setdiff(getItems(efs, dim = 1), c("CENT_EURO", "RUSS_PLUS", "WEST_EURO"))

  for (r in tmp_regions) {
    for (n in tmp_names) {
      efs[r, , n] <- 1
    }
  }

  # 4.2. PAPER, VOC ==============================================================
  # GAINS2025 data contains no emission factors.
  # Reason for manual change:  CEDS2025 contains relevant VOC emissions from Paper
  # (2H_Pulp-and-paper-food-beverage-wood).
  # Assumption: Set emission factor constant to 1 for all regions.
  # Note that VOC has zero cost in remind/modules/11_aerosols/exoGAINS/datainput.gms

  tmp_species <- "VOC"
  tmp_sector <- "PAPER"
  tmp_names <- getNames(mselect(efs, sectorGAINS = tmp_sector, species = tmp_species))
  tmp_regions <- getItems(efs, dim = 1)

  for (r in tmp_regions) {
    for (n in tmp_names) {
      efs[r, , n] <- 1
    }
  }

  # ==============================================================================
  # PREPARE SCENARIOS FOR HANDOVER TO REMIND:
  # A. Constant extrapolation to REMIND timesteps beyond 2100
  # B. Disaggregation of emission factors to ISO level
  # C. Add weights: Read reference emissions for baseyear, compute activities
  #    as weights and add these weights
  # ==============================================================================

  # A. REMIND timesteps ==========================================================
  efs <- time_interpolate(efs, rtime, extrapolation_type = "constant")

  # B. Disaggregation of emission factors to ISO level ===========================
  isoefs <- toolAggregate(
    efs, regmap,
    from = "gainscode", to = "CountryCode",
    weight = NULL, dim = 1, wdim = NULL
  )

  # C. Add weights ===============================================================

  # Reference emissions in 2020
  isoemis2020 <- calcOutput("AirPollBaseyearEmi",
    baseyear = 2020, CEDS.5yearmean = TRUE,
    source = weight_source, outsectors = "GAINS",
    aggregate = FALSE
  )
  getSets(isoemis2020)[getSets(isoemis2020) == "sector"] <- "sectorGAINS"
  isoemis2020 <- magpiesort(isoemis2020)

  # Emissions factors in 2020
  # Since 2020 emission factor comes from baseefs_rescaled_expanded for
  # all scenarios, pick anyone of them
  isoefs2020 <- isoefs[, 2020, ]
  isoefs2020 <- mselect(isoefs2020, ssp = "SSP2", scenario = "SMIPbySSP")
  isoefs2020 <- collapseDim(isoefs2020, dim = c("ssp", "scenario"))
  isoefs2020 <- magpiesort(isoefs2020)

  # Activities in 2020
  isoacts2020 <- isoemis2020 / isoefs2020

  # If emission factor and emissions are both zero, division yields NaN.
  # Replace with zero.
  isoacts2020[is.na(isoacts2020)] <- 0

  # If emission factor is zero but emissions are positive, division yields Inf.
  # Replace with zero. As isoacts2020 is only used as weight for emission
  # factors, the choice of the exact value is actually irrelevant.
  isoacts2020[isoacts2020 == Inf] <- 0

  # Expand year, ssp, and scenario dimension to use 2020 activities as weights
  weights <- time_interpolate(isoacts2020, rtime, extrapolation_type = "constant")

  weights <- toolAddDimensions(
    toolAddDimensions(
      weights, getItems(isoefs, "scenario"), "scenario", 3.1
    ), getItems(isoefs, "ssp"), "ssp", 3.1
  )
  weights <- weights[, , getNames(isoefs)]

  # D. Choose unit for output ====================================================

  if (outunit == "Tg/TWa") {
    # Convert unit
    isoefs <- isoefs * conv_Mt_per_PJ_to_Tg_per_TWa
  } else if (outunit == "Mt/PJ") {
    # Already in correct unit
  } else {
    stop(paste0("Unknown unit: ", outunit))
  }

  # D. Choose sectors for output =================================================
  #    and make necessary changes for handover to REMIND =========================

  if (outsectors == "GAINS2025") {
    # No changes needed as out and weights are already at the desired sectoral
    # resolution
    out <- isoefs
    wgt <- weights
  } else if (outsectors == "REMIND") {
    # SO2 unit conversion =======================================================
    # Apparently REMIND expects TgS internally, but not in exoGAINS
    conv_MtSO2_to_MtS <- 1 / 2 # 32/(32+2*16)
    isoefs[, , "SO2"] <- isoefs[, , "SO2"] * conv_MtSO2_to_MtS

    # Rename and change order subdimensions =====================================
    fixDims <- function(mag) {
      getSets(mag) <- c("region", "year", "ssp", "scenario", "sector", "emi")
      mag <- dimOrder(mag, c(3, 4, 2, 1), dim = 3) # c("sector", "emi", "scenario", "ssp")
      return(mag)
    }
    isoefs <- fixDims(isoefs)
    weights <- fixDims(weights)

    # (Dis-)aggregation from GAINS2025 to REMIND sectors ========================

    # Mapping from GAINS2025 sectors to REMIND GAMS subsectors
    secmap <- toolGetMapping(
      type = "sectoral",
      name = "mappingINTERMEDIARYtoGAINS2025toREMINDtoIAMC.csv",
      where = "mrremind"
    )

    # Drop sectors that are not mapped with sufficient detail. Those that can be
    # used have a dot "." splitting the specific technologies
    dropsectors <- secmap$GAINS2025[!grepl("\\.", secmap$REMIND_INTERNAL)]
    isoefs <- isoefs[, , dropsectors, invert = TRUE]
    weights <- weights[, , dropsectors, invert = TRUE]
    filtsecmap <- secmap[!(secmap$GAINS2025 %in% dropsectors), ]

    # (Dis-)aggregation depending on GAINS sector
    # Example 1: Power_Gen_NatGas is disaggregated to pegas.seel.gaschp.power,
    #            pegas.seel.ngcc.power, and pegas.seel.ngt.power
    # Example 2: End_Use_Industry_Bio_Trad and End_Use_Services_Bio_Trad are
    #            aggregated to pecoal.sesofos.coaltr.indst
    #
    # Since weights are provided at GAINS sector resolution, in Example 1,
    # all REMIND sectors simply receive exactly the same values as the GAINS sector.
    # In Example 2, the weights are used to form a weighted average.
    #
    # Zero weights are allowed for cases where acitivites are zero but the EFs
    # are non-zero. In such cases, the resulting EF is zero.
    isoefs <- toolAggregate(isoefs, filtsecmap, weight = weights, from = "GAINS2025", to = "REMIND_INTERNAL", dim = "sector", wdim = NULL, zeroWeight = "allow")
    weights <- toolAggregate(weights, filtsecmap, weight = NULL, from = "GAINS2025", to = "REMIND_INTERNAL", dim = "sector")

    # Split sector into several subdimensions 00000000000========================
    # Use abind to split the dimensions of specific technologies into subsectors
    splitTechs <- function(mag) {
      mag <- as.magpie(abind::abind(clean_magpie(mag)), spatial = 1, temporal = 2)
      getSets(mag) <- c("region", "year", "sector1", "sector2", "sector3", "sector4", "emi", "scenario", "ssp")
      return(mag)
    }
    isoefs <- splitTechs(isoefs)
    weights <- splitTechs(weights)

    # Drop sectors not used in REMIND anymore ===================================
    isoefs <- isoefs[, , c("pcc", "pco"), invert = TRUE]
    weights <- weights[, , c("pcc", "pco"), invert = TRUE]

    out <- isoefs
    wgt <- weights
  } else {
    stop(paste0("Unknown sector aggergation: ", outsectors))
  }


  return(list(
    x = out,
    weight = wgt,
    unit = outunit,
    description = "Emission factor timeseries for all scenarios and for all REMIND timesteps based on GAINS2025 data."
  ))
}
