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
#' @author Gabriel Abrahao, Laurin Koehler-Schindler
#' @param subtype "emifacs"
#'
calcGAINS2025scenarios <- function() {
  # ==============================================================================
  # Mappings and definitions =====================================================
  # ==============================================================================

  allssps <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")

  # REMIND timesteps
  rtime <- c(seq(2005, 2055, 5), seq(2060, 2110, 10), 2130, 2150)

  # GAINS timesteps
  # baseline
  btime <- seq(2005, 2030, 5)
  # future
  ftime <- seq(2030, 2100, 5)

  # GAINS region mapping
  # Most of the calculations here happen at the GAINS region level, as
  # readGAINS2025 does not disaggregate its outputs
  regmap <- toolGetMapping(type = "regional", name = "regionmapping_GAINS2025.csv", where = "mrremind")

  # ==============================================================================
  # CALCULATION METHOD:
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

  # 1. DERIVE SCENARIO-SPECIFIC EMISSION FACTORS FOR 2030-2100 ===================

  # Read in emission factors for all scenarios (except baseline, for which no
  # emission factors, but only emissions and activities, are available)
  inefs <- readSource("GAINS2025", subtype = "emifacs")

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
  browser()
  
  mask <- setYears(futureefs[, 2030, ], NULL)
  mask[!is.na(mask)] <- 1
  futureefs <- futureefs * mask

  # 1.2. HANDLE MISSING DATA AT END (-2100) =====================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: If data is missing at the end, fill it with last available data
  #            point.
  # Example:
  # BEFORE: ... ,x(2090)= 10, x(2095) = NA, x(2100) = NA
  # AFTER:  ... ,x(2090)= 10, x(2095) = 10, x(2100) = 10

  # TODO

  # 1.3. LINEAR INTERPOLATION ===================================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: If there are NAs in intermediate timesteps, use linear inter-
  #            polation to fill these.
  #            The linear interpolation should be done whenever data is available
  #            for an earlier and for a later timestep.
  #            Note that thanks to 1.1 and 1.2, there are no NAs at the start
  #            or at the end of the timeseries.
  # Example:
  # BEFORE: x(2030)= 2, x(2035) = 5, x(2040) = NA, x(2045) = NA, x(2050) = 20
  # AFTER:  x(2030)= 2, x(2035) = 5, x(2040) = 10, x(2045) = 15, x(2050) = 20

  # TODO

  # Laurins attempt
  # futureefs <- toolFillYears(futureefs, ftime)

  # 1.4. LIMIT RELATIVE INCREASE ========================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: Limit the relative increase between two consecutive timesteps to
  #            a factor of 10.
  #            Starting from t = 2030, the change from x(t) to x(t+5) should be
  #            checked. If x(t+5) > x(t) * 10, set x(t+5) = x(t) * 10.
  #            Then continue with the next timestep, i.e. check the change from
  #            the (possibly) updated value x(t+5) to x(t+10).
  # Example:
  # BEFORE: x(2030)= 1, x(2035) = 20, x(2040) = 15
  # AFTER:  x(2030)= 1, x(2035) = 10, x(2040) = 15

  # TODO

  # 2. CALCULATE BASELINE EMISSION FACTORS FOR 2005-2030 =========================

  # Read baseline emissions and activities =======================================
  baseemis <- readSource("GAINS2025", subtype = "emissions")
  baseacts <- readSource("GAINS2025", subtype = "activities")
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
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: If data is missing at the start, fill it with last available data
  #            point.
  # Example:
  # BEFORE: x(2005) = NA, x(2010) = NA, x(2015) = 3, x(2020) = 2, ...
  # AFTER:  x(2005) = 3,  x(2010) = 3,  x(2015) = 3, x(2020) = 2, ...

  # TODO

  # 2.4. LINEAR INTERPOLATION ===================================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: If there are NAs in intermediate timesteps, use linear inter-
  #            polation to fill these.
  #            The linear interpolation should be done whenever data is available
  #            for an earlier and for a later timestep.
  #            Note that thanks to 2.2 and 2.3, there are no NAs at the start
  #            or at the end of the timeseries.
  # Example:
  # BEFORE: x(2005)= 5, x(2010) = 5, x(2015) = NA, x(2020) = NA, x(2025) = 20, x(2030) = 15
  # BEFORE: x(2005)= 5, x(2010) = 5, x(2015) = 10, x(2020) = 15, x(2025) = 20, x(2030) = 15

  # TODO

  # Laurins attempt
  # futureefs <- toolFillYears(baseefs, btime)

  # 2.5. ENSURE THAT EMISSION FACTOR BEFORE 2030 IS NEVER BELOW 2030 VALUE ======
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: If for some t < 2030, the emission factor ef(t) is smaller than
  #            the emission factor ef(2030), set ef(t) := ef(2030).
  # Example:
  # BEFORE: x(2005)= 0.9,   x(2010) = 1.1, x(2015) = 0.75, x(2020) = 1.5, x(2025) = 2,  x(2030) = 1
  # AFTER:  x(2005)= 1,     x(2010) = 1.1, x(2015) = 1,    x(2020) = 1.5, x(2025) = 2,  x(2030) = 1

  # TODO

  # 2.6. LIMIT RELATIVE CHANGE ==================================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: Limit the relative increase or decrease between two consecutive
  #            timesteps to a factor of 10.
  #            Starting from t = 2030, the change from x(t-5) to x(t) should be
  #            checked. If x(t-5) < x(t)/ 10, set x(t-5) = x(t) / 10.
  #            If x(t-5) > x(t) * 10, set x(t-5) = x(t) * 10.
  #            Then continue with the next timestep, i.e. check the change from
  #            the (possibly) updated value x(t-5) to x(t-10).
  # Note that in comparison to 1.4, this procedure is moving backwards in time.
  # Example:
  # BEFORE: x(2005)= 20,   x(2010) = 150, x(2015) = 10, x(2020) = 150, x(2025) = 12,  x(2030) = 10
  # AFTER:  x(2005)= 20,   x(2010) = 120, x(2015) = 12, x(2020) = 120, x(2025) = 12,  x(2030) = 10

  # TODO




  # 3. COMBINE EMISSION FACTORS FROM 1. (2005-2030) AND FROM 2. (2030-2100) ======
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Note that at this stage, the following properties hold
  # for baseefs (2005-2030) and for futureefs (2030-2100):
  # a. Each timeseries is either complete or only consists of NAs.
  # b. If the emission factor is equal to zero in 2030, it is also zero
  #    in all other timesteps due to the limited relative change.
  # c. If the baseline emission factor in 2030 is non-zero, the baseline
  #    emission factors for 2005-2030 are all non-zero.

  # 3.1. RESCALING OF BASEEFS ====================================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: Multiplicative rescaling of baseline emission factors so
  #            that 2030 baseline emission factor is equal to the
  #            2030 SSP2 x SMIPbySSP emission factor.
  # Important note: This preserves relative changes.

  # If baseline emission factor is zero or NA in 2030, treat it as constant and
  # set 2005-2030 emission factors to 1.

  # TODO

  # Select 2030 emission factors for baseline
  baseline.2030 <- mselect(baseefs, ssp = "baseline", scenario = "baseline")
  baseline.2030 <- baseline.2030[, 2030, ]

  # Select 2030 emission factors for SSP2 x SMIPbySSP
  SMIPSSP2.2030 <- mselect(futureefs, ssp = "SSP2", scenario = "SMIPbySSP")
  SMIPSSP2.2030 <- SMIPSSP2.2030[, 2030, ]

  # Compute rescaling factor
  rescale.factor <- SMIPSSP2.2030 / baseline.2030

  # Rescale baseefs
  for (t in btime) {
    rescaled.baseefs[, t, ] <- baseefs[, t, ] * rescale.factor[, 2030, ]
  }

  # CHECK IF THERE ARE ANY SECTORS x SPECIES x REGIONS, for which no SSP2 x SMIPbySSP data is available,
  #      BUT OTHER SCENARIO DATA IS AVAILABLE
  # IF YES, FIND WORKAROUND
  
  # 3.2. COMBINE RESCALED BASEEFS AND FUTUREEFS ==================================
  # Expand rescaled.baseefs so that it can be bind 
  # to all ssp x scenario combinations.
  # Combine rescaled baseline emission factors with future emission factors
  
  efs <- mbind(rescaled.baseefs, futureefs)
  
  # 3.3. HANDLE REMAINING NAs ====================================================
  
  # TODO

  # ==============================================================================
  # PREPARE SCENARIOS FOR HANDOVER TO REMIND:
  # 1. Make sure that scenarios are available for all SSPs (check REMIND options)
  #    E.g. make MTFR available for all SSPs
  # 2. Country disaggregation
  # 3. Read emissions from GAINS or CEDS in 2020, compute activities as weights
  #    and add these weights
  # ==============================================================================


  return(list(
    x = out,
    weight = wgt,
    description = "Emission factor timeseries for all scenarios from 2005 to 2100 based on GAINS2025 data."
  ))
}
