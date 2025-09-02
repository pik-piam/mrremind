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
  # METHOD:
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
  # Procedure: If data is missing for 2030 (i.e. NA), remove data for all
  #            timesteps (i.e. set to NA)
  # Example:
  # BEFORE: x(2030)= NA, x(2035) = 5, x(2040) =5, ...
  # AFTER:  x(2030)= NA, x(2035) = NA, x(2040) = NA, ...

  TODO

  # 1.2. HANDLE MISSING DATA AT END (-2100) =====================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: If data is missing at the end, fill it with last available data
  #            point.
  # Example:
  # BEFORE: ... ,x(2090)= 10, x(2095) = NA, x(2100) = NA
  # AFTER:  ... ,x(2090)= 10, x(2095) = 10, x(2100) = 10

  TODO

  # 1.3. LINEAR INTERPOLATION ===================================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: If there are NAs in intermediate timesteps, use linear inter-
  #            polation to fill these.
  #            The linear interpolation should be done whenever data is available
  #            for an earlier and for a later timestep.
  #            Note that thanks to 2.1 and 2.2, there are no NAs at the start
  #            or at the end of the timeseries.
  # Example:
  # BEFORE: x(2030)= 2, x(2035) = 5, x(2040) = NA, x(2045) = NA, x(2050) = 20
  # AFTER:  x(2030)= 2, x(2035) = 5, x(2040) = 10, x(2045) = 15, x(2050) = 20

  TODO

  # 1.4. SMOOTHING ==============================================================
  # Granularity: region (dim = 1) x ssp.scenario.sectorGAINS.species (dim = 3)
  # Procedure: Limit the change between two consecutive timesteps to
  #            a factor of 2.5 (a 250 percent increase in 5 years corresponds to
  #            a 20% increase per year).
  #            This shall limit increases as well as decreases.
  #            Starting from t = 2030, the change from x(t) to x(t+5) should be
  #            checked. If x(t+5) > x(t) * 2.5, set x(t+5) = x(t) * 2.5.
  #            If x(t+5) < x(t) / 2.5, set x(t+5) = x(t) / 2.5.
  #            Then continue with the next timestep, i.e. check the change from
  #            the (possibly) updated value x(t+5) to x(t+10).
  # Example 1:
  # BEFORE: x(2030)= 1, x(2035) = 3, x(2040) = 5, x(2045) = 4
  # AFTER:  x(2030)= 1, x(2035) = 2.5, x(2040) = 5, x(2045) = 4
  # Example 2:
  # BEFORE: x(2030)= 1, x(2035) = 0.4, x(2040) = 2, x(2045) = 2
  # AFTER:  x(2030)= 1, x(2035) = 0.4, x(2040) = 1, x(2045) = 2

  # TODO

  # -------------------------

  IGNORE # FROM HERE (WILL BE DONE SIMILARLY)

  # 2. CALCULATE BASELINE EMISSION FACTORS FOR 2005-2030 =========================

  # Read baseline emissions and activities =======================================
  baseemis <- readSource("GAINS2025", subtype = "emissions")
  baseacts <- readSource("GAINS2025", subtype = "activities")
  # Keep only timesteps between 2005 and 2030
  baseemis <- histemis[, btime, ]
  baseacts <- histacts[, btime, ]

  # 2.1. COMPUTE BASELINE EMISSION FACTORS =======================================
  baseefs <- baseemis / baseacts

  #  2.2. IF 2030 is missing, take 2025. If also missing, set all timesteps to NA.
  #  2.3. Interpolate: Intermediate steps linearly, endpoints constant
  #       CHECK: All should now be fully NA or complete
  #  2.4. ALLOW AT MOST CHANGES BY A FACTOR 2.5 per 5-yr timestep - corresponds to about 20% per year (looking backwards from 2030)
  #  2.5. emifactor (t) = max (emifactor (t), emifactor(2030) for t < 2030
  #      CHECK: All combinations are there.


  # 3. COMBINE EMISSION FACTORS FROM 1. (2005-2030) AND FROM 2. (2030-2100) ======



  #---------------------------

  OLD # STUFF FROM HERE


  # ====================================================================
  # NA HANDLING ========================================================
  # ====================================================================
  # Assume activities and emissions with no data are zero for those
  # particular region*sector*pollutant combinations
  baseemi[is.na(baseemi)] <- 0.0
  baseact[is.na(baseact)] <- 0.0

  # EF input files are a bit messier, so only assume zeroes after
  # the time dimension is handled properly and some NAs are filled

  # ====================================================================
  # EF SCENARIO ASSUMPTIONS ============================================
  # ====================================================================

  # We're keeping only future years for the scenarios for now, as actual
  # data is only available for them. We pad them to conform to REMIND
  # timesteps later on.
  keepyears <- seq(2030, 2100, 5)

  # CLE ========================================================================
  # CLE is the only scenario with an actual SSP dimension,
  # and that includes the 2050-2100 period. All of them
  # have the baseline name. Here we keep it as is.
  # It also includes the historical period, which we need for filling in
  # some sectors/years, but we drop in cle (it's in the ssp set)
  cle <- incle[, keepyears, allssps]

  # MFR ========================================================================
  # MFR is, by definition, the maximum feasible reduction scenario. So we assume
  # the same for all SSPs. It also comes with the 2100 extension already
  mfr <- mbind(lapply(allssps, \(ssp) setItems(inmfr, "ssp", ssp)))

  # Some sectors are only present in the historical data, having NAs in the
  # period in incle[,,"historical"] but simply being missing in inmfr and insle
  # Here we bind that future period with NAs to the scenario (taking 2030 as
  # reference), so that concatenation
  # between scenarios works and we can actually fill that data if needed after
  # the historical period is also concatenated.
  padAbsentSectors <- function(magscen, incle, useyear = 2030) {
    abssectors <- setdiff(getItems(incle[, , "historical"], "sectorGAINS"), getItems(magscen, "sectorGAINS"))
    if (length(abssectors) == 0) {
      return(magscen)
    }
    dumfill <- collapseDim(incle[, keepyears, "historical"][, , abssectors], keepdim = "sectorGAINS")
    dumfill <- toolAddDimensions(
      toolAddDimensions(
        dumfill, getItems(magscen, "ssp"), "ssp", 3.1
      ), getItems(magscen, "scenario"), "scenario", 3.1
    )
    dumfill <- mbind(lapply(getYears(magscen), \(yr) setYears(dumfill[, useyear, ], yr)))
    magscen <- mbind(magscen, dumfill)
    return(magscen)
  }

  mfr <- padAbsentSectors(mfr, incle)

  # SLE ========================================================================
  # SLE is a stronger legislation scenario. All data is already included
  sle <- inmid[, keepyears, allssps]

  # See MFR comment above
  sle <- padAbsentSectors(sle, incle)

  # ScenarioMIP scenarios ========================================================================
  # The raw data is directly mapped to ScenarioMIP scenarios ("Low", "High", etc.),
  # only for a limited set of SSPs for each scenario (the ones actually used in ScenarioMIP).
  # However, those are mostly simply mapped by IIASA from a synthetic combination of SLE and CLE
  # that varies only across SSPs based on the GCI. The exception is Very Low Limited Overshot
  # (sic, typo in the actual data), which has a different scenario logic.

  # For this reason, we reduce the original scenario set to two: one which is the ScenarioMIP
  # "default" that varies only with SSPs (SMIPbySSP) and a specific one for VLLO (SMIPVLLO).
  # To facilitate ex-post combinations, we also extend the SSP coverage by making some assumptions

  ## SMIPbySSP ------------------------------------------------------------------------------------
  # Choosing arbitrary scenarios for each SSP, they should all be the same except for VLLO
  dum1 <- setItems(insmp[, , "SSP1.Medium"], "scenario", "SMIPbySSP")
  dum2 <- setItems(insmp[, , "SSP2.Medium"], "scenario", "SMIPbySSP")
  dum3 <- setItems(insmp[, , "SSP3.Medium"], "scenario", "SMIPbySSP")
  # dum4 <- setItems(insmp[, , "SSP4.Low Overshoot"], "scenario", "SMIPbySSP")
  dum5 <- setItems(insmp[, , "SSP5.High"], "scenario", "SMIPbySSP")
  # GA: Final data does not have SSP4, copying it from SSP3
  dum4 <- setItems(setItems(insmp[, , "SSP3.Medium"], "scenario", "SMIPbySSP"), "ssp", "SSP4")

  smpbyssp <- mbind(dum1, dum2, dum3, dum4, dum5)
  smpbyssp <- dimOrder(smpbyssp, perm = c(2, 1, 3, 4))
  smpbyssp <- padAbsentSectors(smpbyssp, incle)



  ## SMIPVLLO -------------------------------------------------------------------------------------
  # VLLO only contains data for SSP1 and SSP2, pad the others with SSP2
  dum1 <- setItems(setItems(insmp[, , "SSP1.Very Low Limited Overshot"], "scenario", "SMIPVLLO"), "ssp", "SSP1")
  dum2 <- setItems(setItems(insmp[, , "SSP2.Very Low Limited Overshot"], "scenario", "SMIPVLLO"), "ssp", "SSP2")
  dum3 <- setItems(setItems(insmp[, , "SSP2.Very Low Limited Overshot"], "scenario", "SMIPVLLO"), "ssp", "SSP3")
  dum4 <- setItems(setItems(insmp[, , "SSP2.Very Low Limited Overshot"], "scenario", "SMIPVLLO"), "ssp", "SSP4")
  dum5 <- setItems(setItems(insmp[, , "SSP2.Very Low Limited Overshot"], "scenario", "SMIPVLLO"), "ssp", "SSP5")

  smpvllo <- mbind(dum1, dum2, dum3, dum4, dum5)
  smpvllo <- dimOrder(smpvllo, perm = c(2, 1, 3, 4))
  smpvllo <- padAbsentSectors(smpvllo, incle)

  # Concatenating scenarios ==============================================================

  # The ScenarioMIP ones actually have valid 2025 data, so we remove it here and
  # add it later, overriding the interpolation filling step for those scenarios
  efs <- mbind(
    cle, mfr, sle,
    smpbyssp[, 2025, , invert = TRUE],
    smpvllo[, 2025, , invert = TRUE]
  )

  # Blow up dimension combinations to ensure it can be concatenated with historical
  # In particular, some sector-pollutant combinations are not present in all scenarios
  efs <- complete_magpie(efs)

  # Concatenating historical EFs to all scenarios ========================================
  histefs <- collapseDim(baseefs)
  # histefs <- collapseDim(incle[, setdiff(getYears(incle), getYears(efs)), "historical"])
  histefs <- mbind(lapply(allssps, \(ssp) add_dimension(histefs, 3.1, add = "ssp", nm = ssp)))
  histefs <- mbind(lapply(getItems(efs, "scenario"), \(scen) add_dimension(histefs, 3.1, add = "scenario", nm = scen)))

  # Blow up dimensions, see above
  histefs <- complete_magpie(histefs)

  # 2025 tends to have no data in either historical or scenarios, so interpolate
  # between 2020 (historical) and 2030 (scenario)
  efs <- mbind(histefs, efs)
  gyears <- getYears(efs, as.integer = TRUE)
  gyears <- c(gyears[gyears <= 2020], 2025, gyears[gyears >= 2030]) # In case 2025 is absent
  efs <- efs[, 2025, , invert = T] # In case 2025 is present
  efs <- toolFillYears(efs, gyears)

  # ScenarioMIP scenarios have (hopefully) properly validated
  # near term data, so use it instead of the interpolation
  efs[, 2025, "SMIPbySSP"] <- complete_magpie(smpbyssp[, 2025, ])
  efs[, 2025, "SMIPVLLO"] <- complete_magpie(smpvllo[, 2025, ])

  # NA handling in EFs ====================================================================
  # The goal is to fill every EF with something, at least zero
  # If there's some EF reported in any timestep of
  # that activity*region*scenario, fill with the temporally closest
  # one, otherwise fill with zero
  efs <- toolFillYearsWithClosest(efs)
  efs[is.na(efs)] <- 0

  # ====================================================================
  # SMOOTHING AND DERIVED SCENARIOS ====================================
  # ====================================================================
  # VLE (Very strong LEgislation) scenario
  # SLE until 2050, then converges towards MFR in 2100
  allyears <- getYears(efs, as.integer = TRUE)
  ssle <- collapseDim(efs[, , "SLE"], dim = 3.1)
  smfr <- collapseDim(efs[, , "MFR"], dim = 3.1)
  svle <- mbind(ssle[, allyears[allyears <= 2050], ], smfr[, allyears[allyears == 2100]])
  svle <- time_interpolate(svle, allyears)
  svle <- add_dimension(svle, dim = 3.1, "scenario", "VLE")
  efs <- mbind(efs, svle)

  # ====================================================================
  # EMISSIONS ==========================================================
  # ====================================================================

  # Extending activities between 2050 and 2100
  # The assumption here is that GDP growth is a proxy for polluting
  # activities. That relationship can be positive or negative, and is
  # modelled with an elasticity factor derived from the GAINS baseline.



  # ====================================================================
  # COUNTRY DISAGGREGATION AND FINAL TOUCHES ===========================
  # ====================================================================
  # Up to this point, it was simpler to deal with everything at the level
  # of GAINS regions. But with emissions and EFs calculated, we have to
  # pick the right disaggregation weights, keeping in mind EFs are
  # intensive.
  # Assume constant emissions, activities and EFs after 2100

  # Use CEDS 2020 emissions as disaggregation weights for emissions
  # and activities
  inceds <- calcOutput("AirPollEmiRef", baseyear = 2020, aggregate = FALSE)
  inceds <- fixPolNames(inceds)

  # Function to pad the magpie object with missing sectors present in seclist
  padMissingSectors <- function(mag, seclist, padval = 0) {
    padsectors <- setdiff(seclist, getItems(mag, "sectorGAINS"))
    emp <- mag[, , getItems(mag, "sectorGAINS")[1]]
    emp[, , ] <- padval
    padmat <- mbind(lapply(padsectors, \(s) setItems(emp, "sectorGAINS", s)))
    mag <- mbind(mag, padmat)
    return(mag)
  }

  if (subtype == "emissions") {
    # Emissions: Weighted by CEDS2020 Emissions in disaggregation,
    # no weights for aggregation (sum)
    csspemi <- toolAggregate(
      sspemi, regmap,
      from = "gainscode", to = "CountryCode",
      weight = inceds, dim = 1, wdim = 1
    )
    outsspemi <- time_interpolate(csspemi, rtime, extrapolation_type = "constant")
    outsspemi <- padMissingSectors(outsspemi, seclist)

    out <- outsspemi * conv_kt_to_Mt
    wgt <- NULL
    unit <- "Mt/yr"
  } else if (subtype %in% c("activities", "emission_factors")) {
    # We also need to calculate activities to use as weights when
    # asked for emissions factors

    # Activities: Weighted by CEDS2020 Emissions in disaggregation,
    # no weights for aggregation (sum)
    sspact[sspact < 0] <- 0
    csspact <- toolAggregate(
      sspact, regmap,
      from = "gainscode", to = "CountryCode",
      weight = inceds, dim = 1, wdim = 1
    )

    outsspact <- time_interpolate(csspact, rtime, extrapolation_type = "constant")
    outsspact <- padMissingSectors(outsspact, seclist)

    out <- outsspact
    wgt <- NULL
    unit <- "PJorMt/yr"
    if (subtype == "emission_factors") {
      # EFs: No disaggregation weights, intensive,
      # but carry activities as aggregation weights
      csspefs <- toolAggregate(
        efs, regmap,
        from = "gainscode", to = "CountryCode",
        weight = NULL, dim = 1, wdim = 1
      )
      outsspefs <- time_interpolate(csspefs, rtime, extrapolation_type = "constant")
      outsspefs <- padMissingSectors(outsspefs, seclist)

      out <- outsspefs * conv_kt_per_PJ_to_Tg_per_TWa
      wgt <- mbind(
        lapply(getItems(outsspefs, "scenario"), \(x) add_dimension(
          outsspact, dimCode("scenario", outsspefs), "scenario", x
        ))
      )
      unit <- "Tg/TWa"
    }
  } else {
    stop(paste0("Unknown subtype: ", subtype))
  }

  return(list(
    x = out,
    weight = wgt,
    unit = unit,
    description = "Scenario for emissions or emission factors calculated based on GAINS2025 data."
  ))
}
