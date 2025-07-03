#' Calculates air pollutant emissions, activities and emission factors
#' for all scenarios and SSPs available from GAINS, at the level
#' of GAINS sectors.
#'
#' This function is meant to be used to prepare the GAINS data
#' in the most inclusive format possible. The actual generation
#' of REMIND-specific files happens elsewhere, and uses this function.
#'
#' Extrapolates activities and emissions for the 2050-2100 periods
#' assuming a relationship between changes in polluting activities
#' and GDP.
#'
#' @return Activity levels, emissions or emission factors
#' @author Gabriel Abrahao
#' @param subtype "emission_factors", "emissions","emissions_starting_values"
#' @param agglevel "agg" or "det", sectoral aggregation level
#'
#' @importFrom magclass as.magpie
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom zoo na.approx
calcGAINS2025scenarios <- function(subtype, agglevel = "agg") {
  # ====================================================================
  # Mappings, auxiliary files and definitions ==========================
  # ====================================================================
  allssps <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")

  # conversion factors
  conv_kt_per_PJ_to_Tg_per_TWa <- 1e-3 / (1e15 / (365 * 24 * 60 * 60) * 1e-12)
  conv_kt_to_Mt <- 1e-3

  fixPolNames <- function(mag) {
    # Mapping from GAINS2025 to REMIND (oldGAINS) pollutant names
    polnamesmap <- c(
      "CO" = "CO",
      "NOx" = "NOX",
      "BC" = "PM_BC",
      "OC" = "PM_OC",
      "SO2" = "SO2",
      "NH3" = "NH3",
      "VOC" = "VOC"
    )
    mag <- mag[, , polnamesmap]
    getItems(mag, "species") <- names(polnamesmap)
    return(mag)
  }

  # REMIND timesteps
  rtime <- c(seq(2005, 2055, 5), seq(2060, 2110, 10), 2130, 2150)

  # GAINS region mapping
  # Most of the calculations here happen at the GAINS region level, as
  # readGAINS2025 does not disaggregate its outputs
  regmap <- toolGetMapping(type = "regional", name = "regionmapping_GAINS2025.csv", where = "mrremind")

  # List of valid sectors, used to pad the data
  fullseclist <- readSource("GAINS2025", subtype = "sectorlist")
  seclist <- fullseclist[, names(fullseclist) == paste0("EMF30_", toupper(agglevel))]
  seclist <- unique(seclist)

  # ====================================================================
  # READING GAINS DATA =================================================
  # ====================================================================

  # GAINS baseline scenario emissions and activities
  # Emissions seem to be in kt(pollutant)/yr, while activities have the units in seclist (mostly PJ or Mt/yr)
  baseemi <- readSource("GAINS2025", subtype = "emissions", subset = paste0("baseline.", agglevel))
  baseact <- readSource("GAINS2025", subtype = "activities", subset = paste0("baseline.", agglevel))

  # GAINS historical scenario emission facts
  baseefs <- readSource("GAINS2025", subtype = "emifacs", subset = paste0("baseline.", agglevel))

  # GAINS scenarios
  incle <- readSource("GAINS2025", subtype = "emifacs", subset = paste0("cle_rev.", agglevel))
  inmid <- readSource("GAINS2025", subtype = "emifacs", subset = paste0("middle.", agglevel))
  inmfr <- readSource("GAINS2025", subtype = "emifacs", subset = paste0("mtfr.", agglevel))
  # Using scenario names closer to the usual IIASA ones
  getItems(incle, "scenario") <- "CLE"
  getItems(inmid, "scenario") <- "SLE"
  getItems(inmfr, "scenario") <- "MFR"

  # ScenarioMIP scenarios
  insmp <- readSource("GAINS2025", subtype = "emifacs", subset = paste0("scenariomip.", agglevel))

  # ====================================================================
  # ADDING EXTENDED SECTORS ============================================
  # ====================================================================
  if (agglevel == "agg") {
    det_baseemi <- readSource("GAINS2025", subtype = "emissions", subset = paste0("baseline.", "det"))
    det_baseact <- readSource("GAINS2025", subtype = "activities", subset = paste0("baseline.", "det"))
    det_baseefs <- readSource("GAINS2025", subtype = "emifacs", subset = paste0("baseline.", "det"))
    det_incle <- readSource("GAINS2025", subtype = "emifacs", subset = paste0("cle_rev.", "det"))
    det_inmid <- readSource("GAINS2025", subtype = "emifacs", subset = paste0("middle.", "det"))
    det_inmfr <- readSource("GAINS2025", subtype = "emifacs", subset = paste0("mtfr.", "det"))
    getItems(det_incle, "scenario") <- "CLE"
    getItems(det_inmid, "scenario") <- "SLE"
    getItems(det_inmfr, "scenario") <- "MFR"
    det_insmp <- readSource("GAINS2025", subtype = "emifacs", subset = paste0("scenariomip.", "det"))

    # Sectors to extend, if present in the detailed datasets
    extsectors <- c(
      "Waste_Solid_Industrial", "Waste_Solid_Municipal", "Waste_Water_Industrial", "Waste_Water_Municipal"
    )
    # GA: In the version obtained from Zig and Shaohui in May 2025,
    # only "Waste_Solid_Municipal" and "Waste_Water_Municipal" were present
    extsectors <- extsectors[extsectors %in% getItems(det_baseemi, "sectorGAINS")]

    # Append extended and drop "Unattributed", which is mostly the waste sectors
    dropSectors <- function(mag) {
      dsecs <- c("Unattributed")
      if (any(extsectors %in% getItems(mag, "sectorGAINS"))) {
        mag <- mag[, , dsecs, invert = T]
      }
      return(mag)
    }
    baseemi <- mbind(dropSectors(baseemi), det_baseemi[, , extsectors])
    baseact <- mbind(dropSectors(baseact), det_baseact[, , extsectors])
    baseefs <- mbind(dropSectors(baseefs), det_baseefs[, , extsectors])
    incle <- mbind(dropSectors(incle), det_incle[, , extsectors])
    inmid <- mbind(dropSectors(inmid), det_inmid[, , extsectors])
    inmfr <- mbind(dropSectors(inmfr), det_inmfr[, , extsectors])
    insmp <- mbind(dropSectors(insmp), det_insmp[, , extsectors])
  }

  # ====================================================================
  # FIXING POLLUTANT NAMES TO REMIND STANDARD ==========================
  # ====================================================================
  baseemi <- fixPolNames(baseemi)
  baseact <- fixPolNames(baseact)
  baseefs <- fixPolNames(baseefs)
  incle <- fixPolNames(incle)
  inmid <- fixPolNames(inmid)
  inmfr <- fixPolNames(inmfr)
  insmp <- fixPolNames(insmp)

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
  # Here we bind that future period with NAs to the scenario, so that concatenation
  # between scenarios works and we can actually fill that data if needed after
  # the historical period is also concatenated.
  padAbsentSectors <- function(magscen, incle) {
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
    smpbyssp[, 2025, , invert = T],
    smpvllo[, 2025, , invert = T]
  )

  # Blow up dimension combinations to ensure it can be concatenated with historical
  # In particular, some sector-pollutant combinations are not present in all scenarios
  efs <- complete_magpie(efs)

  # Dropping odd sectors in the files that have no data
  # efs <- efs[, , c(" ", "Power_Gen_HLF_CCS", "Unattributed"), invert = T]

  # Concatenating historical EFs to all scenarios ========================================
  histefs <- collapseDim(baseefs)
  # histefs <- collapseDim(incle[, setdiff(getYears(incle), getYears(efs)), "historical"])
  histefs <- mbind(lapply(allssps, \(ssp) add_dimension(histefs, 3.1, add = "ssp", nm = ssp)))
  histefs <- mbind(lapply(getItems(efs, "scenario"), \(scen) add_dimension(histefs, 3.1, add = "scenario", nm = scen)))
  # histefs <- histefs[, , c("Unattributed"), invert = T]

  # Blow up dimensions, see above
  histefs <- complete_magpie(histefs)

  # 2025 tends to have no data in either historical or scenarios, so interpolate
  # between 2020 (historical) and 2030 (scenario)
  efs <- mbind(histefs, efs)
  gyears <- getYears(efs, as.integer = T)
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
  # SMOOTHING ==========================================================
  # ====================================================================
  # VLE (Very strong LEgislation) scenario
  # SLE until 2050, then converges towards MFR in 2100
  allyears <- getYears(efs, as.integer = T)
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

  # Reading GDP data from madrat for all SSPs and aggregating to
  # GAINS regions.
  gdp <- calcOutput("GDP", scenario = allssps, aggregate = F)
  gdpgains <- toolAggregate(
    gdp, regmap,
    from = "CountryCode", to = "gainscode",
    weight = NULL, dim = 1, wdim = 1
  )

  # Reference activity relative change between 2025 and 2050
  refract <- setYears(baseact[, 2050, ], NULL) / setYears(baseact[, 2025, ], NULL)
  # Reference GDP relative change between 2025 and 2050
  refrgdp <- setYears(gdpgains[, 2050, "SSP2"], NULL) / setYears(gdpgains[, 2025, "SSP2"], NULL)

  # Estimate elasticities of the GDP-activities relationship
  estela <- collapseDim(log(refract) / log(refrgdp))
  # Cap elasticies to avoid extreme values
  estela[estela > 1] <- 1
  estela[estela < -1] <- -1

  # Future GDP relative change between 2050 and 2100
  futrgdp <- setYears(gdpgains[, 2100, ], NULL) / setYears(gdpgains[, 2050, ], NULL)
  getSets(futrgdp)[getSets(futrgdp) == "variable"] <- "ssp"

  # Increment factor 2050-2100
  incfac <- futrgdp^estela
  incfac[is.na(incfac)] <- 1

  # Expand activities to include all scenarios and SSPs
  sspact <- mbind(lapply(allssps, \(ssp) add_dimension(baseact, 3.1, add = "ssp", nm = ssp)))

  # Apply increment factors and interpolate periods 2050-2100
  sspact2100 <- setYears(sspact[, 2050, ] * incfac, 2100)
  sspact <- mbind(sspact, sspact2100)
  sspact <- toolFillYears(sspact, years = getYears(efs))

  # Remove sectors absent in efs
  sspact <- sspact[, , getItems(efs, "sectorGAINS")]

  # Expand activities to ensure they will match EFs when used as weights
  # As they might be used as weights, fill with zeroes
  sspact <- complete_magpie(sspact)
  sspact[is.na(sspact)] <- 0

  # Extending emissions by applying EFs to activity levels
  # Note that none of those will be the same as the baseline, as the baseline scenario
  # is not included (CLE:current legislation is the least strict one)
  # Activities are scenario-independent, so this expands the scenario dimension
  sspemi <- efs * sspact

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
  inceds <- calcOutput("AirPollEmiRef", baseyear = 2020, aggregate = F)
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
      wgt <- mbind(lapply(getItems(outsspefs, "scenario"), \(x) add_dimension(outsspact, dimCode("scenario", outsspefs), "scenario", x)))
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
