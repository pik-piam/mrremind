#' Calculates air pollutant emissions, activities and emission factors
#' for all scenarios and SSPs available from GAINS, at the level
#' of GAINS sectors.
#'
#' Extrapolates activities and emissions for the 2050-2100 periods
#' assuming a relationship between changes in polluting activities
#' and GDP.
#'
#' @return Activity levels, emissions or emission factors
#' @author Gabriel Abrahao
#' @param subtype "emission_factors", "emissions","emissions_starting_values"
#'
#' @importFrom magclass as.magpie
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom zoo na.approx
calcGAINS2025scenarios <- function(subtype) {
  # require(magclass)
  # require(madrat)
  # devtools::load_all(".")


  # ====================================================================
  # Mappings and definitions ===========================================
  # ====================================================================
  allssps <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")

  # REMIND timesteps
  rtime <- c(seq(2005, 2055, 5), seq(2060, 2110, 10), 2130, 2150)

  # GAINS region mapping
  # Most of the calculations here happen at the GAINS region level, as
  # readGAINS2025 does not disaggregate its outputs
  regmap <- toolGetMapping(type = "regional", name = "regionmapping_GAINS2025.csv", where = "mrremind")

  # ====================================================================
  # READING GAINS DATA =================================================
  # ====================================================================

  # GAINS baseline scenario emissions and activities
  baseemi <- readSource("GAINS2025", subtype = "emissions", subset = "baseline.det")
  baseact <- readSource("GAINS2025", subtype = "activities", subset = "baseline.det")

  # GAINS scenarios
  incle <- readSource("GAINS2025", subtype = "emifacs", subset = "cle_rev.det")
  inmid <- readSource("GAINS2025", subtype = "emifacs", subset = "middle.det")
  inmfr <- readSource("GAINS2025", subtype = "emifacs", subset = "mtfr.det")
  # Using scenario names closer to the usual IIASA ones
  getItems(incle, "scenario") <- "CLE"
  getItems(inmid, "scenario") <- "SLE"
  getItems(inmfr, "scenario") <- "MFR"

  # ====================================================================
  # NA HANDLING ========================================================
  # ====================================================================
  # Assume activities and emissions with no data are zero for those
  # particular region*sector*pollutant combinations
  baseemi[is.na(baseemi)] <- 0.0
  baseact[is.na(baseact)] <- 0.0

  # EF input files are a bit messier, so only assume zeroes after
  # the time dimension is handled properly

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
  cle <- incle[, keepyears, allssps]

  # MFR ========================================================================
  # MFR is, by definition, the maximum feasible reduction scenario. So we assume
  # the same for all SSPs. It also comes with the 2100 extension already.
  mfr <- mbind(lapply(allssps, \(ssp) setItems(inmfr, "ssp", ssp)))

  # SLE ========================================================================
  # SLE is a stronger legislation scenario. All data is already included
  sle <- inmid[, keepyears, allssps]

  # Concatenating scenarios ==============================================================
  efs <- mbind(cle, mfr, sle)
  # Dropping odd sectors in the files that have no data
  efs <- efs[, , c(" ", "Power_Gen_HLF_CCS", "Unattributed"), invert = T]

  # Concatenating historical EFs to all scenarios ========================================
  histefs <- collapseDim(incle[, setdiff(getYears(incle), getYears(efs)), "historical"])
  histefs <- mbind(lapply(allssps, \(ssp) add_dimension(histefs, 3.1, add = "ssp", nm = ssp)))
  histefs <- mbind(lapply(getItems(efs, "scenario"), \(scen) add_dimension(histefs, 3.1, add = "scenario", nm = scen)))
  histefs <- histefs[, , c("Unattributed"), invert = T]

  # 2025 tends to have no data in historical or scenario either, so interpolate
  # between 2020 (historical) and 2030 (scenario)
  efs <- mbind(histefs, efs)
  gyears <- getYears(efs)
  efs <- efs[, 2025, , invert = T]
  efs <- toolFillYears(efs, gyears)

  # NA handling in EFs ====================================================================
  # The goal is to fill every EF with something, at least zero
  # If there's some EF reported in any timestep of 
  # that activity*region*scenario, fill with the temporally closest
  # one, otherwise fill with zero
  efs <- toolFillYearsWithClosest(efs)
  efs[is.na(efs)] <- 0

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

  # Extending emissions by applying EFs to activity levels
  # Note that none of those will be the same as the baseline, as the baseline scenario
  # is not included (CLE:current legislation is the least strict one)
  sspemi <- efs * sspact

  # ====================================================================
  # COUNTRY DISAGGREGATION =============================================
  # ====================================================================
  # Up to this point, it was simpler to deal with everything at the level
  # of GAINS regions. But with emissions and EFs calculated, we have to
  # pick the right disaggregation weights, keeping in mind EFs are
  # intensive.

  # Assume constant emissions, activities and EFs after 2100

  # Use CEDS 2020 emissions as disaggregation weights for emissions
  # and activities
  inceds <- calcOutput("AirPollEmiRef", baseyear = 2020, aggregate = F)

  if (subtype == "emissions") {
    # Emissions: Weighted by CEDS2020 Emissions in disaggregation,
    # no weights for aggregation (sum)
    csspemi <- toolAggregate(
      sspemi, regmap,
      from = "gainscode", to = "CountryCode",
      weight = inceds, dim = 1, wdim = 1
    )
    outsspemi <- time_interpolate(csspemi, rtime, extrapolation_type = "constant")

    out <- outsspemi
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

      out <- outsspefs
      wgt <- outsspact
      unit <- "Mt/PJorMt"
    }
  }

  return(list(
    x = out,
    weight = wgt,
    unit = unit,
    description = "Scenario for emissions or emission factors calculated based on GAINS2025 data."
  ))
}
