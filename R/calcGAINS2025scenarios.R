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
calcGAINS2025scenarios <- function(subtype) {
  require(magclass)
  require(madrat)
  devtools::load_all(".")


  # ====================================================================
  # Mappings and definitions ===========================================
  # ====================================================================
  allssps <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")

  # GAINS region mapping
  # Most of the calculations here happen at the GAINS region level, as
  # readGAINS2025 does not disaggregate its outputs
  regmap <- toolGetMapping(type = "regional", name = "regionmapping_GAINS2025.csv", where = "mrremind")

  # # ====================================================================
  # # GCI ================================================================
  # # ====================================================================

  # # Read Government Capacity Index (GCI), at the country level
  # # Used to derivate scenario extensions
  # ingci <- readSource("GAINS2025", subtype = "GCI")


  # # TODO: Using GDP as aggregation weights for the moment
  # # A more correct approach would be to have a separate aggregation
  # # of both GCI and EFs for each pollutant, but it would require
  # # a different approach to filling NAs and dealing with the
  # # sector mapping before building the scenarios
  # gdp <- calcOutput("GDP", scenario = getItems(ingci, "ssp"), aggregate = FALSE, years = getYears(ingci))
  # getSets(gdp)[1] <- "country"
  # getSets(gdp)[3] <- "ssp"
  # # # Keep only countries with GCI data
  # # gdp <- gdp[getRegions(ingci),,]
  # str(gdp)


  # # Aggregating GCI to GAINS regions
  # # First we fill countries in the mapping without GCI data with region averages
  # fillingci <- mbind(lapply(
  #   getItems(ingci, "ssp"),
  #   \(ssp) toolFillWithRegionAvg(ingci[, , ssp], regionmapping = regmap, callToolCountryFill = TRUE)
  # ))

  # # Then we aggregate, taking care to use only countries in the mapping
  # gcigains <- toolAggregate(
  #   fillingci[regmap$CountryCode, , ], regmap,
  #   from = "CountryCode", to = "gainscode",
  #   weight = gdp[regmap$CountryCode, , ], dim = 1, wdim = 1
  # )

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
  efs <- efs[,,c(" ", "Power_Gen_HLF_CCS", "Unattributed"), invert = T]

  # Concatenating historical EFs to all scenarios ========================================
  histefs <- collapseDim(incle[,setdiff(getYears(incle), getYears(efs)),"historical"])
  histefs <- mbind(lapply(allssps, \(ssp) add_dimension(histefs, 3.1, add = "ssp", nm = ssp)))
  histefs <- mbind(lapply(getItems(efs,"scenario"), \(scen) add_dimension(histefs, 3.1, add = "scenario", nm = scen)))
  histefs <- histefs[,,c("Unattributed"), invert = T]

  efs <- mbind(histefs, efs)

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
  sspact <- sspact[,,getItems(efs, "sectorGAINS")]

  # Extending emissions by applying EFs to activity levels
  # Note that none of those will be the same as the baseline, as the baseline scenario
  # is not included (CLE:current legislation is the least strict one)
  sspemi <- efs * sspact
  
  # ====================================================================
  # REGIONAL DISAGGREGATION =============================================
  # ====================================================================  
  # Up to this point, it was simpler to deal with everything at the level
  # of GAINS regions. But with emissions and EFs calculated, we have to
  # pick the right disaggregation weights

  # inceds <- readSource("CEDS2025")
  # str(inceds)
  # getItems(inceds,"pollutant")


}
