#' Calculate NDC Emissions Targets
#'
#' @description This function calculates the emissions targets for the NDC scenarios applied in the REMIND module 45_carbonprice realization NDC.
#' It contains the following steps:
# 1. Read country-level NDC targets as absolute emissions targets in MtCO2eq/yr
# 2. Make country-specific assumptions about inclusions or adaptations of NDC targets
# 3  Extrapolate NDC targets from 2030 to 2035 for countries which do not have 2035 NDC targets (yet)
# 4. Aggregate country-level absolute emissions targets to region-level absolute emissions targets by summation ("EmiTargetAbs")
# 5. Calculate share of emissions covered under NDC target per REMIND region ("Ghgshare")
#' The parameters calculated in 3.) and 4.) are further used in the NDC realization to calculate the region-wide NDC emissions targets
#' in terms of total GHG emissions excl. bunkers and excl. LULUCF sectors.
#'
#' @param sources database source, must be 'PBL_NDC'
#' @param subtype must be one of
#' - 'EmiTargetAbs': absolute emissions targets in MtCO2eq/yr
#' - 'Ghgshare': share of emissions covered under NDC target per REMIND region
#' @author Rahel Mandaroux, Felix Schreyer
#'
calcNDCTargets <- function(sources, subtype) {
  # Main steps:
  # 1. Read country-level NDC targets as absolute emissions targets in MtCO2eq/yr
  # 2. Make country-specific assumptions about inclusions or adaptations of NDC targets
  # 3  Extrapolate NDC targets from 2030 to 2035 for countries which do not have 2035 NDC targets (yet)
  # 4. Aggregate country-level absolute emissions targets to region-level absolute emissions targets by summation ("EmiTargetAbs")
  # 5. Calculate share of emissions covered under NDC target per REMIND region ("Ghgshare")

  # 1. Read country-level NDC targets as absolute emissions targets ----

  if (sources != "PBL_NDC") {
    stop("Unknown source ", sources, " for calcNDCTargets.")
  }

  if (!subtype %in% c("Ghgshare", "EmiTargetAbs")) {
    stop("Unknown 'subtype' argument")
  }

  # Reference Emissions from CEDS used for aggregation weights
  ghg <- calcOutput("EmiTargetReference", aggregate = FALSE)[, , "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"]

  # read country-level absolute emission targets from PBL ELEVATE protocol
  # detailed target formulations for major emitters, translated to absolute levels
  detailed <- readSource("PBL_NDC", subtype = "detailed")
  # pre-computed levels (excl LULUCF) for all other countries
  reduced <- readSource("PBL_NDC", subtype = "reduced")

  # combine both sources, giving the detailed major-emitter targets precedence over the reduced levels
  combined <- reduced
  combined[!is.na(detailed)] <- detailed[!is.na(detailed)]
  combined <- collapseDim(combined)

  absEmiTarget <- combined

  # 2. Make country-specific assumptions about inclusions or adaptations of NDC targets ----

  # remove US targets since under the Trump Administration the US withdrew from the Paris Agreement
  absEmiTarget["USA", , ] <- NA


  # 3. Extrapolate NDC targets from 2030 to 2035 for countries which do not have 2035 NDC targets ----

  # linear extrapolation: annual reduction rate from 2015 to 2030, projected five more years
  ghgRef2015 <- setYears(ghg[, 2015, ], NULL)
  annualReductionRate <- (absEmiTarget[, 2030, ] - ghgRef2015) / 15
  extrapolated2035 <- setYears(absEmiTarget[, 2030, ] + annualReductionRate * 5, 2035)

  # fill only countries that have no 2035 target yet
  target2035 <- absEmiTarget[, 2035, ]
  target2035[is.na(target2035)] <- extrapolated2035[is.na(target2035)]
  absEmiTarget[, 2035, ] <- target2035


  # 4. Aggregate country-level absolute emissions targets to region-level absolute emissions targets ----

  if (subtype == "EmiTargetAbs") {
    # Explanation: The absolute emissions target ("absEmiTarget") represents NDC target emissions in MtCO2eq/yr on country-level.
    # The emissions cover total GHG emissions excl. land-use change and excl. bunker emissions.
    # They are aggregated to region-level by simple summation of all countries with an NDC target:
    # absEmiTarget(region) = sum(country, absEmiTarget(country)), for all countries with NDC targets.
    # Note that this aggregation is done via the madrat routine run with the return() statement of this function.
    # Weight is set to NULL to sum all country-level targets without weights.

    # absolute emissions target as aggregation variable
    x <- absEmiTarget
    # set countries without NDC targets to 0
    # their contribution to the regional NDC target is taken care of
    # in the GAMS code of "./modules/45_carbonprice/NDC/." by adding their share of emissions from the NPI run
    x[is.na(x)] <- 0


    return(list(
      x = x,
      weight = NULL,
      unit = "MtCO2eq/yr",
      description = glue::glue("Absolute emissions targets in MtCO2eq/yr, \\
                summed for all countries with NDC target in each region per target year.")
    ))
  }

  # 5. Calculate share of emissions covered under NDC target per REMIND region ----

  if (subtype == "Ghgshare") {
    # Explanation: The share of emissions covered under NDC ("ghgShare") is an estimate of target year emissions in a REMIND region
    # from all countries that have an NDC target. It is used to proxy which share of emissions in a region should follow NDC targets and
    # which should follow the Npi scenario (for countries without target).
    # There are two steps:
    # 1. Target year emissions on country-level are projected by assuming the same growth rate of emissions as GDP:
    # Emi(target year) = Emi(2015) * GDP(target year) / GDP(2015).
    # 2. The share of emissions from countries with an NDC target in the region is calculated as:
    # ghgShare = sum(country, Emi(target year, country) / Emi(target year, region) ) for all countries that have NDC targets
    # Note this calculation is done via the madrat aggregation routine run with the return() statement of this function.


    # aggregation variable: 0/1 matrix with 1s indicating countries with target represented as absolute emissions target
    x <- 1 * (!is.na(absEmiTarget))

    # get GDP for extrapolating target year emissions needed for aggregation weights
    gdp <- collapseDim(calcOutput("GDP", scenario = "SSP2", aggregate = FALSE))

    # estimate target year emissions by multiplying 2015 emissions with GDP growth rate
    weight <- setYears(ghg[, 2015, ] / gdp[, 2015, ], NULL) * gdp[, getYears(absEmiTarget), ]

    return(list(
      x = x,
      weight = weight,
      unit = "1",
      description = glue::glue("2015 GHG emission share of countries with \\
                quantifyable emissions under NDC in particular region per target year"),
      min = 0, max = 1
    ))
  }
}
