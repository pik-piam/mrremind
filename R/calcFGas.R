#' generate F-Gases based on IMAGE data
#'
#' @param subtype "IMAGElegacy" will use the old IMAGE data, "IMAGE2025" will use the new IMAGE2025 data.
#' "Kigali2026" builds four scenarios from the ScenarioMIP data (readSource("ScenarioMIP", "FGasScen")) in four steps:
#' (1) all the scenarios of the models MESSAGEix and IMAGE are read; (2) the species that MESSAGEix does not have
#' (HFC-23, C2F6, C6F14) are taken from IMAGE, which is then dropped; (3) the scenarios are harmonized to the EDGAR7
#' emissions of 2020, "High - SSP5" returning to its own values by 2100 and "Very Low - SSP1" keeping the 2020 ratio;
#' (4) the four scenarios are built: MESSAGEHigh and MESSAGEVeryLow are the harmonized scenarios, MESSAGEKAFull and
#' MESSAGEKAImplemented are MESSAGEVeryLow with the Annex F Group I HFC species limited by the Kigali Amendment: the
#' result is the smaller of the scenario and the Kigali fraction (calcOutput("KAReductionFractions"), computed per
#' country) times the 2020 value, in all countries (MESSAGEKAFull) or only in the countries of the table of current
#' policies (readSource("KAReductions", "curpol"), the EU as its 27 member states) (MESSAGEKAImplemented). HFC-23, PFCs
#' and SF6 are not capped. The totals are rebuilt in CO2eq from the species with
#' the GWPs of AR6GWP. The result has the dimensions region, year, scenario and variable, without SSP, RCP or SPA
#' dimensions: the mapping of the scenarios to them is not done here.
#' ASSUMPTION: the HCFC part of the legal Kigali baselines is NOT considered (HFC-only baselines, see
#' calcKAReductionFractions), so the caps are tighter than the legal ones.
#' @param interp "interpolate2025" will intepolate from EDGAR historical data from 2025 to interpyear.
#' To account for the very old IMAGE scenarios in IMAGElegacy,
#' this used to be 2050 but is now flexible. Any other interp value will ignore this step.
#' @param interpyear The year to interpolate to, default is 2030.
#' interpyear is only used if interp is set to "interpolate2025".
#'
#' @return magpie object with F-gases information
#' @author Lavinia Baumstark, Gabriel Abrahao
#' @examples
#' \dontrun{
#' x <- calcOutput("FGas")
#' }
calcFGas <- function(subtype = "Kigali2026", interp = "none", interpyear = 2030) {
  # Mapping for the REMIND variable names. Works with both versions
  # of the IMAGE data
  emi_mapping <- list(
    "Emissions|F-Gases" = "emiFgasTotal",
    "Emissions|HFC" = "emiFgasHFC",
    "Emissions|HFC|HFC125" = "emiFgasHFC125",
    "Emissions|HFC|HFC134a" = "emiFgasHFC134a",
    "Emissions|HFC|HFC143a" = "emiFgasHFC143a",
    "Emissions|HFC|HFC227ea" = "emiFgasHFC227ea",
    "Emissions|HFC|HFC23" = "emiFgasHFC23",
    "Emissions|HFC|HFC245fa" = "emiFgasHFC245fa",
    "Emissions|HFC|HFC32" = "emiFgasHFC32",
    "Emissions|HFC|HFC43-10" = "emiFgasHFC43-10",
    "Emissions|PFC" = "emiFgasPFC",
    "Emissions|SF6" = "emiFgasSF6",
    "Emissions|C2F6" = "emiFgasC2F6",
    "Emissions|C6F14" = "emiFgasC6F14",
    "Emissions|CF4" = "emiFgasCF4"
  )

  if (subtype == "IMAGElegacy") {
    # read in IMAGE data
    x <- readSource("IMAGE")
    # make the dimension needed for REMIND
    getNames(x, dim = 1) <- gsub("-", ".", getNames(x, dim = 1))
    x <- clean_magpie(x, what = "sets")
    x <- collapseNames(x)
    getSets(x)[2] <- "Year"
    getSets(x)[4] <- "rcp"
    getSets(x)[5] <- "SPAscen"
    getSets(x)[6] <- "Variable"
    getSets(x)[7] <- "Unit"

    # change names to the REMIND-set-names
    getNames(x, dim = 1) <- paste0("forcing_", getNames(x, dim = 1))
    getNames(x, dim = 2) <- gsub("34", "37", getNames(x, dim = 2))
    getNames(x, dim = 2) <- paste0("rcp", getNames(x, dim = 2))
    getNames(x, dim = 2) <- gsub("rcpRef", "none", getNames(x, dim = 2))
    getNames(x, dim = 3) <- gsub("[1-9]", "x", getNames(x, dim = 3))

    xCO2 <- x[, , "Emissions|CO2|Fossil Fuels and Industry"]
    x <- x[, , "Emissions|CO2|Fossil Fuels and Industry", invert = TRUE]


    getNames(x, dim = 4) <- emi_mapping[getNames(x, dim = 4)]

    # remove the unit dimension
    x <- collapseNames(x, collapsedim = 5)
    xCO2 <- collapseNames(xCO2, collapsedim = 4)

    # To calculate SSP4 and SSP5 policy emissions we scale SSP2 policy F-gas emissions
    # with SSP5 F-gas baselines and correct for the ratio of CO2 baseline
    # emissions which is a proxy for the CO2 mitigation effort and therefore
    # for the CO2 price.
    # Fgas_SSP5_pol = Fgas_SSP2_pol *
    # (Fgas_SSP5_ref / Fgas_SSP2_ref) *
    # (CO2_SSP5_ref / CO2_SSP2_ref)

    x_SSP5 <- new.magpie(getRegions(x), getYears(x), getNames(x[, , "forcing_SSP2"]))
    getNames(x_SSP5) <- gsub("forcing_SSP2", "forcing_SSP5", getNames(x_SSP5))
    x_SSP5 <- x_SSP5[, , "none", invert = TRUE]

    # loop over rcp scenarios
    for (r in getNames(x_SSP5, dim = 2)) {
      x_SSP5[, , "forcing_SSP5"][, , r] <- collapseNames(x[, , "forcing_SSP2"])[, , r] *
        (collapseNames(x[, , "forcing_SSP5"][, , "none"]) / collapseNames(x[, , "forcing_SSP2"][, , "none"])) *
        (collapseNames(xCO2[, , "forcing_SSP5"][, , "none"]) / collapseNames(xCO2[, , "forcing_SSP2"][, , "none"]))
    }
    x <- mbind(x, x_SSP5)
    x[is.na(x)] <- 0

    # interpolate time to match REMIND years
    rem_years <- c(seq(2015, 2105, 10), seq(2110, 2150, 5))
    rem_years <- seq(2005, 2150, 5)
    x <- time_interpolate(x, interpolated_year = rem_years, extrapolation_type = "constant")

    # add data for rcp 2.0, use data from rcp 2.6
    x_20 <- x[, , "rcp26"]
    getNames(x_20) <- gsub("rcp26", "rcp20", getNames(x_20))
    x <- mbind(x, x_20)

    # If we're not doing anything else, the output is just x
    outx <- x
  } else if (subtype == "IMAGE2025") {
    # Read the IMAGE2025 data
    mdata <- readSource("IMAGE2025")

    # For IMAGE2025, the C6F14 emissions are not available, so we remove it from the mapping
    emi_mapping <- emi_mapping[emi_mapping != "emiFgasC6F14"]

    # Keep only relevant variables
    mdata <- mdata[, , names(emi_mapping)]

    # Change variable names to the REMIND-set-names
    getNames(mdata, dim = "variable") <- emi_mapping[getNames(mdata, dim = "variable")]

    # Remove the unit dimension, refer to the original readIMAGE2025 object
    mdata <- collapseNames(mdata, collapsedim = "unit")

    # Collapse the data to a single scenario, and add dimensions for SSP, RCP and SPA
    formatScen <- function(scendata, outssp, outrcp, outspa = "SPA0") {
      # Collapse the data to a single scenario
      scen <- collapseDim(scendata)

      # Add dimensions for SSP, RCP and SPA
      scen <- add_dimension(scen, dim = 3.1, add = "Scenario", nm = outssp)
      scen <- add_dimension(scen, dim = 3.1, add = "rcp", nm = outrcp)
      scen <- add_dimension(scen, dim = 3.1, add = "SPAscen", nm = outspa)

      return(scen)
    }

    # Mix long-term and short-term scenarios, assuming that the short-term scenario
    # only has valid data until 2050 and always keeping the lower emissions between the two
    mixScenarios <- function(mdata, lngscen = "SSP1_SPA1_19I_D", shtscen = "CP_GI") {
      sht <- collapseDim(mdata[, , shtscen])
      lng <- collapseDim(mdata[, , lngscen])

      sht <- time_interpolate(
        sht[, (getYears(sht, as.integer = TRUE) <= 2050), ],
        interpolated_year = getYears(lng),
        extrapolation_type = "constant"
      )
      msk <- (lng <= sht)
      res <- sht
      res[msk] <- lng[msk]
      return(res)
    }

    # Combine scenarios for SSP2 and SSP1. The logic here is that all RCPs except RCP60
    # abide by the Kigali Amendment, by using the short-term development of the CP_GI
    # scenario until each original RCP leads to less emissions than the CP_GI scenario.
    out <- mbind(
      formatScen(mixScenarios(mdata, "SSP1_SPA1_19I_D"), "forcing_SSP1", "rcp20", "SPA0"),
      formatScen(mixScenarios(mdata, "SSP1_SPA1_26I_D"), "forcing_SSP1", "rcp26", "SPA0"),
      formatScen(mixScenarios(mdata, "SSP1_SPA1_26I_D"), "forcing_SSP1", "rcp37", "SPA0"),
      formatScen(mixScenarios(mdata, "SSP1_SPA1_26I_D"), "forcing_SSP1", "rcp45", "SPA0"),
      formatScen(mdata[, , "SSP1_SPA1_34I_D"], "forcing_SSP1", "rcp60", "SPA0"),
      formatScen(mdata[, , "SSP1-baseline"], "forcing_SSP1", "none", "SPA0"),
      formatScen(mixScenarios(mdata, "SSP2_SPA2_19I_D"), "forcing_SSP2", "rcp20", "SPA0"),
      formatScen(mixScenarios(mdata, "SSP2_SPA2_26I_D"), "forcing_SSP2", "rcp26", "SPA0"),
      formatScen(mixScenarios(mdata, "SSP2_SPA2_26I_D"), "forcing_SSP2", "rcp37", "SPA0"),
      formatScen(mixScenarios(mdata, "SSP2_SPA2_26I_D"), "forcing_SSP2", "rcp45", "SPA0"),
      formatScen(mdata[, , "SSP2_SPA2_45I_D"], "forcing_SSP2", "rcp60", "SPA0"),
      formatScen(mdata[, , "SSP2-baseline"], "forcing_SSP2", "none", "SPA0")
    )

    # Fill SSP3-5 scenarios with SSP2 data, as they are not available in IMAGE2025
    out <- mbind(
      out,
      setItems(out[, , "forcing_SSP2"], "Scenario", "forcing_SSP3"),
      setItems(out[, , "forcing_SSP2"], "Scenario", "forcing_SSP4"),
      setItems(out[, , "forcing_SSP2"], "Scenario", "forcing_SSP5")
    )

    # # Also fill the RCP "none" with RCP60
    # out <- mbind(
    #   out,
    #   setItems(out[, , "rcp60"], "rcp", "none")
    # )


    # Reorder and adjust dimensions
    out <- dimOrder(out, perm = c(3, 2, 1, 4))
    getSets(out)[getSets(out) == "variable"] <- "Variable"

    # Extrapolate and subset periods to match REMIND years
    rem_years <- c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)
    outx <- time_interpolate(out, interpolated_year = rem_years, extrapolation_type = "constant")

    # If we are not doing anything else, the output is just outx. If we are doing the interpolation,
    # keep the x naming
    x <- outx
  } else if (subtype == "Kigali2026") {
    # The species table links the ScenarioMIP variables to the REMIND variables and to the AR6 names of the gases
    spt <- readSource("ScenarioMIP", subtype = "FGasSpecies", convert = FALSE)
    gwpAR6 <- readSource("AR6GWP")
    spt$gwp_ar6 <- gwpAR6$GWP[match(spt$ar6_gas, gwpAR6$Gas)]

    # Step 1: read all the scenarios of the ScenarioMIP data, at country level and in kt of each gas per year: the
    # models MESSAGEix and IMAGE, each with the scenarios "Very Low - SSP1" and "High - SSP5"
    x <- readSource("ScenarioMIP", subtype = "FGasScen")

    # Step 2: some species are missing in the MESSAGEix scenarios, so for those the values of the IMAGE scenario with
    # the same name are used. These are the species, listed one by one:
    #  - HFC-23: it is zero in all the MESSAGEix scenarios, in all regions and years
    #  - C2F6 and C6F14: MESSAGEix does not report them at all
    # After this step IMAGE is not needed any more and is dropped from the data
    fillFromImage <- c("Emissions|HFC|HFC23", "Emissions|C2F6", "Emissions|C6F14")
    xMessage <- collapseNames(x[, , "MESSAGEix"])
    xImage <- collapseNames(x[, , "IMAGE"])
    # Of the MESSAGEix species only the ones that are not in the list are kept (C2F6 and C6F14 are not there anyway)
    xMessage <- xMessage[, , setdiff(getItems(xMessage, dim = "variable"), fillFromImage)]
    x <- mbind(xMessage, xImage[, , fillFromImage])
    # Only the species of the table are kept. The total "Emissions|F-Gases" of the database is not used, the total is
    # calculated below from the species
    x <- x[, , spt$smip_variable]

    # Step 3: harmonization to EDGAR7. The scenarios start from their own 2020 levels, which are far from EDGAR7 for
    # many species. For each country and species, the ratio between the EDGAR7 and the scenario emissions in 2020 is the
    # correction. The scenario is multiplied by 1 + (ratio - 1) x weight, where the weight says how much of the
    # correction still applies in each year. Two weights are used, as vectors that only have the time dimension:
    #  - for the "High - SSP5" scenario the weight is 1 in 2020 and goes linearly to 0 in 2100: the emissions match
    #    EDGAR7 exactly in 2020 and move towards the values of the scenario, which they reach in 2100
    #  - for the "Very Low - SSP1" scenario the weight is 1 in all years, which keeps the 2020 ratio in all years, so
    #    the relative changes of the scenario are the same as before
    # Where the scenario is zero in 2020 the ratio cannot be calculated, and there the scenario is not changed (ratio 1)
    remainingHigh <- as.magpie(data.frame(region = "GLO", year = c(2020, 2100), value = c(1, 0)),
                               spatial = 1, temporal = 2)
    remainingHigh <- time_interpolate(remainingHigh, 2020:2100, integrate_interpolated_years = TRUE)
    remainingLow <- as.magpie(data.frame(region = "GLO", year = c(2020, 2100), value = c(1, 1)),
                              spatial = 1, temporal = 2)
    remainingLow <- time_interpolate(remainingLow, 2020:2100, integrate_interpolated_years = TRUE)

    # EDGAR7 emissions of each country and species in 2020, in kt of each gas, with the names of the scenario variables.
    # Summing over the single year drops the year dimension, as for the scenarios below
    edgar2020 <- readSource("EDGAR7Fgases")[, 2020, spt$remind_variable]
    getNames(edgar2020) <- spt$smip_variable
    edgar2020 <- dimSums(edgar2020, dim = 2)

    # "High - SSP5"
    xHigh <- collapseNames(x[, , "High - SSP5"])
    ratioHigh <- edgar2020
    getSets(ratioHigh) <- getSets(xHigh)
    ratioHigh <- ratioHigh / dimSums(xHigh[, 2020, ], dim = 2)
    ratioHigh[is.na(ratioHigh) | is.infinite(ratioHigh)] <- 1
    weightHigh <- collapseNames(magpie_expand(remainingHigh, xHigh))
    xHigh <- xHigh * (1 + (magpie_expand(ratioHigh, xHigh) - 1) * weightHigh)

    # "Very Low - SSP1"
    xLow <- collapseNames(x[, , "Very Low - SSP1"])
    ratioLow <- edgar2020
    getSets(ratioLow) <- getSets(xLow)
    ratioLow <- ratioLow / dimSums(xLow[, 2020, ], dim = 2)
    ratioLow[is.na(ratioLow) | is.infinite(ratioLow)] <- 1
    weightLow <- collapseNames(magpie_expand(remainingLow, xLow))
    xLow <- xLow * (1 + (magpie_expand(ratioLow, xLow) - 1) * weightLow)

    # Step 4: the four scenarios of the result, all at country level and in kt of each gas per year:
    #  - MESSAGEHigh and MESSAGEVeryLow: the harmonized scenarios "High - SSP5" and "Very Low - SSP1"
    #  - MESSAGEKAFull and MESSAGEKAImplemented: MESSAGEVeryLow limited by the Kigali Amendment, in all countries
    #    (KAFull) or only in the countries whose current policies include it (KAImplemented). The limits are
    #    calculated in this step

    # The Annex F Group I HFCs are the ones capped by the Kigali Amendment. HFC-23 (Group II, which has a destruction
    # duty but no cap), the PFCs and SF6 are not covered by the Amendment and keep the scenario values
    hfcGroupI <- c("Emissions|HFC|HFC125", "Emissions|HFC|HFC134a", "Emissions|HFC|HFC143a", "Emissions|HFC|HFC227ea",
                   "Emissions|HFC|HFC245fa", "Emissions|HFC|HFC32", "Emissions|HFC|HFC43-10")
    hfcAll <- c(hfcGroupI, "Emissions|HFC|HFC23")
    pfcAll <- c("Emissions|CF4", "Emissions|C2F6", "Emissions|C6F14")

    # The Kigali fractions per country and year, the same for all Group I species (see calcKAReductionFractions). They
    # are the allowed share of the 2020 emissions of the country and never above 1
    ka <- calcOutput("KAReductionFractions", aggregate = FALSE)[, getItems(xLow, dim = 2), ]
    ka <- collapseDim(ka)

    # The value of each country and Group I species in 2020, before any cap. After the harmonization this is the EDGAR7
    # level (except where the scenario was zero), which is the level the Kigali fractions are defined on. The cap of a
    # country is its Kigali fraction times this value. Summing over the single year drops the year dimension, so that
    # it can be combined with the fractions of all years
    ref2020 <- dimSums(xLow[, 2020, hfcGroupI], dim = 2)

    # The Kigali group of each country, as masks: 1 if the country is in the group, 0 if not
    grp <- readSource("KAReductions", subtype = "groups")

    # This is the place where the Kigali fractions meet the scenario. For each country and Group I species the result is
    # the smaller of the scenario value and the cap, min(scenario, cap) (magclass has no pmin, so the part of the
    # scenario above the cap, the excess, is calculated; removing it gives the minimum). If the scenario is already
    # below the cap nothing is removed. The cap only applies from the first year in which the group is bound by the
    # treaty, before that the scenario is not touched. Fractions are never above 1, so until the first cut of the group
    # bites the cap is simply the 2020 level (the freeze). One block per group, with the start year of the group. Each
    # block ends with the reduction that its countries get, which is later removed from MESSAGEVeryLow

    # Non-Article 5 (developed countries): there is no freeze, the first cut already applies in 2019, so the cap is on
    # for all years of the scenario
    isNonA5 <- collapseDim(grp[, , "nonA5"])
    startNonA5 <- 2019
    yearsNonA5 <- 0 * ka
    yearsNonA5[, getYears(ka, as.integer = TRUE) >= startNonA5, ] <- 1
    appliesNonA5 <- isNonA5 * yearsNonA5
    capNonA5 <- ka * ref2020
    excessNonA5 <- xLow[, , hfcGroupI] - capNonA5
    excessNonA5[excessNonA5 < 0] <- 0
    reductionNonA5 <- excessNonA5 * appliesNonA5

    # Non-Article 5 countries with special early steps (Belarus, Russia, Kazakhstan, Tajikistan and Uzbekistan): their
    # first step is in 2020, there is no freeze, so the cap is on from 2020 (all years of the scenario)
    isNonA5Special <- collapseDim(grp[, , "nonA5_special"])
    startNonA5Special <- 2020
    yearsNonA5Special <- 0 * ka
    yearsNonA5Special[, getYears(ka, as.integer = TRUE) >= startNonA5Special, ] <- 1
    appliesNonA5Special <- isNonA5Special * yearsNonA5Special
    capNonA5Special <- ka * ref2020
    excessNonA5Special <- xLow[, , hfcGroupI] - capNonA5Special
    excessNonA5Special[excessNonA5Special < 0] <- 0
    reductionNonA5Special <- excessNonA5Special * appliesNonA5Special

    # Article 5 Group 1 (developing countries not in Group 2, this includes China): the treaty starts to bind with the
    # freeze in 2024, so that is the first year of the cap. Before 2024 the scenario is left alone
    isA5G1 <- collapseDim(grp[, , "A5G1"])
    startA5G1 <- 2024
    yearsA5G1 <- 0 * ka
    yearsA5G1[, getYears(ka, as.integer = TRUE) >= startA5G1, ] <- 1
    appliesA5G1 <- isA5G1 * yearsA5G1
    capA5G1 <- ka * ref2020
    excessA5G1 <- xLow[, , hfcGroupI] - capA5G1
    excessA5G1[excessA5G1 < 0] <- 0
    reductionA5G1 <- excessA5G1 * appliesA5G1

    # Article 5 Group 2 (India, Pakistan, Iran, Iraq, Bahrain, Kuwait, Oman, Qatar, Saudi Arabia, UAE): the treaty
    # starts to bind with the freeze in 2028, so that is the first year of the cap. Before 2028 the scenario is
    # left alone
    isA5G2 <- collapseDim(grp[, , "A5G2"])
    startA5G2 <- 2028
    yearsA5G2 <- 0 * ka
    yearsA5G2[, getYears(ka, as.integer = TRUE) >= startA5G2, ] <- 1
    appliesA5G2 <- isA5G2 * yearsA5G2
    capA5G2 <- ka * ref2020
    excessA5G2 <- xLow[, , hfcGroupI] - capA5G2
    excessA5G2[excessA5G2 < 0] <- 0
    reductionA5G2 <- excessA5G2 * appliesA5G2

    # The groups do not overlap, so each country gets the reduction of its own group only (the others are zero for it)
    reductionKA <- reductionNonA5 + reductionNonA5Special + reductionA5G1 + reductionA5G2
    # The years up to 2029 (inclusive) are taken as history and are not modified: the reduction is frozen at zero for
    # them, so that MESSAGEKAFull and MESSAGEKAImplemented are the same as MESSAGEVeryLow until 2029, and the caps only
    # act from 2030 on
    reductionKA[, getYears(reductionKA, as.integer = TRUE) <= 2029, ] <- 0

    # MESSAGEKAFull: all countries get the reduction
    xKAFull <- xLow
    xKAFull[, , hfcGroupI] <- xLow[, , hfcGroupI] - reductionKA

    # MESSAGEKAImplemented: only the countries whose current policies include the Kigali Amendment get the reduction.
    # They are the countries of the table KACurPol.csv (from the Current Policies Protocol). The table is only used for
    # this list of countries: the reduction they get is the one calculated above for their Kigali group, not the
    # targets of the table. The EU is a single entry of the table, it stands for its 27 member states. As a mask: 1 if
    # the country gets the reduction, 0 if not
    curpol <- readSource("KAReductions", subtype = "curpol", convert = FALSE)
    eu27 <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL",
              "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")
    curpolCountries <- unique(c(setdiff(curpol$iso, "EU"), if ("EU" %in% curpol$iso) eu27))
    isCurPol <- 0 * ka
    isCurPol[curpolCountries, , ] <- 1
    xKAImplemented <- xLow
    xKAImplemented[, , hfcGroupI] <- xLow[, , hfcGroupI] - reductionKA * isCurPol

    # The four scenarios in one object, with the scenario as a dimension in front of the species
    scenarioNames <- c("MESSAGEHigh", "MESSAGEVeryLow", "MESSAGEKAFull", "MESSAGEKAImplemented")
    x <- mbind(add_dimension(xHigh, dim = 3.1, add = "Scenario", nm = "MESSAGEHigh"),
               add_dimension(xLow, dim = 3.1, add = "Scenario", nm = "MESSAGEVeryLow"),
               add_dimension(xKAFull, dim = 3.1, add = "Scenario", nm = "MESSAGEKAFull"),
               add_dimension(xKAImplemented, dim = 3.1, add = "Scenario", nm = "MESSAGEKAImplemented"))

    # The AR6 GWP100 of each species, one global value per species, with the same scenarios, species and set names as x
    # so that the multiplication below is element-wise and does not build a cross product
    gwp <- new.magpie("GLO", NULL, spt$smip_variable, fill = spt$gwp_ar6, sets = c("region", "year", "variable"))
    gwp <- add_dimension(gwp, dim = 3.1, add = "Scenario", nm = scenarioNames)
    # kt of each gas times its GWP gives kt CO2eq, divided by 1000 for Mt CO2eq
    co2 <- x * gwp / 1000
    gwpHFC134a <- gwpAR6$GWP[gwpAR6$Gas == "HFC-134a"]
    gwpCF4 <- gwpAR6$GWP[gwpAR6$Gas == "PFC-14"]

    # The REMIND variables, in the units of f_emiFgas, for each scenario
    # The species, kt of each gas per year, with the REMIND names of the table
    species <- x
    getItems(species, dim = 3.2) <- spt$remind_variable[match(getItems(species, dim = 3.2), spt$smip_variable)]
    # The total of all the species of the table with the AR6 GWPs, which is why it is not the total of the database.
    # Mt CO2eq per year
    total <- add_dimension(dimSums(co2, dim = 3.2), dim = 3.2, add = "variable", nm = "emiFgasTotal")
    # All HFCs, including HFC-23, in kt HFC134a-equivalent per year (Mt CO2eq times 1000, divided by the GWP of
    # HFC-134a)
    hfc <- dimSums(co2[, , hfcAll], dim = 3.2) * 1000 / gwpHFC134a
    hfc <- add_dimension(hfc, dim = 3.2, add = "variable", nm = "emiFgasHFC")
    # All PFCs (CF4, C2F6 and C6F14) in kt CF4-equivalent per year
    pfc <- dimSums(co2[, , pfcAll], dim = 3.2) * 1000 / gwpCF4
    pfc <- add_dimension(pfc, dim = 3.2, add = "variable", nm = "emiFgasPFC")
    mdata <- mbind(species, total, hfc, pfc)
    # The variables of the other subtypes, without C6F14
    mdata <- mdata[, , unlist(emi_mapping[emi_mapping != "emiFgasC6F14"], use.names = FALSE)]

    # The result has the four scenarios and the REMIND variables. It has no SSP, RCP or SPA dimensions: the mapping of
    # the scenarios to them is done later
    out <- mdata
    getSets(out)[getSets(out) == "variable"] <- "Variable"
    # The REMIND years. The scenario has no values before 2020 and after 2100: the years in between are interpolated
    # linearly, the earlier years are a constant extension of 2020 and 2110, 2130 and 2150 a constant extension of 2100
    rem_years <- c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)
    outx <- time_interpolate(out, interpolated_year = rem_years, extrapolation_type = "constant")
    x <- outx
  } else {
    stop("Invalid subtype. Please use 'IMAGE2025', 'IMAGElegacy' or 'Kigali2026'")
  }


  if (interp == "interpolate2025") {
    # Read EDGAR7 F-gas emissions, already matching the standard used here
    edgar_all <- readSource("EDGAR7Fgases")
    getSets(edgar_all)[3] <- "Variable"

    # Years to interpolate the scenarios to
    scenyears <- getYears(x, as.integer = T)

    # Extrapolate 2025 with the 2010-2020 average growth
    edgar2025 <- setYears(edgar_all[, 2020, ] + 0.5 * (edgar_all[, 2020, ] - edgar_all[, 2010, ]), 2025)

    # Basis for interpolation
    xpts <- x[, c(scenyears[scenyears <= 2025 | scenyears >= interpyear]), ]

    # Fill the years for which we do have EDGAR data with it, and 2025
    commonyears <- intersect(getYears(edgar_all), getYears(xpts))
    commonvars <- intersect(getItems(edgar_all, "Variable"), getItems(xpts, "Variable"))
    xpts[, commonyears, commonvars] <- edgar_all[, commonyears, commonvars]
    xpts[, 2025, commonvars] <- edgar2025[, 2025, commonvars]

    # Interpolate the years between 2025 and interpyear, which
    # are the only ones not in xpts
    outx <- toolFillYears(xpts, scenyears)
  }

  return(list(
    x = outx,
    weight = NULL,
    unit = "Mt CO2-equiv/yr, kt HFC134a-equiv/yr, kt HFC125/yr, kt HFC134a/yr, kt HFC143a/yr, kt HFC227ea/yr, kt HFC23/yr, kt HFC245fa/yr, kt HFC32/yr, kt HFC43-10/yr, kt CF4-equiv/yr, kt SF6/yr, kt C2F6/yr, kt C6F14/yr, kt CF4/yr",
    description = "F-gases prescribed scenarios from ScenarioMIP, harmonized to EDGAR7 and capped by the Kigali Amendment"
  ))
}
