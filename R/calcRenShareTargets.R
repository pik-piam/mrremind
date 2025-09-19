#' Calculate Renewable Energy Share Targets
#'
#' @description This function calculates the renewable energy share targets by aggregating country-level targets to targets of REMIND regions. It calculates
#' the following types of share targets: 1) share of renewables in electricity, 2) share of non-biomass renewables in electricty,
#' 3) share of non-fossil generation in  electricity, 4) share of renewables in total final energy.
#' First, targets are aggregated to one of these 4 types. Second, countries without targets are assumed to maintain their renewable share from 2020.
#' Third, targets are harmonized within a REMIND region and filtered based on how many countries within a REMIND region have a target. Finally, energy share targets are aggregated from country-level
#' to REMIND region-level using projections of electricity or final energy demand on country-level. These country-level projections are
#' derived from final energy trends by REMIND regions from the EDGE models as well as GDP trends by country from SSP scenarios (see toolCalcEnergyProj).
#'
#' @param sources "NewClimate"
#' @author Felix Schreyer
#'
#' @importFrom quitte interpolate_missing_periods as.quitte
#'

calcRenShareTargets <- function(sources) {

  # The aggregation of renewable share targets from countries to REMIND regions is done with the following method:
  # RenShareTarget(REMINDregion,targetYear) = sum over all countries of
  # TotalDemand(country,targetYear) / TotalDemand(REMINDregion,targetYear) * RenShareTarget(country,targetYear).
  # RenShareTarget - is the renewable share target
  # TotalDemand - is the total demand,
  # i.e. in case of renewable electricity share targets it is total final electricity demand,
  # in case of renewable final energy share targets it is total final energy demand
  # for countries without targt it is assumed that they maintain their renewable share in the target year:
  # RenShareTarget(countriesWithoutTarget,targetYear) = historical renewable share
  # TotalDemand(country,targetYear) is calculated by toolCalcEnergyProj() based on FE trajectories of EDGE models and GDP from SSP scenarios
  # The function prepares RenShareTarget and TotalDemand and provides it as arguments x and weight respectively to the madrat aggregation routine,
  # which implements the above method.

  # The preparation makes the following steps:
  # 1. get data and select target types to be considered
  # 2. get historic renewable shares and totals
  # 3. extrapolate targets to REMIND time steps
  # 4. make assumptions about target development over time for countries with and without target
  # 5. select which REMIND regions get targets and determine their respective target year
  # 6. get total demand in target year to be used as aggregation weights
  # 7. hand-over to madrat for aggregation via weighted sum

  ### define functions ----

  # function to get historical 2020 renewable energy shares as well as
  # energy totals for the respective target types
  # (e.g. for target type "SE|Electricity|Renewable",
  # xHistShare is renewable electricity share and
  # xHistTotal is total electricity generation.)
  getHistEnergyShare <- function(ShareTypes, AvgSeveralYears) {
    # get historical FE and SE data from IEA energy balances
    IEA <- calcOutput("FE", source = "IEA", ieaVersion = "default", aggregate = F)

    # create array for historical energy shares and totals
    xHistShare <- new.magpie(getItems(IEA, dim = 1), years = NULL, names = ShareTypes, fill = NA)
    xHistTotal <- new.magpie(getItems(IEA, dim = 1), years = NULL, names = ShareTypes, fill = NA)

    for (targetType in ShareTypes) {
      if (targetType == "FE|Renewable") {
        # variables to sum for renewable final energy
        # for electricity and heat count SE renewable as FE renewable
        share.vars.toSum <- c(
          "SE|Electricity|Biomass (EJ/yr)",
          "SE|Electricity|Solar (EJ/yr)",
          "SE|Electricity|Wind (EJ/yr)",
          "SE|Electricity|Hydro (EJ/yr)",
          "SE|Electricity|Geothermal (EJ/yr)",
          "SE|Heat|Biomass (EJ/yr)",
          "SE|Heat|Geothermal|HP (EJ/yr)",
          "FE|Transport|Liquids|Biomass (EJ/yr)",
          "FE|Buildings|Liquids|Biomass (EJ/yr)",
          "FE|Industry|Liquids|Biomass (EJ/yr)",
          "FE|Buildings|Solids|Biomass (EJ/yr)",
          "FE|Industry|Solids|Biomass (EJ/yr)",
          "FE|Buildings|Gases|Biomass (EJ/yr)",
          "FE|Industry|Gases|Biomass (EJ/yr)"
        )
        # variables to sum for total final energy
        total.vars.toSum <- c(
          "FE|Buildings (EJ/yr)",
          "FE|Industry (EJ/yr)",
          "FE|Transport (EJ/yr)"
        )
      } else if (targetType == "SE|Electricity|Renewable") {
        # variables to sum for renewable electricity
        share.vars.toSum <- c(
          "SE|Electricity|Biomass (EJ/yr)",
          "SE|Electricity|Solar (EJ/yr)",
          "SE|Electricity|Wind (EJ/yr)",
          "SE|Electricity|Hydro (EJ/yr)",
          "SE|Electricity|Geothermal (EJ/yr)"
        )

        # variables to sum for total electricity
        total.vars.toSum <- c(
          "SE|Electricity|Coal (EJ/yr)",
          "SE|Electricity|Gas (EJ/yr)",
          "SE|Electricity|Oil (EJ/yr)",
          "SE|Electricity|Nuclear (EJ/yr)",
          share.vars.toSum
        )
      } else if (targetType == "SE|Electricity|Non-Biomass Renewable") {
        # variables to sum for non-biomass renewable electricity
        share.vars.toSum <- c(
          "SE|Electricity|Solar (EJ/yr)",
          "SE|Electricity|Wind (EJ/yr)",
          "SE|Electricity|Hydro (EJ/yr)",
          "SE|Electricity|Geothermal (EJ/yr)"
        )

        # variables to sum for total electricity
        total.vars.toSum <- c(
          "SE|Electricity|Coal (EJ/yr)",
          "SE|Electricity|Gas (EJ/yr)",
          "SE|Electricity|Oil (EJ/yr)",
          "SE|Electricity|Nuclear (EJ/yr)",
          "SE|Electricity|Biomass (EJ/yr)",
          share.vars.toSum
        )
      } else if (targetType == "SE|Electricity|Non-Fossil") {
        # variables to sum for non-fossil electricity
        share.vars.toSum <- c(
          "SE|Electricity|Solar (EJ/yr)",
          "SE|Electricity|Wind (EJ/yr)",
          "SE|Electricity|Hydro (EJ/yr)",
          "SE|Electricity|Geothermal (EJ/yr)",
          "SE|Electricity|Nuclear (EJ/yr)",
          "SE|Electricity|Biomass (EJ/yr)"
        )

        # variables to sum for total electricity
        total.vars.toSum <- c(
          "SE|Electricity|Coal (EJ/yr)",
          "SE|Electricity|Gas (EJ/yr)",
          "SE|Electricity|Oil (EJ/yr)",
          share.vars.toSum
        )
      }

      xHistShareVar <- dimSums(IEA[, , share.vars.toSum], dim = 3)
      xHistTotalVar <- dimSums(IEA[, , total.vars.toSum], dim = 3)

      if (AvgSeveralYears) {
        # average over 2018-2022
        AvgYears <- c("y2018", "y2019", "y2020", "y2021", "y2022")
        xHistShareVar <- collapseDim(dimSums(xHistShareVar[, AvgYears, ], dim = 2) / length(AvgYears))
        xHistTotalVar <- collapseDim(dimSums(xHistTotalVar[, AvgYears, ], dim = 2) / length(AvgYears))
      } else {
        # only get 2020 year
        xHistShareVar <- collapseDim(xHistShareVar[, "y2020", ])
        xHistTotalVar <- collapseDim(xHistTotalVar[, "y2020", ])
      }

      # calculate renewable / non-fossil share in historical year by dividing share variable by total variable
      xHistShare[, , targetType] <- xHistShareVar / xHistTotalVar
      # for countries without data, set to share to 0
      xHistShare[is.na(xHistShare)] <- 0
      # get total
      xHistTotal[, , targetType] <- xHistTotalVar
    }

    return(list(xHistShare,xHistTotal))
  }


  # function to extrapolate target years which are not REMIND timesteps to REMIND timesteps
  extrapolateTargetYears <- function(x, xHist, targetYears) {
    # generate new object x_target that has values only for targetYears and transfer those from x
    x_target <- new.magpie(getItems(x, dim = 1), targetYears, getNames(x), fill = NA)
    x_target[, intersect(getYears(x), getYears(x_target)), ] <- x[, intersect(getYears(x), getYears(x_target)), ]

    for (targetYear in getYears(x, as.integer = TRUE)) {
      # if target year is outisde of REMIND five-year time steps
      if (targetYear %% 5 != 0) {
        # calculate annual change rate between 2020 historical share and target year share
        ShareChangeRate <- (x[, targetYear, ] - xHist) / (targetYear - 2020)
        # next five-year time step
        NextTimeStep <- targetYear - (targetYear %% 5) + 5
        # linearly extrapolate energy share target to next five-year time step
        x[, targetYear, ] <- x[, targetYear, ] + (5 - (targetYear %% 5)) * ShareChangeRate[, targetYear, ]
        # if there is already a value for the next REMIND time step, use the higher value
        # if not, assign extrapolated value to next REMIND time step
        if (NextTimeStep %in% getYears(x, as.integer = TRUE)) {
          x_target[, NextTimeStep, ] <- base::pmax(x[, targetYear, ], x[, NextTimeStep, ], na.rm = TRUE)
        } else {
          x_target[, NextTimeStep, ] <- x[, targetYear, ]
        }
      }
    }
    return(x_target)
  }

  # function to select REMIND regions with renewable share targets and choose target year per REMIND region
  # only assign renewable share target to REMIND regions with a demand share of countries with targets of at least MinCountryTargetShare
  # choose target year of country with largest demand share across countries with targets
  selectTargetRemindRegion <- function(x, x_intp, xHistTotal, regionmapping, MinCountryTargetShare) {
    # create object for target selection
    targetSelect <- new.magpie(getRegions(x), getYears(x), getNames(x), fill = NA)
    for (RemindRegion in unique(regionmapping$RegionCode)) {
      # get all countries in REMIND region
      countries <- regionmapping %>%
        filter(.data$RegionCode == RemindRegion) %>%
        pull(.data$CountryCode)

      for (targetType in getNames(x)) {

        # get all countries with renewable share targets in REMIND region
        countriesWithTarget <- as.quitte(x) %>%
          filter(!is.na(.data$value),
                 .data$region %in% countries,
                 .data$data == targetType) %>%
          pull(.data$region) %>%
          as.vector()

        # check whether share in total demand of countries with target in REMIND region is larger than MinCountryTargetShare
        # If it is not, do not select share target for this REMIND region (leave targetSelect NA)
        TargetCountryDemandShare <- as.vector(dimSums(xHistTotal[countriesWithTarget,,targetType], dim = 1, na.rm = T) / dimSums(xHistTotal[countries,,targetType], dim = 1, na.rm = T) )

        # if no country has target, set to 0 and discard
        if (length(TargetCountryDemandShare) == 0) {
          TargetCountryDemandShare <- 0
        }

        if (TargetCountryDemandShare < MinCountryTargetShare) {
          print(paste("Share of countries with ", targetType ,"target in", RemindRegion, "is lower than", MinCountryTargetShare, "-> discard target for this REMIND region."))
          next
        }

        # find out country with target with maximum historical total electricity production
        MaxCountry <- as.quitte(xHistTotal) %>%
          filter(.data$region %in% countriesWithTarget,
                 .data$data == targetType) %>%
          mutate( "Max" = max(.data$value)) %>%
          filter( .data$value == .data$Max) %>%
          pull(.data$region) %>%
          as.vector()

        # get target year of this country
        MaxCountryYear <- as.quitte(x) %>%
          filter(.data$region == MaxCountry,
                 .data$data == targetType,
                 !is.na(.data$value)) %>%
          pull(.data$period) %>%
          as.vector()


        # set target year for all countries in REMIND region to target year of country with maximum historic electricity production
        targetSelect[countries,MaxCountryYear,targetType] <- 1
      }
    }

    # select targets by multiplying interpolated share targets with target selector targetSelect
    # (if targetSelect is 1, target is included in target for REMIND region.
    # if targetSelect is NA, it is not included)
    x_select <- x_intp * targetSelect

    return(x_select)
  }

  ### main function ----


  ## 1. get data and select target types to be considered ----

  # get NewClimate energy share targets and conert from percent to share
  x <- readSource("NewClimate", subtype = "RenShareTargets")
  x <- collapseDim(x / 100)

  # calaulate renewable share targets of specific countries from technology-specific targets

  # For Japan, add renewable electricity share targets per technology to total renewable share
  x["JPN", , "SE|Electricity|Renewable"] <- x["JPN", , "SE|Electricity|Solar"] + x["JPN", , "SE|Electricity|Wind"] +
    x["JPN", , "SE|Electricity|Geothermal"] + x["JPN", , "SE|Electricity|Biomass"] +
    x["JPN", , "SE|Electricity|Hydro"]

  # For Thailand, interpret renewable share target on FE electricity consumption as renewable share target on SE electricity generation
  x["THA", , "SE|Electricity|Renewable"] <- x["THA", , "FE|Electricity|Renewable"]

  # filter target types to be implemented
  x <- x[, , c("SE|Electricity|Renewable", "SE|Electricity|Non-Biomass Renewable", "SE|Electricity|Non-Fossil", "FE|Renewable")]
  # take minimum energy share for renewable / non-fossil targets,
  # assume that if there is a range countries will interpret the lower bound of the range as criterion to fulfill the target
  x <- collapseDim(x[, , "Min"])


  ## 2. get historic renewable shares and totals ----

  # historic energy share in 2020
  xHist <- getHistEnergyShare(getNames(x), AvgSeveralYears = T)
  xHistShare <- xHist[[1]]
  xHistTotal <- xHist[[2]]

  ## 3. extrapolate targets to REMIND time steps ----

  # extrapolate targets to REMIND time steps where targets are in years that are not REMIND timesteps
  x <- extrapolateTargetYears(x, xHistShare, seq(2020, 2050, 5))

  ## 4. make assumptions about target development over time for countries with and without target ----

  # interpolate and extrapolate renewable share targets for countries with targets
  # assume that countries with share targets reach their share linearly from 2020 shares to the target year
  # assume that after target year, renewable share targets stay constant
  # assume that countries without target keep their historic share in the future
  x_intp <- x
  x_intp[,"y2020",] <- xHistShare
  x_intp <- as.quitte(x_intp) %>%
              mutate( "variable" = .data$data) %>%
              select(-.data$data) %>%
              interpolate_missing_periods(expand.values = T) %>%
              as.magpie()

  ## 4. select which REMIND regions get targets and determine their respective target year ----

  # get regionmapping to be used for aggregation method
  regionmapping <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")
  # select which REMIND regions get targets and determine their respective target year
  x <- selectTargetRemindRegion(x, x_intp, xHistTotal, regionmapping, 0.2)

  ## 5. format renewable share target data to be used as input data for REMIND

  # rename target types to those used in REMIND techpol realization
  getNames(x) <- c( "RenElec",  "RenFE", "NonBioRenElec", "NonFossilElec")

  # replace NA with 0 for the case that there are not targets in a country
  x[is.na(x)] <- 0


  ## 6. get total demand in target year to be used as aggregation weights ----

  # get aggregation weights for renewable share targets
  # generate new object EnergyProj for projections of total energy in target year
  # that are used as country weights when aggregating to REMIND regions REMIND regions
  TotalsEnergyProj <- new.magpie(getItems(x, dim = 1), getItems(x, dim = 2), getNames(x), fill = NA)
  # get projections of energy use in target years
  TotalsEnergyProj[, , "RenFE"] <- toolCalcEnergyProj(subtype = "FE", subset = "SSP2")
  TotalsEnergyProj[, , "RenElec"] <- toolCalcEnergyProj(subtype = "SE|Electricity", subset = "SSP2")
  # for non-biomass renewable share in electricity and non-fossil share in electricity the total is both SE electricity,
  # the same as for the renewable share in electricity
  TotalsEnergyProj[, , "NonBioRenElec"] <- TotalsEnergyProj[, , "RenElec"]
  TotalsEnergyProj[, , "NonFossilElec"] <- TotalsEnergyProj[, , "RenElec"]


  ## 7. hand-over to madrat for aggregation via weighted sum ----

  return(list(
  # renewable share targets by country as aggregation variable
    x = x,
  # total energy demand in target year by country as weight
    weight = TotalsEnergyProj,
    unit = "share",
    description = glue::glue("Renewable energy share targets for electricity and final energy \\
                                         based on NewClimate policy protocol")
  ))
}
