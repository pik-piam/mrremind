#' Calculate Energy Share Targets
#'
#' @description This function calculates the energy share targets used for NPi runs from the climate policy modeling protocol provided by NewClimate. It selects
#' the following energy share targets from the NewClimate data that we implement: 1) share of renewables in total final energy (FE|Renewable),
#' 2) share of renewables in electricity generation (SE|Electricity|Renewable) and 3) share of non-fossil energy in total primary energy (PE|Non-fossil). It aggregates
#' technology-specific energy share targets to one of these three categories (e.g. solar shares targets and wind share targets to a total renewable share target). Next,
#' energy share targets for years which are not REMIND timesteps are extrapolated to REMIND time steps. Finally, energy share targets are aggregated from country-level
#' to REMIND region-level using downscaled projections of FE demand from the EDGE models for the target years as country weights.
#'
#' @param sources "NewClimate"
#' @author Felix Schreyer
#'

calcEnergyShareTargets <- function(sources) {
  ### define functions ----

  # function to get historiocal 2020 energy shares for the respective target types
  # (e.g. renewable electricity share in 2020 per-country for target type "SE|Electricity|Renewable")
  getHistEnergyShare <- function(ShareTypes, AvgSeveralYears = F) {
    # get historical FE and SE data from IEA energy balances
    IEA <- calcOutput("FE", source = "IEA", ieaVersion = "default", aggregate = F)

    # create array for historical energy shares
    xHist <- new.magpie(getItems(IEA, dim = 1), years = NULL, names = ShareTypes, fill = NA)

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
        AvgYears <- c("2018", "y2019", "y2020", "2021", "2022")
        xHistShareVar <- collapseDim(dimSums(xHistShareVar[, AvgYears, ], dim = 2) / length(AvgYears))
        xHistTotalVar <- collapseDim(dimSums(xHistTotalVar[, AvgYears, ], dim = 2) / length(AvgYears))
      } else {
        # only get 2020 year
        xHistShareVar <- collapseDim(xHistShareVar[, "y2020", ])
        xHistTotalVar <- collapseDim(xHistTotalVar[, "y2020", ])
      }

      # calculate renewable / non-fossil share in historical year by dividing share variable by total variable
      xHist[, , targetType] <- xHistShareVar / xHistTotalVar
    }

    return(xHist)
  }

  targetYears <- seq(2020, 2050, by = 5)

  # function extrapolate target years which are not REMIND timesteps to REMIND timesteps
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

  ### main function ----

  # get NewClimate energy share targets
  x <- readSource("NewClimate", subtype = "EnergyShareTargets")
  x <- collapseDim(x)



  # for now, select only three categories of energy share targets
  # 1) share of renewables in total final energy (FE|Renewable)
  # 2) share of renewables in electricity generation (SE|Electricity|Renewable)
  # 3) share of non-fossil energy in total primary energy (PE|Non-fossil)

  # assign some of the more detailed or similar targets to one of those 3 categories

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


  # historic energy share in 2020
  xHist <- getHistEnergyShare(getNames(x), AvgSeveralYears = F)

  # extrapolate targets to REMIND time steps where targets are in years that are not REMIND timesteps
  x_target <- extrapolateTargetYears(x, xHist, seq(2020, 2050, 5))




  # get aggregation weights for energy share targets
  # generate new object EnergyProj for projections of total energy in target year that are used as aggregation weights to REMIND regions
  TotalsEnergyProj <- new.magpie(getItems(x_target, dim = 1), targetYears, getNames(x_target), fill = NA)
  # get projections of energy use in target years
  TotalsEnergyProj[, , "FE|Renewable"] <- toolCalcEnergyProj(subtype = "FE", subset = "SSP2")
  TotalsEnergyProj[, , "SE|Electricity|Renewable"] <- toolCalcEnergyProj(subtype = "SE|Electricity", subset = "SSP2")
  # for non-biomass renewable share in electricity and non-fossil share in eletricity the total is both SE electricity,
  # the same as for the renewable share in electricity
  TotalsEnergyProj[, , "SE|Electricity|Non-Biomass Renewable"] <- TotalsEnergyProj[, , "SE|Electricity|Renewable"]
  TotalsEnergyProj[, , "SE|Electricity|Non-Fossil"] <- TotalsEnergyProj[, , "SE|Electricity|Renewable"]

  # for countries without target, assume that they have to at least keep their historic 2020 share at all times
  x_target[is.na(x_target)] <- xHist

  return(list(
    x = x_target,
    weight = TotalsEnergyProj,
    unit = "percent",
    description = glue::glue("Energy share targets for renewable share in final energy, renewable share in electricity \\
                                         based on NewClimate policy protocol")
  ))
}
