#' Calculate energy projections on country-level based on EDGE models outputs per country.
#' These energy projections are used in the input data preparation for aggregating country-specific data to REMIND regions.
#' They are a country-level proxy of the final energy demand trajectories on the level of REMIND regions provided by the EDGE models.
#'
#' @author Felix Schreyer
#' @param subtype "FE" (Total final energy consumption), "SE|Electricity" (SE electricity generation)
#' @param subset GDP scenario to use
#' @param scenario set of GDP scenarios to use for calcFEDemand calculation (trigger standard cache in this function)
#' @seealso [convertNewClimate()]

toolCalcEnergyProj <- function(subtype, subset, scenario, years = seq(2020, 2050, 5)) {
  # This function projects FE demand and SE electricity generation on country-level to future years based on a FE and GDP trend approach:
  #
  # Energy(targetYear) = Energy(2020) * [ FE(REMINDregion,targetYear) / FE(REMINDregion,2020)
  #                                         + ( GDP(country,targetYear) / GDP(country,2020) - GDP(REMINDregion,targetYear) / GDP(REMINDregion,2020) ) ]
  #
  # if subtype = "FE" -> Energy is total final energy, FE is total final energy consumption
  # if subtype = "SE|Electricity" -> Energy is total secondary energy electricity generation, FE is FE electricity consumption
  # This means that, for example, future country-level FE demand is projected by multiplying historical FE demand with the sum of
  # a) the FE demand trend from the REMIND region of this country and
  # b) the difference in GDP between the country and the REMIND region.

  # The projection applies four steps:
  # 1. get historical energy data for 2020
  # 2. calculate trend of FE in REMIND region based on EDGE model projections
  # 3. calculate difference of GDP trends between country and REMIND region based on SSP GDP projections
  # 4. project energy in the future with the above formulate using the terms calculated in 1. to 3.

  ### define functions ----

  # function get historical energy data for 2020
  .getHistData <- function(subtype, AvgSeveralYears) {
    # get historical FE and SE data from IEA energy balances
    IEA <- calcOutput("FE", source = "IEA", ieaVersion = "default", aggregate = F)

    # add items to calculate historical FE demand or historical SE generation
    if (subtype == "FE") {
      xHist <- collapseDim(IEA[, , "FE|Buildings (EJ/yr)"] + IEA[, , "FE|Industry (EJ/yr)"] + IEA[, , "FE|Transport (EJ/yr)"], dim = 3)
    } else if (subtype == "SE|Electricity") {
      xHist <- collapseDim(IEA[, , "SE|Electricity|Biomass (EJ/yr)"] + IEA[, , "SE|Electricity|Solar (EJ/yr)"] + IEA[, , "SE|Electricity|Wind (EJ/yr)"] +
        IEA[, , "SE|Electricity|Hydro (EJ/yr)"] + IEA[, , "SE|Electricity|Geothermal (EJ/yr)"] + IEA[, , "SE|Electricity|Coal (EJ/yr)"] + IEA[, , "SE|Electricity|Gas (EJ/yr)"] + IEA[, , "SE|Electricity|Oil (EJ/yr)"] +
        IEA[, , "SE|Electricity|Nuclear (EJ/yr)"], dim = 3)
    }

    if (AvgSeveralYears) {
      AvgYears <- c("y2018", "y2019", "y2020", "y2021", "y2022")
      xHist <- collapseDim(dimSums(xHist[, AvgYears, ], dim = 2) / length(AvgYears))
    } else {
      # only get 2020 year
      xHist <- collapseDim(xHist[, "y2020", ])
    }

    return(xHist)
  }

  # function to calculate FE trend on level of REMIND regions based on EDGE data
  .computeFeTrend <- function(subtype, subset, years) {
    # get FE demand projections per REMIND region for transport, buildings and industry for SSP2
    # transport FE demand from EDGE-T (in TWa/yr)
    FETransport <- calcOutput("EDGETransport", subtype = "f35_demByTech")
    # filter for SSP2 scenario and Mix1 EDGE scenario
    FETransport <- collapseNames(mselect(FETransport, GDP_scenario = subset, DEM_scenario = subset, EDGE_scenario = "Mix1"))
    # buildings and industry FE demand from EDGE-B and EDGE-I (in EJ/yr)
    FEBuildIndustry <- calcOutput("FEdemand", scenario = scenario , signif = 4)
    # filter for SSP2 scenario
    FEBuildIndustry <- collapseNames(FEBuildIndustry[, , subset])
    # define CES inputs over which to sum for respective FE demand
    if (subtype == "FE") {
      # for FE projection, get FE trend of total FE
      input.fe.build <- c("feelcb", "feelhpb", "feelrhb", "fegab", "fehob", "fesob", "feheb")
      input.fe.indst <- c(
        grep("fe.*steel", getNames(FEBuildIndustry), value = T),
        grep("fe.*cement", getNames(FEBuildIndustry), value = T),
        grep("fe.*chem", getNames(FEBuildIndustry), value = T),
        grep("fe.*otherInd", getNames(FEBuildIndustry), value = T)
      )
      input.fe.trans <- getNames(FETransport)
    } else if (subtype == "SE|Electricity") {
      # for SE electricity projection, get FE trend of FE electricity
      input.fe.build <- c("feelcb", "feelhpb", "feelrhb")
      input.fe.indst <- c(
        "feel_steel_secondary", "feel_steel_primary", "feel_cement",
        "feelwlth_chemicals", "feelhth_chemicals",
        "feelwlth_otherInd", "feelhth_otherInd"
      )
      input.fe.trans <- c("feelt")
    }

    # common years across EDGE outputs
    common.years <- intersect(getYears(FEBuildIndustry), getYears(FETransport))

    # sum FE demand across sectors per REMIND region
    FeTotal <- dimSums(FEBuildIndustry[, common.years, input.fe.build], dim = 3) +
      dimSums(FEBuildIndustry[, common.years, input.fe.indst], dim = 3) +
      dimSums(FETransport[, common.years, input.fe.trans], dim = 3) * 31.536 # convert TWa/yr to EJ/yr
    # calculate FE trend of target year with respect to 2020
    FeTrend <- collapseDim(FeTotal[, years, ] / FeTotal[, "y2020", ])
    # get regionmapping
    mapping <- toolGetMapping(getConfig("regionmapping"), type = "regional", where = "mappingfolder")
    # assign REMIND FE region trend to all countries within this region
    FeTrend <- toolAggregate(FeTrend, rel = mapping)

    return(FeTrend)
  }

  # function to calculate difference in GDP trend between country and REMIND region
  .computeGDPTrendDiff <- function(subset, years, MaxThreshold) {
    # GDP on country-level
    GDP_country <- calcOutput("GDP", scenario = subset, years = years, aggregate = F)
    # GDP on REMIND region-level
    GDP_region <- calcOutput("GDP", scenario = subset, years = years)
    # get regionmapping
    mapping <- toolGetMapping(getConfig("regionmapping"), type = "regional", where = "mappingfolder")
    # assign REMIND GDP trend to all countries within this region to be able to subtract region GDP trend from country GDP trend
    GDP_region <- toolAggregate(GDP_region, rel = mapping)
    # calculate difference between country GDP trend and region GDP trend of target year with respect to 2020
    GDPTrendDiff <- collapseDim(GDP_country[, years, ] / GDP_country[, "y2020", ] - GDP_region[, years, ] / GDP_region[, "y2020", ])
    # check whether GDP trend difference is within MaxThreshold,
    # if GDP growth in country is too different from GDP growth of region,
    # very strong energy projection trends (negative energy projected) could be produced
    GDPTrendDiff[GDPTrendDiff < -MaxThreshold] <- -MaxThreshold
    GDPTrendDiff[GDPTrendDiff > MaxThreshold] <- MaxThreshold


    return(GDPTrendDiff)
  }

  ### main function ----

  # 1. get historical energy data for 2020 ----
  xHist <- .getHistData(subtype, AvgSeveralYears = T)
  # 2. calculate FE trend on level of REMIND regions based on EDGE data ----
  FeTrend <- .computeFeTrend(subtype, subset, years)
  # 3. calculate difference in GDP trend between country and REMIND region ----
  GDPTrendDiff <- .computeGDPTrendDiff(subset, years, MaxThreshold = 0.3)

  # 4. project energy according to above formula ----
  x <- xHist * (FeTrend + GDPTrendDiff)


  return(x)
}
