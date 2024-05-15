#' Output for 2 policy cases
#' @author Aman Malik, Christoph Bertram, Oliver Richters
#' @param sources currently only UNFCCC_NDC
#' @param subtype "Ghgshare2005", "Ghgfactor", "Ghghistshare"

calcEmiTarget <- function(sources, subtype) {

  ### Import historical data (gdp and emi) needed for the calculations

  # Historical emissions for 1990-2015 - co2 (excl LU),ch4,n2o (so far no Fgas historic time series)
  ceds <- calcOutput("Emissions", datasource = "CEDS2REMIND", aggregate = FALSE)
  gwpCH4 <- 28 # "Global Warming Potentials of CH4, AR5 WG1 CH08 Table 8.7"     /28/
  gwpN2O <- 265 # "Global Warming Potentials of N2O, AR5 WG1 CH08 Table 8.7"     /265/
  # calculate GHG total of CO2, CH4 and N2O [unit Mt CO2eq]
  # note: CEDS2021 does not include 'Emi|N2O|Land Use|*' variables and cannot be used.
  ghg <- ceds[, seq(1990, 2015, 1), c("Emi|CO2|Energy and Industrial Processes (Mt CO2/yr)")] +
    +gwpN2O / 1000 * dimSums(ceds[, seq(1990, 2015, 1), c("Emi|N2O|Energy and Industrial Processes (kt N2O/yr)",
                                                  "Emi|N2O|Land Use|Agriculture and Biomass Burning (kt N2O/yr)",
                                                  "Emi|N2O|Land Use|Forest Burning (kt N2O/yr)",
                                                  "Emi|N2O|Land Use|Grassland Burning (kt N2O/yr)",
                                                  "Emi|N2O|Waste (kt N2O/yr)")], dim = 3) +
    +gwpCH4 * dimSums(ceds[, seq(1990, 2015, 1), c("Emi|CH4|Energy and Industrial Processes (Mt CH4/yr)",
                                             "Emi|CH4|Land Use|Agriculture and Biomass Burning (Mt CH4/yr)",
                                             "Emi|CH4|Land Use|Forest Burning (Mt CH4/yr)",
                                             "Emi|CH4|Land Use|Grassland Burning (Mt CH4/yr)",
                                             "Emi|CH4|Waste (Mt CH4/yr)")], dim = 3)
  # create global data for checking plausibility of data
  globGhg <- dimSums(ghg, dim = 1)
  ghg <- toolCountryFill(ghg, fill = 0, verbosity = 2)

  # Future GDP values
  gdp <- calcOutput("GDP", aggregate = FALSE)

  convertNAto0 <- function(x) {
    x[is.na(x)] <- 0
    return(x)
  }

  if (sources == "UNFCCC_NDC") {

    listGhgfactors <- list(
      "2018_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2018_cond"),
      "2018_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2018_uncond"),
      "2021_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2021_cond"),
      "2021_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2021_uncond"),
      "2022_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2022_cond"),
      "2022_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2022_uncond"),
      "2023_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2023_cond"),
      "2023_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2023_uncond")
    )

    listYears   <- lapply(listGhgfactors, getItems, dim = "year") %>% unlist() %>% unique() %>% sort()
    listRegions <- lapply(listGhgfactors, getItems, dim = "iso3c") %>% unlist() %>% unique() %>% sort()

    # expand all magpies to listYears
    expandMagpieYears <- function(x) {
      y <- new.magpie(cells_and_regions = listRegions, years = listYears, names = getNames(x))
      for (year in getItems(x, dim = "year")) {
        y[, year, ] <- x[, year, ]
      }
      return(y)
    }

    lapply(listGhgfactors, expandMagpieYears) %>% mbind() -> ghgfactor

    # create 1/0 dummy for calculation of regional share covered by quantitative target, per TarYear.
    # Note that 0 implies no goal, net zero targets have ghgfactor of 0 but dummy of 1
    dummy1 <- 1 * (!is.na(ghgfactor[, , "gdp_SSP2", drop = TRUE]))

    if (grepl("Ghgfactor", subtype, fixed = TRUE)) { # p45_factor_targetyear.cs3r
      # in order to calculate the share of regional emissions coming from countries with quantitative target
      ghgTarget <- setNames(setYears(ghg[, 2005, ], NULL), NULL) * dummy1[, , ]
      # check share of total emissions by countries with quantitative target
      dimSums(ghgTarget, dim = c(1)) / setYears(setNames(globGhg[, 2005, ], NULL), NULL)
      description <- "Multiplier for target year emissions vs 2005 emissions, as weighted average for all countries with NDC target in each region per target year"
      return(list(x = convertNAto0(ghgfactor), weight = ghgTarget[, , ], unit = "1", description = description,
                  min = -5, max = 4)) # these are not necessary fixed values defined until eternity, but rather plausibility checks

    } else if (grepl("Ghgshare2005", subtype, fixed = TRUE)) { # p45_2005share_target.cs3r
      # calculate growth for GDP weight for GHG emission share
      # assuming constant relative emission intensities across countries of one region
      gdpWeight <- new.magpie(getItems(dummy1, dim = "region"), getItems(dummy1, dim = "year"), getNames(ghgfactor))
      for (t in getItems(dummy1, dim = "year")) {
        gdpWeight[, t, ] <- setYears(ghg[, 2005, ] / gdp[, 2005, ], NULL) * gdp[, t, ]
      }
      description <- "2005 GHG emission share of countries with quantifyable emissions under NDC in particular region per target year"
      return(list(x = 1 * (!is.na(ghgfactor[, , ])), weight = gdpWeight, unit = "1", description = description, min = 0, max = 1))

    } else if (grepl("Ghghistshare", subtype, fixed = TRUE)) {
      # make ghgTarget only represent countries with 2030 data
      dummy2 <- new.magpie(cells_and_regions = listRegions, years = getItems(ghg, dim = "year"), names = names(listGhgfactors))
      dummy2[, , ] <- dummy1[, "y2030", ]
      description <- "GHG emissions share of countries with quantifyable 2030 target in particular region"
      return(list(x = dummy2, weight = ghg, unit = "1", description = description, min = 0, max = 1))

    } else {
      cat("Unknown subtype ", subtype, " for calcEmiTarget with source UNFCCC_NDC. Nothing returned.")
    }
  }
}
