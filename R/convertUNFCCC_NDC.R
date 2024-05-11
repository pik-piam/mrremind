#' Policy targets for NDCs from UNFCCC_NDC
#' @description Converts conditional and unconditional capacity targets into total capacity (GW) in target year
#' the Generation targets are similar to the capacity targets but include the capacity factors,
#' the Emissions targets are the total (except land CO2) emissions in the target year
#' @param x MAgPIE object to be converted
#' @param subtype Capacity_YYYY_cond or Capacity_YYYY_uncond for Capacity Targets, Emissions_YYYY_cond or
#' Emissions_YYYY_uncond for Emissions targets, with YYYY NDC version year
#' @return Magpie object with Total Installed Capacity (GW) targets, target years differ depending upon the database.
#' @author Aman Malik, Christoph Bertram, Oliver Richters
#' @importFrom R.utils isZero

convertUNFCCC_NDC <- function(x, subtype) {                                # nolint: object_name_linter.
  if (grepl("Capacity", subtype, fixed = TRUE)) {
    # for testing: subtype="Capacity_2021_cond"; x <- readSource("UNFCCC_NDC", subtype=subtype, convert=FALSE);

    # add missing magclass columns if they were not in the data provided to avoid index out of bound errors
    targetTypes <- c("AC-Absolute", "Production-Absolute", "TIC-Absolute", "FE-Production-Share")
    if (FALSE %in% (getNames(x[, , ], fulldim = TRUE)$`Type of target` %in% targetTypes)) {
      cat("Table read from UNFCCC_NDC contains unknown target types: ",
      getNames(x[, , ], fulldim = TRUE)$`Type of target`[!(getNames(x[, , ], fulldim = TRUE)$`Type of target` %in% targetTypes)])
    }
    techList <- c("Wind", "Solar", "Hydro", "Nuclear", "Biomass")
    listAllCombinations <- do.call(paste, c(expand.grid(getNames(x[, , ], fulldim = TRUE)$Conditionality, targetTypes, techList), sep = "."))
    missingCombinations <- listAllCombinations[!listAllCombinations %in% getNames(x[, , ])]
    x <- add_columns(x, addnm = missingCombinations, dim = 3, fill = NA)

    if (grepl("uncond", subtype, fixed = TRUE)) {  # unconditional policies
      x <- x[, , "conditional", invert = TRUE, drop = TRUE] # keep only unconditional policies
    } else { # conditional policies
      # loop to make conditional targets at least as good as unconditional targets
      for (r in getItems(x, dim = "ISO")) {
        for (t in getItems(x, dim = "Target Year")) {
          for (tech in getNames(x[, , "conditional", drop = TRUE])) {
            if (is.na(x[r, t, paste0("conditional", ".", tech)])) {
              x[r, t, paste0("conditional", ".", tech)] <- x[r, t, paste0("unconditional", ".", tech)]
            }
          }
        }
      }
      x <- x[, , "unconditional", invert = TRUE, drop = TRUE] # keep only conditional policies
    }

    if ("FE-Production-Share" %in% getNames(x[, , ], fulldim = TRUE)$`Type of target`) {
      cat("FE-Production-Share currently not implemented.")
    }

    x[is.na(x)] <- 0 # Converting all NAs to zero

    # generate target years, at least to 2035, but allow every year in x to be rounded up to next fiver
    targetYears <- seq(2020, max(2035, 4 + max(getYears(x, as.integer = TRUE))), by = 5)

    # generate new object x_mod5 that has values only for targetYears and transfer those from x
    x_mod5 <- new.magpie(getItems(x, dim = "ISO"), targetYears, getNames(x), fill = 0)
    for (i in getYears(x, as.integer = TRUE)) {
      if (i %% 5 == 0) {
        x_mod5[, i, ] <- x[, i, ]
      }
    }

    # for non-fiver years, transfer them to following fiver year, increasing every year by 5 percentage points
    # if 2032 and 2035 data are given for the same country, use the higher value (apply max row-wise).
    for (i in getYears(x, as.integer = TRUE)) {
      if (i %% 5 != 0) {
        x_mod5[, i - (i %% 5) + 5, ] <- apply(matrix(c(as.vector(x_mod5[, i - (i %% 5) + 5, ]), as.vector(x[, i, ]) * (1 + (5 - (i %% 5)) * 0.05)), ncol = 2), 1, max)
      }
    }

    # Creating magpie object which at the end will only contain capacity targets
    x_capacity <- new.magpie(getItems(x_mod5, dim = "region"), targetYears, techList)
    x_capacity[is.na(x_capacity)] <- 0

    # reading historical data
    hist_cap <- readSource("IRENA", subtype = "Capacity") / 1000 # converting from MW to GW
    hist_gen <- readSource("IRENA", subtype = "Generation")      # Units are GWh

    # Real world capacity factor for hydro = Generation in last year/Capacity in last year
    cf_hydro_realworld <- hist_gen[, 2015, "Renewable hydropower"] / (8760 * hist_cap[, 2015, "Renewable hydropower"])
    cf_hydro_realworld[is.na(cf_hydro_realworld) | is.infinite(cf_hydro_realworld)] <- 0
    getNames(cf_hydro_realworld) <- "Hydro"

    # Capacity factors in REMIND. From : calcOutput("Capacityfactor"...)
    cf_biomass <- 0.75
    cf_nuclear <- 0.85
    cf_hydro   <- max(cf_hydro_realworld) + 0 * cf_hydro_realworld
    # using cf_hydro_realworld directly causes converges errors because some are very small. Second term needed such that cf_hydro has right structure

    # Initialising all capacities for all model years to current capacities and converting generation to capacity
    x_capacity[, , c("Wind", "Solar")]  <- setYears(hist_cap[getItems(x_mod5, dim = "region"), 2015, c("Wind", "Solar")])
    x_capacity[, , "Biomass"]           <- setYears(hist_cap[getItems(x_mod5, dim = "region"), 2015, "Bioenergy"])

    # special case for hydro.
    x_capacity[, , "Hydro"]             <- setYears(hist_gen[getItems(x_capacity, dim = "region"), 2015, "Renewable hydropower"])
    # Special case for nuclear
    hist_gen_nuclear <- readSource("BP", subtype = "Generation") * 1000 # TWh to GWh
    for (i in targetYears) {
      for (j in getNames(x_mod5[, , "Nuclear"])) {
        for (k in getItems(x_mod5, dim = "region")) {
          if (x_mod5[k, i, j] != 0)
            x_capacity[k, , "Nuclear"] <- setYears(hist_gen_nuclear[k, 2015, "Generation|Nuclear (TWh)"]) / (8760 * cf_nuclear)
        }
      }
    }
    x_current <- x_capacity # x_current contains current capacities except for hydro where current generation values are taken
    x_capacity_abs <- 0 * x_capacity
    x_capacity_prod_nb <- 0 * x_capacity
    x_capacity_tic <- 0 * x_capacity
    x_capacity_prod_swh <- 0 * x_capacity

    # Converting additional capacity targets to absolute capacity targets
    x_capacity_abs[, , c("Nuclear", "Biomass", "Wind", "Solar")] <- x_current[, , c("Nuclear", "Biomass", "Wind", "Solar")] +
      x_mod5[, , c("AC-Absolute.Nuclear", "AC-Absolute.Biomass", "AC-Absolute.Wind", "AC-Absolute.Solar"), drop = TRUE]
    x_capacity_abs[, , "Hydro"] <- x_current[, , "Hydro"] + x_mod5[, , "AC-Absolute.Hydro", drop = TRUE] * setYears(cf_hydro[getItems(x_mod5, dim = "region"), , ] * 8760)

    # Converting Production targets (GWh) to Capacity targets (TIC-Absolute) (GW) for nuclear and biomass
    # pmax used to always take the higher value from existing capacity and new capacity (from production)
    x_capacity_prod_nb[, , "Nuclear"] <- pmax(x_current[, , "Nuclear"], x_mod5[, , c("Production-Absolute.Nuclear")] / (8760 * cf_nuclear))
    x_capacity_prod_nb[, , "Biomass"] <- pmax(x_current[, , "Biomass"], x_mod5[, , c("Production-Absolute.Biomass")] / (8760 * cf_biomass))

    # Total installed capacity Targets
    # target in target year should be the maximum from the target year and the current capacity
    x_capacity_tic[, , c("Nuclear", "Biomass", "Wind", "Solar")] <- pmax(x_current[, , c("Nuclear", "Biomass", "Wind", "Solar")], x_mod5[, , c("Nuclear", "Biomass", "Wind", "Solar")][, , "TIC-Absolute", drop = TRUE])
    x_capacity_tic[, , "Hydro"] <- pmax(x_current[, , "Hydro"], x_mod5[, , "TIC-Absolute.Hydro", drop = TRUE] * setYears(cf_hydro[getItems(x_mod5, dim = "region"), , ]))

    # Converting Production targets to capacity targets for solar (pv and csp), hydro, and wind
    # Obtaining the capacity factors (nur) values and associated maxproduction (maxprod) for Hydro, Wind, and Solar
    data_wind <- calcOutput("PotentialWindOn", aggregate = FALSE)
    # Reordering dim=3 for data_wind so that 1st position corresponds to maxprod.nur.1 and not maxprod.nur.9
    data_wind_sorted <- mbind(data_wind[, , "1"], data_wind[, , "2"], data_wind[, , "3"], data_wind[, , "4"],
                              data_wind[, , "5"], data_wind[, , "6"], data_wind[, , "7"], data_wind[, , "8"], data_wind[, , "9"])
    data_hydro <- calcOutput("PotentialHydro", aggregate = FALSE)
    data_solar <- calcOutput("Solar", regionmapping = "regionmappingTCD.csv")
    names_solar <- paste0("Solar.", getNames(collapseNames((mselect(data_solar, type = c("nur", "maxprod"), technology = "spv")), collapsedim = 2)))
    names_hydro <- paste0("Hydro.", getNames(data_hydro))
    names_wind <- paste0("Wind.", getNames(data_wind_sorted))
    data_combined <- new.magpie(getItems(data_hydro, dim = "region"), NULL, c(names_solar, names_hydro, names_wind))
    data_combined[, , "Hydro"] <- data_hydro
    data_combined[, , "Wind"] <- data_wind_sorted
    data_combined[c("TCD", "JPN"), , "Solar"][, , "maxprod"]  <- as.vector(data_solar[c("TCD", "JPN"), , "maxprod"][, , "spv"])
    data_combined[c("TCD", "JPN"), , "Solar"][, , "nur"]  <- as.vector(data_solar[c("TCD", "JPN"), , "nur"][, , "spv"])
    data_combined <- data_combined[getItems(x_mod5, dim = "region"), , ]
    for (n in getNames(data_combined, dim = 1)) {
      name <- paste0(n, ".maxprod")
      # Conversion from EJ/a to GWh
      data_combined[, , name, pmatch = TRUE] <- data_combined[, , name, pmatch = TRUE] * 277777.778
    }

    data_combined[is.na(data_combined)] <- 0
    # Production/Generation targets are converted into capacity targets by alloting production to certain capacity factors based on maxprod.
    final <- numeric(length(getItems(x_mod5, dim = "region")))
    names(final) <- getItems(x_mod5, dim = "region")
    tmp_target <- numeric(10)

    # x_mod5[,,"Production-Absolute.Hydro"] <- pmax(x_mod5[,,"Production-Absolute.Hydro"],x_capacity[,,"Hydro"])
    x_mod5[, , "Production-Absolute.Hydro"] <- pmax(x_mod5[, , "Production-Absolute.Hydro"], x_capacity_tic[, , "Hydro"], x_capacity_abs[, , "Hydro"])
    x_mod5[is.na(x_mod5)] <- 0
    # For all countries which have non-zero generation values but zero or negative maxprod(),
    #  replace x_mod5[,,"Production-Absolute.Hydro]==0
    #  Even if there is one +ve production absolute value for Hydro but all maxprod are zero
    for (r in names(final)) {
      if (any(x_mod5[r, , "Production-Absolute.Hydro"] != 0) &
         all(data_combined[r, , "Hydro.maxprod"] == 0) | any(data_combined[r, , "Hydro.maxprod"] < 0))
        x_mod5[r, , "Production-Absolute.Hydro"] <- 0
    }

    for (t in c("Solar", "Wind", "Hydro")) {
      data_sel <- data_combined[, , t]
      data_in_use <-  data_sel[, , "maxprod"] / data_sel[, , "nur"]
      for (y in targetYears) {
        final[] <- 0
        for (r in names(final)) {
          tmp_target <- numeric(10)
          name <- paste0(t, ".maxprod")
          name2 <- paste0("Production-Absolute.", t)
          if (!isZero(x_mod5[, , "Production-Absolute"][, , t])[r, y, ] &
              dimSums(data_combined[r, , name], na.rm = TRUE) > max(x_mod5[r, , name2])) {
            # extracting the first non-zero location of maxprod
            name <- paste0(t, ".maxprod")
            loc <- min(which(!R.utils::isZero(data_combined[r, , name, pmatch = TRUE])))
            tmp_target[1] <- x_mod5[r, y, "Production-Absolute"][, , t]
            if (data_sel[r, , "maxprod"][, , loc] > tmp_target[1]) {
              final[r] <- tmp_target[1] / (8760 * data_sel[r, , "nur"][, , loc])
            } else {
            tmp_target[2] <- tmp_target[1] - data_sel[r, , "maxprod"][, , loc]
            if (data_sel[r, , "maxprod"][, , loc + 1] > tmp_target[2]) {
              final[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + tmp_target[1] / data_sel[r, , "nur"][, , loc + 1])
            } else {
            tmp_target[3] <- tmp_target[2] - data_sel[r, , "maxprod"][, , loc + 1]
            if (data_sel[r, , "maxprod"][, , loc + 2] > tmp_target[3]) {
              final[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + data_in_use[r, , ][, , loc + 1]
                                    + tmp_target[2] / data_sel[r, , "nur"][, , loc + 2])
            } else {
            tmp_target[4] <- tmp_target[3] - data_sel[r, , "maxprod"][, , loc + 2]
            if (data_sel[r, , "maxprod"][, , loc + 3] > tmp_target[4]) {
              final[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + data_in_use[r, , ][, , loc + 1] + data_in_use[r, , ][, , loc + 2] +
                                      tmp_target[3] / data_sel[r, , "nur"][, , loc + 3])
              final[r] <- tmp_target[1]
            } else {
            tmp_target[5] <- tmp_target[4] - data_sel[r, , "maxprod"][, , loc + 3]
            if (data_sel[r, , "maxprod"][loc + 4] > tmp_target[5]) {
              final[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + data_in_use[r, , ][, , loc + 1] + data_in_use[r, , ][, , loc + 2] +
                                     data_in_use[r, , ][, , loc + 3] + tmp_target[4] / data_sel[r, , "nur"][, , loc + 4])
            } else {
            tmp_target[6] <- tmp_target[5] - data_sel[r, , "maxprod"][, , loc + 4]
            if (data_sel[r, , "maxprod"][loc + 5] > tmp_target[6]) {
              final[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + data_in_use[r, , ][, , loc + 1] + data_in_use[r, , ][, , loc + 2] +
                                      data_in_use[r, , ][, , loc + 3] + data_in_use[r, , ][, , loc + 4] +
                                      tmp_target[5] / data_sel[r, , "nur"][, , loc + 5])
            }
            }
            }
            }
            }
            }
          }
        }
        x_capacity_prod_swh[, y, t] <- final
      }
    }

    x_capacity_gen <- mbind(x_capacity_prod_swh[, , c("Solar", "Wind", "Hydro")], x_capacity_prod_nb[, , c("Biomass", "Nuclear")])
    x_capacity[, , c("Solar", "Wind", "Hydro", "Biomass", "Nuclear")] <- pmax(x_capacity_abs[, , c("Solar", "Wind", "Hydro", "Biomass", "Nuclear")],
                                                                   x_capacity_gen[, , c("Solar", "Wind", "Hydro", "Biomass", "Nuclear")],
                                                                   x_capacity_tic[, , c("Solar", "Wind", "Hydro", "Biomass", "Nuclear")])
    # x_capacity[,,c("Solar","Wind")] <- pmax(x_capacity_copy[,,c("Solar","Wind")],x_capacity[,,c("Solar","Wind")])
    x_capacity[, , "Hydro"] <- x_capacity_gen[, , "Hydro"]

    #  # Making sure that targets in subsequent years are always same or greater than the proceeding year
    for (r in getItems(x_mod5, dim = "region")) {
      for (t in techList) {
        for (i in head(targetYears, -1)) {
          if (x_capacity[r, i + 5, t] < setYears(x_capacity[r, i, t])) {
            x_capacity[r, i + 5, t] <- setYears(x_capacity[r, i, t])
          } else {
            x_capacity[r, i, t] <- x_capacity[r, i, t]
          }
        }
      }
    }
    # countries not in the database
    rest_regions <- getItems(hist_cap, dim = "Country/area")[!(getItems(hist_cap, dim = "Country/area") %in% getItems(x_capacity, dim = "region"))]
    x_other <- new.magpie(rest_regions, targetYears, techList)
    x_other[, , c("Wind", "Solar")]  <- setYears(hist_cap[rest_regions, 2015, c("Solar", "Wind")])
    x_other[, , "Nuclear"] <- 0
    x_other[, , "Biomass"] <- setYears(hist_cap[rest_regions, 2015, "Bioenergy"])
    x_other[, , "Hydro"] <- setYears(hist_cap[rest_regions, 2015, "Renewable hydropower"]) * setYears(cf_hydro[rest_regions, , ])

    x_final <- magpiesort(mbind(x_capacity, x_other))
    x_final[is.na(x_final)] <- 0
    x <- toolCountryFill(x_final, fill = NA, verbosity = 2) # will be returned
    getNames(x) <- c("wind", "spv", "hydro", "tnrs", "bioigcc")
    # end subtype contains Capacity


  } else if (grepl("Emissions", subtype, fixed = TRUE)) { # calculate emissions in target year relative to 2005 emissions

    reductionData <- x
    ### Import historical data (gdp and emi) needed for the calculations

    # Historical emissions for 1990-2015 - co2 (excl LU),ch4,n2o (so far no Fgas historic time series)
    ceds <- calcOutput("Emissions", datasource = "CEDS2REMIND", aggregate = FALSE)
    gwpCH4 <- 28  # "Global Warming Potentials of CH4, AR5 WG1 CH08 Table 8.7"     /28/
    gwpN2O <- 265 # "Global Warming Potentials of N2O, AR5 WG1 CH08 Table 8.7"     /265/
    # calculate GHG total of CO2, CH4 and N2O [unit Mt CO2eq]
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

    # Future GDP values
    gdp <- calcOutput("GDP", aggregate = FALSE)

    # Define EU countries + croatia for special treatment because of joint NDC
    EUR_NDC_countries <- c("POL", "CZE", "ROU", "BGR", "HUN", "SVK", "LTU", "EST", "SVN",
                           "LVA", "DEU", "FRA", "ITA", "ESP", "NLD", "BEL", "GRC", "AUT",
                           "PRT", "FIN", "SWE", "IRL", "DNK", "LUX", "CYP", "MLT", "JEY",
                           "FRO", "GIB", "GGY", "IMN", "HRV", # included: HRV/croatia
                           if (grepl("_20(18|19|20|21)_", subtype)) "GBR") # own NDC in 2022


    # NDC Types, order must be exactly the same as in readUNFCCC_NDC.R!
    allowedType <- c("GHG-Absolute", "GHG", "GHG/GDP", "CO2/GDP", "GHG-fixed-total", "GHG/CAP")
    # define function that calculates ghgfactor compared to 2005 for each regi and year, taken into account different Types
    calcGhgfactor <- function(data) {
      if (nregions(data) != 1 || nyears(data) != 1) {
        warning("function calcGhgfactor(data) should be called with one single region and year as data only")
      }
      regi <- getItems(data, dim = "ISO_Code")
      year <- getYears(data)
      ghgtarget <- NA
      if (allowedType[data[regi, year, "Type"]] == "GHG-Absolute") {
        if (data[regi, year, "Reference_Year"] == -1) {  # -1 if BAU;
          if (!is.na(data[regi, year, "BAU_or_Reference_emissions_in_MtCO2e"])) { # ignore if no BAU emissions given
            ghgtarget <- data[regi, year, "BAU_or_Reference_emissions_in_MtCO2e"] + data[regi, year, uncond_or_cond]
          }
        } else { # use historic ghg data for Reference_Year, not those given in the database
          ghgtarget <- setYears(ghg[regi, min(data[regi, year, "Reference_Year"], max(getYears(ghg, as.integer = TRUE))), ], NULL) + data[regi, year, uncond_or_cond]
        }
      } else if (allowedType[data[regi, year, "Type"]] == "GHG") { # relative change of GHG
        if (data[regi, year, "Reference_Year"] == -1) {  # -1 if BAU.
          if (!is.na(data[regi, year, "BAU_or_Reference_emissions_in_MtCO2e"])) {  # ignore if no BAU emissions given
            ghgtarget <- data[regi, year, "BAU_or_Reference_emissions_in_MtCO2e"] * (1 + data[regi, year, uncond_or_cond])
          }
        } else { # then Reference_Year contains a year
          ghgtarget <- setYears(ghg[regi, min(data[regi, year, "Reference_Year"], max(getYears(ghg, as.integer = TRUE))), ], NULL) * (1 + data[regi, year, uncond_or_cond])
        }
      } else if (allowedType[data[regi, year, "Type"]] %in% c("GHG/GDP", "CO2/GDP")) { # OR: Why no distinction?
        ghgtarget <- (1 + data[regi, year, uncond_or_cond]) * gdp[regi, year, ] / setYears(gdp[regi, round(as.numeric(data[regi, year, "Reference_Year"]) / 5) * 5, ], NULL) * setYears(ghg[regi, as.numeric(data[regi, year, "Reference_Year"]), ], NULL)
      } else if (allowedType[data[regi, year, "Type"]] == "GHG-fixed-total") {
        ghgtarget <- data[regi, year, uncond_or_cond]
      } else {
        warning("Unknown Type for regi ", regi, " and year ", year, ": ", data[regi, year, "Type"], " / ", allowedType[data[regi, year, "Type"]], " (note: GHG/CAP currently not implemented)")
      }
      # return ghgtarget
      return(ghgtarget)
    } # end of function calc_ghgfactor

    # countries with known NDC/2005 ratios bigger than 2.5 or less than 0
    knownhigh <- list("IND" = c("y2030"))
    knownlow <- list("GAB" = c("y2050"))

    # ghgfactor compared to 2005, first set to NA
    # this copy of the gdp structure is needed because of the different SSP
    ghgfactor <- gdp[unique(c(getItems(reductionData, dim = "ISO_Code")[getItems(reductionData, dim = "ISO_Code") != "EUR"], EUR_NDC_countries)), getYears(reductionData), ]
    ghgfactor[, , ] <- NA
    # define string that can be used to assess magpie variables
    uncond_or_cond <- ifelse (length(grep("uncond", subtype)) == 0, "Conditional", "Unconditional")
    # loop through regions and years
    for (regi in getItems(reductionData, dim = "ISO_Code")) {
      for (year in getYears(reductionData)) {
        if (!is.na(reductionData[regi, year, uncond_or_cond])[1]) {  # check whether a target exists
          if (regi != "EUR") { # call function calc_ghgfactor and divide by 2005 ghg data to get the ghgfactor
            # move it to a year REMIND understands
            newyear <- as.numeric(gsub("y", "", year))
            newyear <- if (newyear < 2060) ceiling((newyear-1)/5)*5 else ceiling((newyear-2)/10)*10
            ghgfactor[regi, newyear, ] <- calcGhgfactor(reductionData[regi, year, ]) / setYears(ghg[regi, 2005, ], NULL)
            ghgfactormax <- max(c(0, as.numeric(ghgfactor[regi, newyear, ])), na.rm = TRUE)
            if (isTRUE(ghgfactormax > 2.5) && ! year %in% knownhigh[[regi]]) {
              message("For ", regi, " in ", year, ", ghgfactor=", ghgfactormax, " is above 2.5 and will be dropped.")
            }
            ghgfactormin <- min(c(0, as.numeric(ghgfactor[regi, newyear, ])), na.rm = TRUE)
            if (isTRUE(ghgfactormin < 0) && ! year %in% knownlow[[regi]]) {
              warning("For ", regi, " in ", year, ", ghgfactor=", ghgfactormin, " is below 0. ",
                      "Is the country really promising negative net emissions? Add it to 'knownlow' in calcEmiTarget.")
            }
          }
        }
      }
    }

    for (year in getYears(reductionData)) { # assign EUR NDC goal to all EUR28 countries
      if (!is.na(reductionData["EUR", year, uncond_or_cond])[1]) {  # check whether a target exists
        if (allowedType[reductionData["EUR", year, "Type"]] == "GHG") {
             ghggoal <- (1 + reductionData["EUR", year, uncond_or_cond]) * setYears(ghg[EUR_NDC_countries, min(reductionData["EUR", year, "Reference_Year"], max(getYears(ghg, as.integer = TRUE))), ], NULL)
             ghgfactor[EUR_NDC_countries, year, ] <- ghggoal / setYears(ghg[EUR_NDC_countries, 2005, ], NULL)
        } else if (allowedType[reductionData["EUR", year, "Type"]] == "GHG-fixed-total") {
             ghg_EUR_2005 <- sum(setYears(ghg[EUR_NDC_countries, 2005, ], NULL))
             ghgfactor[EUR_NDC_countries, year, ] <- reductionData["EUR", year, uncond_or_cond] / ghg_EUR_2005
        } else {
            warning("Calculation assumes EU target is GHG or GHG-fixed-total, but database says ",
                    reductionData["EUR", year, "Type"], ". Please check or exjust calcEmiTarget!")
        }
      }
    }

    # exclude country targets with factor higher than 2.5, which is about the highest region average BAU growth rate (but always use China and India)
    for (regi in getItems(ghgfactor, dim = "iso3c")) {
      if (!regi %in% c("IND", "CHN")) {
        ghgfactor[regi, , ][ghgfactor[regi, , ] > 2.5] <- NA
      }
    }
    x <- toolCountryFill(ghgfactor, fill = NA, verbosity = 2)

  } # end subtype = Emissions_all

  # add NDC_version from subtype to allow for distinction of NDC version in calcEmiTarget/calcCapTarget
  NDC_version <- paste(unlist(strsplit(subtype, "_"))[-1], collapse = "_")
  getNames(x) <- paste(NDC_version, getNames(x), sep = ".")
  return(x)
}
