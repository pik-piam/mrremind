#' Policy targets for REN21
#' @description This code aggregates and homogenises different types of
#' renewable energy targets into total installed capacity targets (in GW).
#' @details Policy database accessible in "inputdata/sources/REN21/README"
#' @param x MAgPIE object to be converted
#' @param subtype Only "Capacity" as of now
#' @return Magpie object with Total Installed Capacity targets. The target years
#'         differ depending upon the database.
#' @author Aman Malik
#' @importFrom utils read.csv
#'
convertREN21 <- function(x, subtype) {
  if (subtype == "Capacity") {
    x <- magpiesort(x) # sorting years chronologically and region names alphabetically
    x[is.na(x)] <- 0 # Converting all NAs to zero
    getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1)) # Country names to ISO3 code

    # reading historical data
    hist_cap <- readSource(type = "IRENA", subtype = "Capacity") / 1000 # converting from MW to GW
    hist_gen <- readSource("IRENA", subtype = "Generation") # Units are GWh

    # Real world capacity factor for hydro = Generation in last year/Capacity in 2015
    # Note: "Hydropower" contains renewable hydropower and mixed hydro plants, but not pure pumped storage
    cf_realworld <- hist_gen[, 2015, "Hydropower"] / (8760 * hist_cap[, 2015, "Hydropower"])
    cf_realworld[is.na(cf_realworld)] <- 0
    getNames(cf_realworld) <- "Hydro"

    # renaming variable names from IRENA historical database
    getNames(hist_cap)[c(2, 7, 8, 10, 11, 12)] <- c(
      "Hydro", "Wind_ON", "Wind_OFF", "SolarPV",
      "SolarCSP", "Biomass"
    )

    x_tmp <- x # contains original targets + targets extrapolated to model years

    # Combining wind (on and offshore) as no distinction made in REMIND
    x_tmp <- add_columns(x_tmp, addnm = "Wind", dim = 3.2)
    x_tmp[, , "Wind"] <- x_tmp[, , "Wind_ON"] + x_tmp[, , "Wind_OFF"]

    # Selecting relevant technologies i.e., all except "Base Year"
    techs <- c("SolarPV", "SolarCSP", "Wind", "Hydro", "Biomass", "Geothermal")

    # 1. If targets are given in non-model year. e.g., 2022, 2032, then targets are extrapolated linearly.
    input_yr <- getYears(x, as.integer = TRUE)
    input_yr <- input_yr[input_yr %% 5 != 0] # years not multiple of 5/non-model years
    # Selecting relevant model years
    target_years <- c(2020, 2025, 2030, 2035, 2040)
    regions <- getItems(x_tmp, dim = 1)
    tech_n <- getNames(x_tmp[, , techs])
    for (r in regions) {
      for (t in tech_n) {
        for (i in input_yr) {
          if (x_tmp[r, i, t] != 0) {
            if (i > 2015 && i < 2020) {
              x_tmp[r, 2020, tech_n] <- setYears(x_tmp[r, i, tech_n]) * (1 + (2020 - i) * 0.05)
            }
            if (i > 2020 && i < 2025) {
              x_tmp[r, 2025, tech_n] <- setYears(x_tmp[r, i, tech_n]) * (1 + (2025 - i) * 0.05)
            }
            if (i > 2025 && i < 2030) {
              x_tmp[r, 2030, tech_n] <- setYears(x_tmp[r, i, tech_n]) * (1 + (2030 - i) * 0.05)
            }
            if (i > 2030 && i < 2035) {
              x_tmp[r, 2035, tech_n] <- setYears(x_tmp[r, i, tech_n]) * (1 + (2035 - i) * 0.05)
            }
            if (i > 2035 && i < 2040) {
              x_tmp[r, 2040, tech_n] <- setYears(x_tmp[r, i, tech_n]) * (1 + (2040 - i) * 0.05)
            }
            # else
          }
        }
      }
    }

    # for now# x_tmp <- x_tmp[,target_years,] #  only take model years

    # Creating new magpie object containing historical or base year targets
    x_cap <- new.magpie(getItems(x_tmp, dim = 1), target_years, techs)
    x_cap[is.na(x_cap)] <- 0 # replacing NAs with zero
    # Capacity factors in REMIND. From : calcOutput("Capacityfactor"...)
    cf_biomass <- 0.75
    cf_geothermal <- 0.8
    cf_hydro <- cf_realworld

    # Initialising all capacities for all model years to historical 2015 capacities.
    # except when Base year is mentioned (then initialise with base year capacities)
    # and except for Hydro which will get historical generation.

    year <- getYears(x_tmp)
    for (t in year) {
      for (r in getItems(x_tmp, dim = 1)) {
        if (x_tmp[r, t, "AC-Absolute.Base year"] != 0) {
          x_cap[r, , ] <- setYears(hist_cap[r, x_tmp[r, t, "AC-Absolute.Base year"], techs])
        } else {
          x_cap[r, , ] <- setYears(hist_cap[r, 2015, techs])
        }
      }
    }

    # for hydro overwrite capacity  with generation
    x_cap[, , "Hydro"] <- setYears(hist_gen[getItems(x_cap, dim = 1), 2015, "Hydropower"])

    x_cap_ac <- x_cap # object will contain additional capacity targets as absolute capacity targets
    x_cap_ac[] <- 0
    x_cap_pt <- x_cap # object will contain generation targets as absolute capacity targets
    x_cap_pt[] <- 0
    x_cap_tic <- x_cap # object will contain total installed capacity targets
    x_cap_tic[] <- 0

    # Converting additional capacity targets to absolute capacity targets.

    x_cap_ac[, , c("Biomass", "Wind", "SolarPV", "SolarCSP")] <- x_cap[, , c("Biomass", "Wind", "SolarPV", "SolarCSP")] +
      x_tmp[, target_years, c("AC-Absolute.Biomass", "AC-Absolute.Wind", "AC-Absolute.SolarPV", "AC-Absolute.SolarCSP"), drop = TRUE]

    # For hydro converting to additional generation targets
    x_cap_ac[, , "Hydro"] <- x_cap[, , "Hydro"] + x_tmp[, target_years, "AC-Absolute.Hydro", drop = TRUE] * setYears(cf_hydro[getItems(x_tmp, dim = 1), , ] * 8760)

    # Total installed capacity Targets for all technologies

    x_cap_tic[, , c("Wind", "SolarPV", "SolarCSP", "Biomass")] <- pmax(x_cap[, , c("Wind", "SolarPV", "SolarCSP", "Biomass")], x_tmp[, target_years, c("Wind", "SolarPV", "SolarCSP", "Biomass")][, , "TIC-Absolute", drop = TRUE])
    x_cap_tic[, , "Hydro"] <- pmax(x_cap[, , "Hydro"], x_tmp[, target_years, "TIC-Absolute.Hydro", drop = TRUE] * setYears(cf_hydro[getItems(x_tmp, dim = 1), , ] * 8760))

    # Converting Production targets (GWh) to Capacity targets (TIC-Absolute) (GW) for geothermal and biomass
    # pmax takes the higher value from existing capacity and new capacity derived (from production)
    x_cap_pt[, , "Biomass"] <- pmax(x_cap[, , "Biomass"], x_tmp[, target_years, "Production-Absolute.Biomass"] / (8760 * cf_biomass))

    x_cap_pt[, , "Geothermal"] <- pmax(x_cap[, , "Geothermal"], x_tmp[, target_years, c("Production-Absolute.Geothermal")] / (8760 * cf_geothermal))
    # for solar, wind, and hydro conversion will be done using another method below
    x_cap_pt[, target_years, c("SolarPV", "SolarCSP", "Wind")] <- x_tmp[, target_years, c("SolarPV", "SolarCSP", "Wind")][, , "Production-Absolute", drop = TRUE]


    # Converting Production targets to capacity targets for solar and wind
    # For Hydro, all targets (which have already been converted into generation targets) will be
    # converted into capacity targets based on maxprod and capacity factors.

    # Special case for hydro as mentioned above
    x_cap_pt[, target_years, "Hydro"] <- pmax(x_tmp[, target_years, "Production-Absolute.Hydro"], x_cap_tic[, , "Hydro"], x_cap_ac[, , "Hydro"])

    # Obtaining the capacity factors (nur) values and associated maxproduction (maxprod) for Hydro, Wind, and Solar

    data_wind <- calcOutput("PotentialWindOn", aggregate = FALSE)
    # Reordering dim=3 for data_wind so that 1st position corresponds to maxprod.nur.1 and not maxprod.nur.9
    data_wind_sorted <- mbind(
      data_wind[, , "1"], data_wind[, , "2"], data_wind[, , "3"], data_wind[, , "4"],
      data_wind[, , "5"], data_wind[, , "6"], data_wind[, , "7"], data_wind[, , "8"], data_wind[, , "9"]
    )
    data_hydro <- calcOutput("PotentialHydro", aggregate = FALSE)
    data_solar <- calcOutput("Solar")
    names_solarPV <- paste0("SolarPV.", getNames(collapseNames((mselect(data_solar, type = c("nur", "maxprod"), technology = "spv")), collapsedim = 2)))
    names_solarCSP <- paste0("SolarCSP.", getNames(collapseNames((mselect(data_solar, type = c("nur", "maxprod"), technology = "csp")), collapsedim = 2)))
    names_hydro <- paste0("Hydro.", getNames(data_hydro))
    names_wind <- paste0("Wind.", getNames(data_wind_sorted))
    data_combined <- new.magpie(getItems(data_hydro, dim = 1), NULL, c(names_solarPV, names_solarCSP, names_hydro, names_wind))
    data_combined[, , "Hydro"] <- data_hydro
    data_combined[, , "Wind"] <- data_wind_sorted

    data_combined[c("MKD"), , c("SolarPV", "SolarCSP")][, , c("maxprod", "nur")] <- as.vector(data_solar[c("JPN"), , c("spv", "csp")][, , c("maxprod", "nur")])
    data_combined[c("KOR"), , c("SolarPV", "SolarCSP")][, , c("maxprod", "nur")] <- as.vector(data_solar[c("JPN"), , c("spv", "csp")][, , c("maxprod", "nur")])

    data_combined <- data_combined[getItems(x_tmp, dim = 1), , ] # only interested in limited dataset

    for (n in getNames(data_combined, dim = 1)) {
      name <- paste0(n, ".maxprod")
      # Conversion from EJ/a to GWh
      data_combined[, , name, pmatch = TRUE] <- data_combined[, , name, pmatch = TRUE] * 277777.778
    }

    final <- numeric(nregions(x_tmp))
    names(final) <- getItems(x_tmp, dim = 1)
    tmp_target <- numeric(10)
    x_cap_pt_2_tic <- x_cap
    x_cap_pt_2_tic[] <- 0
    # For all countries which have non-zero generation values but zero or negative
    # maxprod() for hydro (i.e., for some read), replace x_tmp[,,"Production-Absolute.Hydro]==0,
    # Even if there is one +ve production absolute value for Hydro but all maxprod are zero

    for (t in c("SolarPV", "SolarCSP", "Wind", "Hydro")) {
      data_sel <- data_combined[, , t]
      data_in_use <- data_sel[, , "maxprod"] / data_sel[, , "nur"]
      for (y in target_years) {
        final[] <- 0
        for (r in names(final)) {
          # Only start loop if Production targets are non-zero and if the maxprod for that country can absorb the targets set.
          name <- paste0(t, ".maxprod")
          if (!R.utils::isZero(x_cap_pt[r, y, t] &
            dimSums(data_combined[r, , name]) > max(x_cap_pt[r, , t]))) { # extracting the first non-zero location of maxprod
            loc <- min(which(!R.utils::isZero(data_combined[r, , name, pmatch = TRUE])))
            tmp_target[1] <- x_cap_pt[r, y, t]
            if (data_sel[r, , "maxprod"][, , loc] > tmp_target[1]) {
              final[r] <- tmp_target[1] / (8760 * data_sel[r, , "nur"][, , loc])
            } else {
              tmp_target[2] <- tmp_target[1] - data_sel[r, , "maxprod"][, , loc]
              if (data_sel[r, , "maxprod"][, , loc + 1] > tmp_target[2]) {
                final[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + tmp_target[2] / data_sel[r, , "nur"][, , loc + 1])
              } else {
                tmp_target[3] <- tmp_target[2] - data_sel[r, , "maxprod"][loc + 1]
                if (data_sel[r, , "maxprod"][, , loc + 2] > tmp_target[3]) {
                  final[r] <- (1 / 8760) * (data_in_use[r, , ][loc] + data_in_use[r, , ][loc + 1] +
                    tmp_target[3] / data_sel[r, , "nur"][loc + 2])
                } else {
                  tmp_target[4] <- tmp_target[3] - data_sel[r, , "maxprod"][, , loc + 2]
                  if (data_sel[r, , "maxprod"][, , loc + 3] > tmp_target[4]) {
                    final[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + data_in_use[r, , ][loc + 1] + data_in_use[r, , ][, , loc + 2] +
                      tmp_target[4] / data_sel[r, , "nur"][, , loc + 3])
                  } else {
                    tmp_target[5] <- tmp_target[4] - data_sel[r, , "maxprod"][, , loc + 3]
                    if (data_sel[r, , "maxprod"][loc + 4] > tmp_target[5]) {
                      final[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + data_in_use[r, , ][loc + 1] + data_in_use[r, , ][, , loc + 2] +
                        data_in_use[r, , ][, , loc + 3] + tmp_target[5] / data_sel[r, , "nur"][, , loc + 4])
                    } else {
                      tmp_target[6] <- tmp_target[5] - data_sel[r, , "maxprod"][, , loc + 4]
                      if (data_sel[r, , "maxprod"][loc + 5] > tmp_target[6]) {
                        final[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + data_in_use[r, , ][loc + 1] + data_in_use[r, , ][, , loc + 2] +
                          data_in_use[r, , ][, , loc + 3] + data_in_use[r, , ][, , loc + 4] +
                          tmp_target[6] / data_sel[r, , "nur"][, , loc + 5])
                      } else {
                        tmp_target[7] <- tmp_target[6] - data_sel[r, , "maxprod"][, , loc + 5]
                        if (data_sel[r, , "maxprod"][loc + 6] > tmp_target[7]) {
                          final[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + data_in_use[r, , ][loc + 1] + data_in_use[r, , ][, , loc + 2] +
                            data_in_use[r, , ][, , loc + 3] + data_in_use[r, , ][, , loc + 4] + data_in_use[r, , ][, , loc + 5]
                            + tmp_target[7] / data_sel[r, , "nur"][, , loc + 6])
                        } else {
                          tmp_target[8] <- tmp_target[7] - data_sel[r, , "maxprod"][, , loc + 6]
                          if (data_sel[r, , "maxprod"][loc + 7] > tmp_target[8]) {
                            final[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + data_in_use[r, , ][loc + 1] + data_in_use[r, , ][, , loc + 2] +
                              data_in_use[r, , ][, , loc + 3] + data_in_use[r, , ][, , loc + 4] + data_in_use[r, , ][, , loc + 5]
                              + data_in_use[r, , ][, , loc + 6] + tmp_target[8] / data_sel[r, , "nur"][, , loc + 7])
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
        x_cap_pt_2_tic[, y, t] <- final
      }
    }

    # combining generation to installed capacity targets of biomass and geothermal from before
    x_cap_pt_2_tic <- mbind(x_cap_pt_2_tic[, , c("SolarPV", "SolarCSP", "Wind", "Hydro")], x_cap_pt[, , c("Biomass", "Geothermal")])
    # choose the maximum of installed capacity targets
    techs_e_hydro <- setdiff(techs, "Hydro")
    x_cap_pt_2_tic[, , techs_e_hydro] <- pmax(x_cap_ac[, , techs_e_hydro], x_cap_tic[, , techs_e_hydro], x_cap_pt_2_tic[, , techs_e_hydro])

    for (r in regions) {
      for (t in techs) {
        for (i in c(2020, 2025, 2030, 2035)) {
          if (x_cap_pt_2_tic[r, i + 5, t] < setYears(x_cap_pt_2_tic[r, i, t])) {
            x_cap_pt_2_tic[r, i + 5, t] <- setYears(x_cap_pt_2_tic[r, i, t])
          }
        }
      }
    }

    # countries not in the REN21 capacity targets database
    rest_regions <- setdiff(getRegions(hist_cap), getRegions(x_cap))
    x_other <- new.magpie(rest_regions, target_years, techs)
    # for all other countries not in database, targets for all model years are historical capacities
    x_other[, , c("Wind", "SolarPV", "SolarCSP", "Biomass", "Geothermal")] <- setYears(hist_cap[rest_regions, 2019, techs_e_hydro])
    x_other[, , "Hydro"] <- setYears(hist_cap[rest_regions, 2019, "Hydro"]) * setYears(cf_hydro[rest_regions, , ])
    x_final <- magpiesort(mbind(x_cap_pt_2_tic, x_other))
    x <- x_final
    x[is.na(x)] <- 0
    # renaming to REMIND convention
    getNames(x) <- c("spv", "csp", "wind", "hydro", "biochp", "geohdr")
  } else if (subtype == "investmentCosts") {
    # save data of specific countries
    x_country <- x[c("China", "India", "United States"), , ]
    # translate country names into ISO-codes
    getItems(x_country, dim = 1) <- toolCountry2isocode(getRegions(x_country))
    # delete those countries from x
    x <- x[c("China", "India", "United States"), , , invert = TRUE]

    # split up regional data into countries
    map <- read.csv("regionmappingREN2Country.csv", sep = ";")
    x <- toolAggregate(x, map, weight = NULL)

    # overwrite country data
    x[getRegions(x_country), , ] <- x_country
  }

  return(x)
}
