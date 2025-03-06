#' Policy targets for REN21
#' @description This code aggregates and homogenises different types of
#' renewable energy targets into total installed capacity targets (in GW).
#' @details Policy database accessible in "inputdata/sources/REN21/README"
#' @param targets MAgPIE object to be converted
#' @param subtype Only "Capacity" as of now
#' @return Magpie object with Total Installed Capacity targets. The target years differ depending upon the database.
#' @author Aman Malik
#' @importFrom utils read.csv
#'
convertREN21 <- function(targets, subtype) {
  if (subtype == "Capacity") {
    hPerYear <- 365 * 24 # 8760
    targets <- magpiesort(targets) # sorting years chronologically and region names alphabetically
    targets[is.na(targets)] <- 0 # Converting all NAs to zero
    getItems(targets, dim = 1) <- toolCountry2isocode(getItems(targets, dim = 1)) # Country names to ISO3 code

    # reading historical data
    hist_cap <- readSource(type = "IRENA", subtype = "Capacity") / 1000 # converting from MW to GW
    hist_gen <- readSource(type = "IRENA", subtype = "Generation") # Units are GWh

    # Real world capacity factor for hydro = Generation in last year/Capacity in 2015
    cf_realworld <- hist_gen[, 2015, "Renewable hydropower"] / (hPerYear * hist_cap[, 2015, "Renewable hydropower"])
    cf_realworld[is.na(cf_realworld)] <- 0
    getNames(cf_realworld) <- "Hydro"
   
    
    tech_mapping <- tribble(
      ~REN21,       ~REMIND,    ~IRENA,    
      "SolarPV",    "spv",      "Solar photovoltaic",
      "SolarCSP",   "csp",      "Concentrated solar power",
      "Wind_ON",    "windon",   "Onshore wind energy",
      "Wind_OFF",   "windoff",  "Offshore wind energy",
      "Hydro",      "hydro",    "Hydropower",
      "Biomass",    "biochp",   "Bioenergy",  
      "Geothermal", "geohdr",   ""
    )

    # renaming variable names from IRENA historical database
    getNames(hist_cap) <- getNames(hist_cap) %>%
      map_chr(function(x) ifelse (
        x %in% tech_mapping$IRENA,
        filter(tech_mapping, IRENA == x) %>% pull(REN21),
        x
      ))

    targets_allYears <- targets # contains original targets + targets extrapolated to model years

    # 1. If targets are given in non-model year. e.g., 2022, 2032, then targets are extrapolated linearly.
    target_years <- c(2020, 2025, 2030, 2035, 2040)
    regions <- getItems(targets_allYears, dim = 1)
    tech_names <- getNames(targets_allYears[, , tech_mapping$REN21])
    for (year in getYears(targets, as.integer = TRUE)) {
      target_year <- ceiling(year / 5) * 5 # round to the above multiple of 5
      if (year != target_year && target_year %in% target_years) { # years not multiple of 5 (non-model years)
        for (region in regions) {
          for (tech_name in tech_names) {
            if (targets_allYears[region, year, tech_name] != 0) {
              targets_allYears[region, target_year, tech_name] <- setYears(targets_allYears[region, year, tech_name]) * (1 + (target_year - year) * 0.05)
            }
          }
        }
      }
    }

    # Creating new magpie object containing historical or base year targets
    targets_modelYears <- new.magpie(getItems(targets_allYears, dim = 1), target_years, tech_mapping$REN21)
    targets_modelYears[is.na(targets_modelYears)] <- 0 # replacing NAs with zero
    # Capacity factors in REMIND. From : calcOutput("Capacityfactor"...)
    cf_biomass <- 0.75
    cf_geothermal <- 0.8
    cf_hydro <- cf_realworld

    # Initialising all capacities for all model years to historical 2015 capacities.
    # except when Base year is mentioned (then initialise with base year capacities)
    # and except for Hydro which will get historical generation.

    year <- getYears(targets_allYears)
    for (t in year) {
      for (r in getItems(targets_allYears, dim = 1)) {
        if (targets_allYears[r, t, "AC-Absolute.Base year"] != 0) {
          targets_modelYears[r, , ] <- setYears(hist_cap[r, targets_allYears[r, t, "AC-Absolute.Base year"], tech_mapping$REN21])
        } else {
          targets_modelYears[r, , ] <- setYears(hist_cap[r, 2015, tech_mapping$REN21])
        }
      }
    }

    # for hydro overwrite capacity with generation
    targets_modelYears[, , "Hydro"] <- setYears(hist_gen[getItems(targets_modelYears, dim = 1), 2015, "Renewable hydropower"])

    targets_additionalCapacity <- targets_modelYears # object will contain additional capacity targets as absolute capacity targets
    targets_additionalCapacity[] <- 0
    targets_generation <- targets_modelYears # object will contain generation targets as absolute capacity targets
    targets_generation[] <- 0
    targets_installedCapacity <- targets_modelYears # object will contain total installed capacity targets
    targets_installedCapacity[] <- 0

    # Converting additional capacity targets to absolute capacity targets.
    targets_additionalCapacity[, , c("Biomass", "Wind_ON", "Wind_OFF", "SolarPV", "SolarCSP")] <-
      targets_modelYears[, , c("Biomass", "Wind_ON", "Wind_OFF", "SolarPV", "SolarCSP")] +
      targets_allYears[, target_years, c("AC-Absolute.Biomass", "AC-Absolute.Wind_ON", "AC-Absolute.Wind_OFF", "AC-Absolute.SolarPV", "AC-Absolute.SolarCSP"), drop = TRUE]

    # For hydro converting to additional generation targets
    targets_additionalCapacity[, , "Hydro"] <- targets_modelYears[, , "Hydro"] + targets_allYears[, target_years, "AC-Absolute.Hydro", drop = TRUE] * setYears(cf_hydro[getItems(targets_allYears, dim = 1), , ] * hPerYear)

    # Total installed capacity Targets for all technologies
    targets_installedCapacity[, ,      c("Wind_ON", "Wind_OFF", "SolarPV", "SolarCSP", "Biomass")] <- pmax(
      targets_modelYears[, ,           c("Wind_ON", "Wind_OFF", "SolarPV", "SolarCSP", "Biomass")],
      targets_allYears[, target_years, c("Wind_ON", "Wind_OFF", "SolarPV", "SolarCSP", "Biomass")][, , "TIC-Absolute", drop = TRUE])
    targets_installedCapacity[, , "Hydro"] <- pmax(targets_modelYears[, , "Hydro"], targets_allYears[, target_years, "TIC-Absolute.Hydro", drop = TRUE] * setYears(cf_hydro[getItems(targets_allYears, dim = 1), , ] * hPerYear))

    # Converting Production targets (GWh) to Capacity targets (TIC-Absolute) (GW) for geothermal and biomass
    # pmax takes the higher value from existing capacity and new capacity derived (from production)
    targets_generation[, , "Biomass"] <- pmax(targets_modelYears[, , "Biomass"], targets_allYears[, target_years, "Production-Absolute.Biomass"] / (hPerYear * cf_biomass))
    targets_generation[, , "Geothermal"] <- pmax(targets_modelYears[, , "Geothermal"], targets_allYears[, target_years, c("Production-Absolute.Geothermal")] / (hPerYear * cf_geothermal))

    # for solar, wind, and hydro conversion will be done using another method below
    targets_generation[, target_years, c("SolarPV", "SolarCSP", "Wind_ON", "Wind_OFF")] <- 
       targets_allYears[, target_years, c("SolarPV", "SolarCSP", "Wind_ON", "Wind_OFF")][, , "Production-Absolute", drop = TRUE]


    # Converting Production targets to capacity targets for solar and wind
    # For Hydro, all targets (which have already been converted into generation targets) will be
    # converted into capacity targets based on maxprod and capacity factors.

    # Special case for hydro as mentioned above
    targets_generation[, target_years, "Hydro"] <- pmax(targets_allYears[, target_years, "Production-Absolute.Hydro"], targets_installedCapacity[, , "Hydro"], targets_additionalCapacity[, , "Hydro"])

    # Obtaining the capacity factors (nur) values and associated maxproduction (maxprod) for Hydro, Wind, and Solar
    data_hydro <- calcOutput("PotentialHydro", aggregate = FALSE)
    data_solar <- calcOutput("Solar")
    data_windon <- calcOutput("PotentialWindOn", aggregate = FALSE)
    data_windoff <- calcOutput("PotentialWindOff", aggregate = FALSE)
    # Reordering dim=3 for wind data so that 1st position corresponds to maxprod.nur.1 and not maxprod.nur.9
    data_windon_sorted <- do.call(mbind, lapply(1:9, function(i) data_windon[, , as.character(i)]))
    data_windoff_sorted <- do.call(mbind, lapply(1:9, function(i) data_windoff[, , as.character(i)]))

    names_hydro <- paste0("Hydro.", getNames(data_hydro))
    names_solarPV <- paste0("SolarPV.", getNames(collapseNames((mselect(data_solar, type = c("nur", "maxprod"), technology = "spv")), collapsedim = 2)))
    names_solarCSP <- paste0("SolarCSP.", getNames(collapseNames((mselect(data_solar, type = c("nur", "maxprod"), technology = "csp")), collapsedim = 2)))
    names_windon <- paste0("Wind_ON.", getNames(data_windon_sorted))
    names_windoff <- paste0("Wind_OFF.", getNames(data_windoff_sorted))
    data_combined <- new.magpie(getItems(data_hydro, dim = 1), NULL, c(names_solarPV, names_solarCSP, names_hydro, names_windon, names_windoff))
    data_combined[, , "Hydro"] <- data_hydro
    data_combined[, , "Wind_ON"] <- data_windon_sorted
    data_combined[, , "Wind_OFF"] <- data_windoff_sorted

    data_combined[c("MKD"), , c("SolarPV", "SolarCSP")][, , c("maxprod", "nur")] <- as.vector(data_solar[c("JPN"), , c("spv", "csp")][, , c("maxprod", "nur")])
    data_combined[c("KOR"), , c("SolarPV", "SolarCSP")][, , c("maxprod", "nur")] <- as.vector(data_solar[c("JPN"), , c("spv", "csp")][, , c("maxprod", "nur")])

    data_combined <- data_combined[getItems(targets_allYears, dim = 1), , ] # only interested in limited dataset

    for (n in getNames(data_combined, dim = 1)) {
      name <- paste0(n, ".maxprod")
      # Conversion from EJ/a to GWh
      data_combined[, , name, pmatch = TRUE] <- data_combined[, , name, pmatch = TRUE] * 277777.778
    }

    installedCapacity <- numeric(nregions(targets_allYears))
    names(installedCapacity) <- getItems(targets_allYears, dim = 1)
    tmp_target <- numeric(10)
    targets_installedCapacityGrades <- targets_modelYears
    targets_installedCapacityGrades[] <- 0
    # For all countries which have non-zero generation values but zero or negative
    # maxprod() for hydro (i.e., for some read), replace targets_allYears[,,"Production-Absolute.Hydro]==0,
    # Even if there is one positive production absolute value for Hydro but all maxprod are zero
    for (t in c("SolarPV", "SolarCSP", "Wind_ON", "Wind_OFF", "Hydro")) {
      data_sel <- data_combined[, , t]
      data_in_use <- data_sel[, , "maxprod"] / data_sel[, , "nur"]
      
      for (y in target_years) {
        installedCapacity[] <- 0
        for (r in names(installedCapacity)) {
          # Only start loop if Production targets are non-zero and if maxprod for that country can absorb the targets set.
          name <- paste0(t, ".maxprod")
          if (!R.utils::isZero(targets_generation[r, y, t] & dimSums(data_combined[r, , name]) > max(targets_generation[r, , t]))) {
            # extracting the first grade with non-zero
            min_grade <- min(which(!R.utils::isZero(data_combined[r, , name, pmatch = TRUE])))
            
            remaining_target <- targets_generation[r, y, t]
            for (grade in min_grade:9) {
              # Check if the current grade can satisfy the remaining target
              if (data_sel[r, , "maxprod"][, , grade] > remaining_target) {
                # Update the installedCapacity value and break the loop
                installedCapacity[r] <- (1 / hPerYear) * (
                  sum(data_in_use[r, , ][, , min_grade:(grade - 1)]) +
                  remaining_target / data_sel[r, , "nur"][, , grade]
                )
                break
              } else { # Update the remaining target and continue to the next grade
                remaining_target <- remaining_target - data_sel[r, , "maxprod"][, , grade]
              }
            }
          }
        }
        targets_installedCapacityGrades[, y, t] <- installedCapacity
      }
    }


    # combining generation to installed capacity targets of biomass and geothermal from before
    targets_installedCapacityGrades <- mbind(targets_installedCapacityGrades[, , c("SolarPV", "SolarCSP", "Wind_ON", "Wind_OFF", "Hydro")], targets_generation[, , c("Biomass", "Geothermal")])
    # choose the maximum of installed capacity targets
    techs_wo_hydro <- setdiff(tech_mapping$REN21, "Hydro")
    targets_installedCapacityGrades[, , techs_wo_hydro] <- pmax(
      targets_additionalCapacity[, , techs_wo_hydro],
      targets_installedCapacity[, , techs_wo_hydro],
      targets_installedCapacityGrades[, , techs_wo_hydro]
    )

    for (r in regions) {
      for (t in tech_mapping$REN21) {
        for (i in c(2020, 2025, 2030, 2035)) {
          if (targets_installedCapacityGrades[r, i + 5, t] < setYears(targets_installedCapacityGrades[r, i, t])) {
            targets_installedCapacityGrades[r, i + 5, t] <- setYears(targets_installedCapacityGrades[r, i, t])
          }
        }
      }
    }

    # countries not in the REN21 capacity targets database
    rest_regions <- setdiff(getRegions(hist_cap), getRegions(targets_modelYears))
    targets_otherRegions <- new.magpie(rest_regions, target_years, tech_mapping$REN21)
    # for all other countries not in database, targets for all model years are historical capacities
    targets_otherRegions[, , c("Wind_ON", "Wind_OFF", "SolarPV", "SolarCSP", "Biomass", "Geothermal")] <- setYears(hist_cap[rest_regions, 2019, techs_wo_hydro])
    targets_otherRegions[, , "Hydro"] <- setYears(hist_cap[rest_regions, 2019, "Hydro"]) * setYears(cf_hydro[rest_regions, , ])
    targets <- magpiesort(mbind(targets_installedCapacityGrades, targets_otherRegions))
    targets[is.na(targets)] <- 0
    # renaming to REMIND convention
    getNames(targets) <- tech_mapping$REMIND
  } else if (subtype == "investmentCosts") {
    # save data of specific countries
    targets_selectedCountries <- targets[c("China", "India", "United States"), , ]
    # translate country names into ISO-codes
    getItems(targets_selectedCountries, dim = 1) <- toolCountry2isocode(getRegions(targets_selectedCountries))
    # delete those countries from targets
    targets <- targets[c("China", "India", "United States"), , , invert = TRUE]

    # split up regional data into countries
    map <- read.csv("regionmappingREN2Country.csv", sep = ";")
    targets <- toolAggregate(targets, map, weight = NULL)

    # overwrite country data
    targets[getRegions(targets_selectedCountries), , ] <- targets_selectedCountries
  }

  return(targets)
}
