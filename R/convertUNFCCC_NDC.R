#' Convert policy targets for NDCs from UNFCCC_NDC
#'
#' Converts conditional and unconditional capacity and production targets into total capacity (GW) in target year.
#' For countries and years without targets, 2015 values from IRENA are used to fill the gaps.
#'
#' Emission targets are represented by a GHG factor, which is the quotient of total GHG
#' emissions in the target year divided by the CEDS GHG emissions in 2005.
#'
#' @param x a magclass object to be converted
#' @param subtype Capacity_YYYY_cond or Capacity_YYYY_uncond for Capacity Targets, Emissions_YYYY_cond or
#'   Emissions_YYYY_uncond for Emissions targets, with YYYY NDC version year
#' @param subset String, designating the GDP scenarios to use. Only used for emission targets.
#' @author Aman Malik, Christoph Bertram, Oliver Richters, Sophie Fuchs, Rahel Mandaroux, Falk Benke
#' @seealso [readIRENA()]
#'
convertUNFCCC_NDC <- function(x, subtype, subset = NULL) { # nolint: object_name_linter.

  if (grepl("Capacity", subtype, fixed = TRUE)) {

    # pre-processing ----
    x <- complete_magpie(x)


    if (grepl("uncond", subtype, fixed = TRUE)) {
      x <- x[, , "conditional", invert = TRUE, drop = TRUE]
    } else {
      uncond <- x[, , "conditional", invert = TRUE, drop = TRUE]
      x <- x[, , "unconditional", invert = TRUE, drop = TRUE]
      # when there is no conditional target, adapt unconditional target
      x[is.na(x)] <- uncond[is.na(x)]
    }

    x[is.na(x)] <- 0

    # generate target years, at least to 2035, but allow every year in x to be rounded up to next fiver
    targetYears <- seq(2020, max(2035, 4 + max(getYears(x, as.integer = TRUE))), by = 5)

    # generate new object x_target that has values only for targetYears and transfer those from x
    x_target <- new.magpie(getItems(x, dim = 1), targetYears, getNames(x), fill = 0)
    x_target[, intersect(getYears(x), getYears(x_target)), ] <- x[, intersect(getYears(x), getYears(x_target)), ]

    # for non-fiver years, transfer them to following fiver year, increasing every year by 5 percentage points
    # resolve conflicts by using the higher value
    for (i in getYears(x, as.integer = TRUE)) {
      if (i %% 5 != 0) {
        x_target[, i - (i %% 5) + 5, ] <- apply(
          matrix(c(as.vector(x_target[, i - (i %% 5) + 5, ]),
                   as.vector(x[, i, ]) * (1 + (5 - (i %% 5)) * 0.05)), ncol = 2), 1, max
        )
      }
    }

    x_capacity <- new.magpie(getItems(x_target, dim = 1), getItems(x_target, dim = 2),
                             getNames(x_target, dim = 2), fill = 0)

    # gather reference capacities (2015) ----
    # x_ref contains 2015 capacities except for hydro where current generation values are taken

    x_ref <- x_capacity

    hist_cap <- readSource("IRENA", subtype = "Capacity") / 1000 # Units in GW
    hist_gen <- readSource("IRENA", subtype = "Generation") # Units in GWh

    # Real world capacity factor for hydro = Generation in 2015 / Capacity in last 2015
    # using cf_hydro_realworld directly causes converges errors because some are very small
    cf_hydro_realworld <- hist_gen[, 2015, "Renewable hydropower"] /
      (8760 * hist_cap[, 2015, "Renewable hydropower"])
    cf_hydro <- max(cf_hydro_realworld, na.rm = TRUE)

    # Capacity factors in REMIND, obtained using calcOutput("Capacityfactor")
    # TODO: update hard coded values or read in directly?
    cf_biomass <- 0.75
    cf_nuclear <- 0.85

    # initialize Wind, Solar, Biomass
    x_ref[, , c("Wind", "Solar")] <- hist_cap[getItems(x_ref, dim = 1), 2015, c("Wind", "Solar")]
    x_ref[, , "Biomass"] <- hist_cap[getItems(x_ref, dim = 1), 2015, "Bioenergy"]

    # initialize Hydro with historical generation
    x_ref[, , "Hydro"] <- hist_gen[getItems(x_ref, dim = 1), 2015, "Renewable hydropower"]

    # initialize Nuclear
    # TODO: find a better source for this? or rather use with convert = F?
    hist_gen_nuclear <- readSource("BP", subtype = "Generation")
    # convert TWh to GWh
    hist_gen_nuclear <- hist_gen_nuclear[getItems(x_ref, dim = 1), 2015, "Generation|Nuclear (TWh)"] * 1000
    hist_cap_nuclear <- hist_gen_nuclear / (8760 * cf_nuclear)

    # 0/1 mask to only initialize cells with targets in any year
    # this is most likely a correction of the values coming from BP,
    # which in some cases are the result of disaggregating regions
    mask <- dimSums(x_target, dim = c(2, 3.1))[, , "Nuclear"]
    mask[mask != 0] <- 1

    x_ref[, , "Nuclear"] <- hist_cap_nuclear * mask

    # handle Target Type 'AC-Absolute' ----
    x_capacity_abs <- x_capacity

    # converting additional capacity targets to absolute capacity targets

    # TODO: so that means, all additional capacity targets mean 'in addition to 2015'?
    x_capacity_abs[, , c("Nuclear", "Biomass", "Wind", "Solar")] <- x_ref[, , c("Nuclear", "Biomass", "Wind", "Solar")] +
      x_target[, , c("AC-Absolute.Nuclear", "AC-Absolute.Biomass", "AC-Absolute.Wind", "AC-Absolute.Solar"), drop = TRUE]

    # TODO: SDN 2024 reported in GWh/yr, not in GW?
    # for Hydro, this is production (GWh/yr)
    x_capacity_abs[, , "Hydro"] <- x_ref[, , "Hydro"] +
      x_target[, , "AC-Absolute.Hydro", drop = TRUE] * cf_hydro * 8760


    # handle Target Type 'TIC-Absolute' ----
    x_capacity_tic <- x_capacity

    # total installed capacity targets are the maximum of 2015 capacity and capacity in target year
    x_capacity_tic[, , c("Nuclear", "Biomass", "Wind", "Solar")] <- pmax(
      x_ref[, , c("Nuclear", "Biomass", "Wind", "Solar")],
      x_target[, , c("Nuclear", "Biomass", "Wind", "Solar")][, , "TIC-Absolute", drop = TRUE]
    )

    # for Hydro, this is production (GWh/yr)
    x_capacity_tic[, , "Hydro"] <- pmax(
      x_ref[, , "Hydro"],
      # TODO: Isn't the conversion missing * 8670?
      x_target[, , "TIC-Absolute.Hydro", drop = TRUE] * cf_hydro
    )

    # handle Target Type 'Production-Absolute' for Biomass and Nuclear ----
    x_capacity_prod <- x_capacity

    # converting production targets (GWh) to absolute capacity targets (GW)

    # always take the higher value from 2015 capacity and capacity in target year, calculated from production
    x_capacity_prod[, , "Nuclear"] <- pmax(x_ref[, , "Nuclear"],
                                           x_target[, , "Production-Absolute.Nuclear"] / (8760 * cf_nuclear))
    x_capacity_prod[, , "Biomass"] <- pmax(x_ref[, , "Biomass"],
                                           x_target[, , "Production-Absolute.Biomass"] / (8760 * cf_biomass))

    # handle Target Type 'Production-Absolute' for Solar, Wind, and Hydro ----

    # obtain the capacity factors (nur) values and associated maximum production (maxprod) for Hydro, Wind, and Solar
    potentialWind <- calcOutput("PotentialWindOn", aggregate = FALSE)
    potentialWind <- add_dimension(potentialWind, dim = 3.1, add = "tech", nm = "Wind")

    potentialHydro <- calcOutput("PotentialHydro", aggregate = FALSE)
    potentialHydro <- add_dimension(potentialHydro, dim = 3.1, add = "tech", nm = "Hydro")
    potential <- mbind(potentialWind, potentialHydro)

    potentialSolar <- calcOutput("Solar", regionmapping = "regionmappingTCD.csv")[c("TCD", "JPN"), , c("nur", "maxprod")][, , "spv"]
    potentialSolar <- collapseDim(potentialSolar, dim = 3.2)
    potentialSolar <- toolCountryFill(potentialSolar, fill = NA, verbosity = 2)
    potentialSolar <- add_dimension(potentialSolar, dim = 3.1, add = "tech", nm = "Solar")
    potential <- mbind(potential, potentialSolar)

    # filter countries with target
    potential <- potential[getItems(x_target, dim = 1), , ]

    # make sure location is sorted in increasing order
    potential <- potential[, , as.character(seq(1:9))]

    # convert maxprod from EJ/a to GWh
    potential[, , "maxprod"] <- potential[, , "maxprod"] * 277777.778
    potential[is.na(potential)] <- 0

    # for Hydro, any TIC and AC Absolute targets are converted to GWh/yr
    # and copied over to Production-Absolute
    x_target[, , "Production-Absolute.Hydro"] <- pmax(x_target[, , "Production-Absolute.Hydro"],
                                                      x_capacity_tic[, , "Hydro"],
                                                      x_capacity_abs[, , "Hydro"])

    # drop the target for all Production-Absolute Hydro targets with maxprod
    # all-zero or at least one negative maxprod value,
    for (r in getItems(x_target, dim = 1)) {
      if (any(x_target[r, , "Production-Absolute.Hydro"] != 0) &&
          all(potential[r, , "Hydro.maxprod"] == 0) || any(potential[r, , "Hydro.maxprod"] < 0)) {
        message(subtype, "Dropping target(s) of type 'Production-Absolute' for Hydro in country for ", r,
                ", as it has zero or negative maxprod.")
        x_target[r, , "Production-Absolute.Hydro"] <- 0
      }
    }

    # converting production targets (GWh/yr) to absolute capacity targets (GW)
    # by allotting production to certain capacity factors based on maxprod

    regions <- numeric(length(getItems(x_target, dim = 1)))
    names(regions) <- getItems(x_target, dim = 1)

    for (t in c("Solar", "Wind", "Hydro")) {
      pot <- potential[, , t]
      tar <- x_target[, , t]
      data_in_use <- pot[, , "maxprod"] / pot[, , "nur"]
      for (y in targetYears) {
        regions[] <- 0
        for (r in getItems(tar, dim = 1)) {
          tmp_target <- numeric(10)
          # target exists, maxprod exceeds maximum target
          if (!R.utils::isZero(tar[r, y, "Production-Absolute"]) &&
                dimSums(pot[r, , "maxprod"], na.rm = TRUE) > max(tar[r, , "Production-Absolute"])) {

            # extracting the first non-zero location of maxprod
            loc <- min(which(!R.utils::isZero(pot[r, , "maxprod"])))
            tmp_target[1] <- tar[r, y, "Production-Absolute"]
            # maxprod in first location already exceeds maximum target
            if (pot[r, , "maxprod"][, , loc] > tmp_target[1]) {
              # convert production to capacity using specific capacity factor
              regions[r] <- tmp_target[1] / (8760 * pot[r, , "nur"][, , loc])
            } else {
              tmp_target[2] <- tmp_target[1] - pot[r, , "maxprod"][, , loc]
              if (pot[r, , "maxprod"][, , loc + 1] > tmp_target[2]) {
                regions[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + tmp_target[1] / pot[r, , "nur"][, , loc + 1])
              } else {
                tmp_target[3] <- tmp_target[2] - pot[r, , "maxprod"][, , loc + 1]
                if (pot[r, , "maxprod"][, , loc + 2] > tmp_target[3]) {
                  regions[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + data_in_use[r, , ][, , loc + 1]
                                              + tmp_target[2] / pot[r, , "nur"][, , loc + 2])
                } else {
                  tmp_target[4] <- tmp_target[3] - pot[r, , "maxprod"][, , loc + 2]
                  if (pot[r, , "maxprod"][, , loc + 3] > tmp_target[4]) {
                    regions[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + data_in_use[r, , ][, , loc + 1] + data_in_use[r, , ][, , loc + 2] +
                                                  tmp_target[3] / pot[r, , "nur"][, , loc + 3])
                    regions[r] <- tmp_target[1]
                  } else {
                    tmp_target[5] <- tmp_target[4] - pot[r, , "maxprod"][, , loc + 3]
                    if (pot[r, , "maxprod"][loc + 4] > tmp_target[5]) {
                      regions[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + data_in_use[r, , ][, , loc + 1] + data_in_use[r, , ][, , loc + 2] +
                                                    data_in_use[r, , ][, , loc + 3] + tmp_target[4] / pot[r, , "nur"][, , loc + 4])
                    } else {
                      tmp_target[6] <- tmp_target[5] - pot[r, , "maxprod"][, , loc + 4]
                      if (pot[r, , "maxprod"][loc + 5] > tmp_target[6]) {
                        regions[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + data_in_use[r, , ][, , loc + 1] + data_in_use[r, , ][, , loc + 2] +
                                                      data_in_use[r, , ][, , loc + 3] + data_in_use[r, , ][, , loc + 4] +
                                                      tmp_target[5] / pot[r, , "nur"][, , loc + 5])
                      }
                    }
                  }
                }
              }
            }
          }
        }
        x_capacity_prod[, y, t] <- regions
      }
    }

    # merge capacities ---

    # take the maximum of
    x_capacity[, , c("Solar", "Wind", "Biomass", "Nuclear")] <- pmax(
      x_capacity_abs[, , c("Solar", "Wind", "Biomass", "Nuclear")],
      x_capacity_tic[, , c("Solar", "Wind", "Biomass", "Nuclear")],
      x_capacity_prod[, , c("Solar", "Wind", "Biomass", "Nuclear")]
    )

    # TIC and AC-Absolute for Hydro have been included in previous steps already
    x_capacity[, , "Hydro"] <- x_capacity_prod[, , "Hydro"]

    # make sure that targets in subsequent years are always same or greater
    for (r in getItems(x_target, dim = 1)) {
      for (t in getNames(x_capacity)) {
        for (i in utils::head(targetYears, -1)) {
          if (x_capacity[r, i + 5, t] < x_capacity[r, i, t]) {
            x_capacity[r, i + 5, t] <- x_capacity[r, i, t]
          }
        }
      }
    }

    # fill other regions with 2015 capacity values ----
    otherRegions <- setdiff(getItems(hist_cap, dim = 1), getItems(x_capacity, dim = 1))

    x_other <- new.magpie(otherRegions, targetYears, getNames(x_capacity))
    x_other[, , c("Wind", "Solar")]  <- hist_cap[otherRegions, 2015, c("Solar", "Wind")]
    x_other[, , "Biomass"] <- hist_cap[otherRegions, 2015, "Bioenergy"]
    x_other[, , "Hydro"] <- hist_cap[otherRegions, 2015, "Renewable hydropower"] * cf_hydro
    x_other[, , "Nuclear"] <- 0

    x <- magpiesort(mbind(x_capacity, x_other))
    x[is.na(x)] <- 0

    # rename technologies according to REMIND ----
    m <- c("Wind" = "wind", "Solar" = "spv", "Biomass" = "bioigcc",
           "Nuclear" = "tnrs", "Hydro" = "hydro")
    getNames(x) <- m[getNames(x)]

  }

  if (grepl("Emissions", subtype, fixed = TRUE)) {
    ghgFactor <- toolCalcGhgFactor(x, subtype, subset)
    x <- toolCountryFill(ghgFactor, fill = NA, verbosity = 2)
  }

  # add NDC version from subtype
  ver <- paste(unlist(strsplit(subtype, "_"))[-1], collapse = "_")
  x <- add_dimension(x, add = "version", nm = ver, dim = 3.1)
  return(x)
}
