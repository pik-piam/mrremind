#' Convert policy targets for NDCs from UNFCCC_NDC
#'
#' Converts conditional and unconditional capacity and production targets into total capacity (GW) in target year.
#' For countries and years without targets, 2015 values from IRENA and BP are used to fill the gaps.
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

    # TODO: do we want to implement FE-Production-Share?

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

    # generate target years, at least to 2035, but allow every year in x to be rounded up to next fiver
    targetYears <- seq(2020, max(2035, 4 + max(getYears(x, as.integer = TRUE))), by = 5)

    # generate new object x_target that has values only for targetYears and transfer those from x
    x_target <- new.magpie(getItems(x, dim = 1), targetYears, getNames(x), fill = NA)
    x_target[, intersect(getYears(x), getYears(x_target)), ] <- x[, intersect(getYears(x), getYears(x_target)), ]

    for (i in getYears(x, as.integer = TRUE)) {
      if (i %% 5 != 0) {
        # for non-fiver years, transfer them to following fiver year, increasing every year by 5 percentage points
        x[, i, ] <- x[, i, ] * (1 + (5 - (i %% 5)) * 0.05)
        j <- i - (i %% 5) + 5
        if (j %in% getYears(x, as.integer = TRUE)) {
          # if there is already a value for the year, use the higher value
          x_target[, j, ] <- pmax(x[, i, ], x[, j, ], na.rm = TRUE)
        } else {
          x_target[, j, ] <- x[, i, ]
        }
      }
    }

    x_capacity <- new.magpie(getItems(x_target, dim = 1), getItems(x_target, dim = 2),
      getNames(x_target, dim = 2),
      fill = NA
    )

    # gather reference capacities (2015) ----
    # x_ref contains 2015 capacities except for hydro where current generation values are taken

    x_ref <- x_capacity

    hist_cap <- readSource("IRENA", subtype = "Capacity") / 1000 # Units in GW
    hist_gen <- readSource("IRENA", subtype = "Generation") # Units in GWh

    # capacity factors determine how much electricity (GWh/yr) you can produce from 1 GW of capacity
    # the lower the number, the more electricity can be produced

    # Real world capacity factor for hydro = Generation in 2015 / Capacity in last 2015
    # using cf_hydro_realworld directly causes converges errors because some are very small
    # TODO: why not use the more detailed capacity factors from PotentialHydro?
    # Note: "Hydropower" contains renewable hydropower and mixed hydro plants, but not pure pumped storage
    cf_hydro_realworld <- hist_gen[, 2015, "Hydropower"] /
      (8760 * hist_cap[, 2015, "Hydropower"])
    cf_hydro <- max(cf_hydro_realworld, na.rm = TRUE)

    # Capacity factors in REMIND, obtained using calcOutput("Capacityfactor")
    # TODO: update hard coded values or read in directly?
    cf_biomass <- 0.75
    cf_nuclear <- 0.85

    # initialize Wind, Solar, Biomass
    x_ref[, , c("Wind", "Solar")] <- hist_cap[getItems(x_ref, dim = 1), 2015, c("Wind", "Solar")]
    x_ref[, , "Biomass"] <- hist_cap[getItems(x_ref, dim = 1), 2015, "Bioenergy"]

    # initialize Hydro with historical generation
    x_ref[, , "Hydro"] <- hist_gen[getItems(x_ref, dim = 1), 2015, "Hydropower"]

    # initialize Nuclear
    hist_gen_nuclear <- readSource("BP", subtype = "Generation")
    # convert TWh to GWh
    hist_gen_nuclear <- hist_gen_nuclear[getItems(x_ref, dim = 1), 2015, "Generation|Nuclear (TWh)"] * 1000
    hist_cap_nuclear <- hist_gen_nuclear / (8760 * cf_nuclear)

    # 0/1 mask to only initialize cells with targets in any year
    # this is most likely a correction of the values coming from BP,
    # which in a few cases are the result of disaggregating regions (e.g. Other CIS)
    # TODO: might lead to weird behaviour when actual targets are 0, can this be removed?
    mask <- dimSums(x_target, dim = c(2, 3.1), na.rm = TRUE)[, , "Nuclear"]
    mask[mask != 0] <- 1

    x_ref[, , "Nuclear"] <- hist_cap_nuclear * mask

    # handle Target Type 'AC-Absolute' ----
    x_capacity_abs <- x_capacity

    # converting additional capacity targets to absolute capacity targets

    # all additional capacity targets currently mean 'in addition to 2015'
    # TODO: read in reference year as well, otherwise invalid capacity target
    t <- c("Nuclear", "Biomass", "Wind", "Solar")
    x_capacity_abs[, , t] <- x_ref[, , t] + x_target[, , "AC-Absolute", drop = TRUE][, , t]

    # TODO: SDN 2024 target has the wrong target type?
    # for Hydro, this is production (GWh/yr)
    x_capacity_abs[, , "Hydro"] <- x_ref[, , "Hydro"] +
      x_target[, , "AC-Absolute.Hydro", drop = TRUE] * cf_hydro * 8760

    # handle Target Type 'TIC-Absolute' ----
    x_capacity_tic <- x_capacity

    # total installed capacity targets are already in the right format
    x_capacity_tic[, , c("Nuclear", "Biomass", "Wind", "Solar")] <- x_target[, , c("Nuclear", "Biomass", "Wind", "Solar")][, , "TIC-Absolute", drop = TRUE]

    # for Hydro, this is production (GWh/yr)
    # TODO: Isn't the conversion capacity to production missing multiplication with 8760?
    x_capacity_tic[, , "Hydro"] <- x_target[, , "TIC-Absolute.Hydro", drop = TRUE] * cf_hydro

    # handle Target Type 'Production-Absolute' for Biomass and Nuclear ----
    x_capacity_prod <- x_capacity

    # converting production targets (GWh) to absolute capacity targets (GW)

    # capacity in target year, calculated from production
    x_capacity_prod[, , "Nuclear"] <- x_target[, , "Production-Absolute.Nuclear"] / (8760 * cf_nuclear)
    x_capacity_prod[, , "Biomass"] <- x_target[, , "Production-Absolute.Biomass"] / (8760 * cf_biomass)

    # handle Target Type 'Production-Absolute' for Solar, Wind, and Hydro ----

    # obtain the capacity factors (nur) and associated maximum production (maxprod) for Hydro, Wind, and Solar
    # the quality is a continuous number (1-9 for Solar/ Wind and 1-5 for Hydro) that is binned onto capacity factors,
    # each with a limited installation potential, the lower the number, the higher the capacity factor
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

    # for Hydro, any TIC and AC Absolute targets are converted to Production-Absolute targets in GWh/yr
    # Production-Absolute is the max of TIC, AC, Production-Absolute, and 2015 reference
    # TODO: why not do this just like for other Renewables? why special treatment for Hydro?
    # TODO: why do the complex calculation below for reference values as well?

    x_target[, , "Production-Absolute.Hydro"] <- pmax(x_target[, , "Production-Absolute.Hydro"],
      x_capacity_tic[, , "Hydro"],
      x_capacity_abs[, , "Hydro"],
      x_ref[, , "Hydro"],
      na.rm = TRUE
    )

    # drop the target for all Production-Absolute Hydro targets with maxprod all-zero,
    # or at least one negative maxprod value
    for (r in getItems(x_target, dim = 1)) {
      if (any(x_target[r, , "Production-Absolute.Hydro"] != 0) &&
        (all(potential[r, , "Hydro.maxprod"] == 0) || any(potential[r, , "Hydro.maxprod"] < 0))) {
        message(subtype, ": Dropping target(s) of type 'Production-Absolute' for Hydro in country for ", r, ", as it has zero or negative maxprod.")
        x_target[r, , "Production-Absolute.Hydro"] <- 0
      }
    }

    # converting production targets (GWh/yr) to absolute capacity targets (GW)
    # by allotting production to certain capacity factors based on maxprod

    regions <- numeric(length(getItems(x_target, dim = 1)))
    names(regions) <- getItems(x_target, dim = 1)


    # TODO: refactor this
    for (t in c("Solar", "Wind", "Hydro")) {
      pot <- potential[, , t]
      tar <- x_target[, , t]
      data_in_use <- pot[, , "maxprod"] / pot[, , "nur"]
      for (y in targetYears) {
        regions[] <- 0
        for (r in getItems(tar, dim = 1)) {
          tmp_target <- numeric(10)
          # target exists
          if (!is.na(tar[r, y, "Production-Absolute"]) && !R.utils::isZero(tar[r, y, "Production-Absolute"])) {
            # cumulated maxprod potential exceeds largest target for that region
            if (dimSums(pot[r, , "maxprod"], na.rm = TRUE) > max(tar[r, , "Production-Absolute"], na.rm = TRUE)) {
              # extracting the first non-zero location of maxprod
              loc <- min(which(!R.utils::isZero(pot[r, , "maxprod"])))
              tmp_target[1] <- tar[r, y, "Production-Absolute"]
              # maxprod for best capacity factor exceeds target
              if (pot[r, , "maxprod"][, , loc] > tmp_target[1]) {
                # convert production to capacity using best capacity factor
                regions[r] <- tmp_target[1] / (8760 * pot[r, , "nur"][, , loc])
              } else {
                tmp_target[2] <- tmp_target[1] - pot[r, , "maxprod"][, , loc]
                if (pot[r, , "maxprod"][, , loc + 1] > tmp_target[2]) {
                  # TODO this is most likely wrong, should be tmp_target[2]
                  regions[r] <- (1 / 8760) * (data_in_use[r, , ][, , loc] + tmp_target[1] / pot[r, , "nur"][, , loc + 1])
                } else {
                  # so far, we never get to beyond point in the calculation
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
            } else {
              message(
                subtype, ": Dropping ", y, " target of type 'Production-Absolute' for ", t,
                " in ", r, ", as it exceeds maxprod potential."
              )
            }
          }
        }
        x_capacity_prod[, y, t] <- regions
      }
    }

    # merge capacities ---

    # take the maximum of all target types (usually, only one is target is given)
    # and the 2015 reference value
    x_capacity[, , c("Solar", "Wind", "Biomass", "Nuclear")] <- pmax(
      x_capacity_abs[, , c("Solar", "Wind", "Biomass", "Nuclear")],
      x_capacity_tic[, , c("Solar", "Wind", "Biomass", "Nuclear")],
      x_capacity_prod[, , c("Solar", "Wind", "Biomass", "Nuclear")],
      x_ref[, , c("Solar", "Wind", "Biomass", "Nuclear")],
      na.rm = TRUE
    )

    # TIC and AC-Absolute for Hydro and 2015 reference have been included in previous steps already
    x_capacity[, , "Hydro"] <- x_capacity_prod[, , "Hydro"]

    # make sure that targets in subsequent years are always same or greater
    for (r in getItems(x_target, dim = 1)) {
      for (t in getNames(x_capacity)) {
        for (i in utils::head(targetYears, -1)) {
          if (is.na(x_capacity[r, i + 5, t]) || x_capacity[r, i + 5, t] < x_capacity[r, i, t]) {
            x_capacity[r, i + 5, t] <- x_capacity[r, i, t]
          }
        }
      }
    }

    # fill other regions with 2015 capacity values ----
    otherRegions <- setdiff(getItems(hist_cap, dim = 1), getItems(x_capacity, dim = 1))

    x_other <- new.magpie(otherRegions, targetYears, getNames(x_capacity))
    x_other[, , c("Wind", "Solar")] <- hist_cap[otherRegions, 2015, c("Solar", "Wind")]
    x_other[, , "Biomass"] <- hist_cap[otherRegions, 2015, "Bioenergy"]
    x_other[, , "Hydro"] <- hist_cap[otherRegions, 2015, "Hydropower"] * cf_hydro
    x_other[, , "Nuclear"] <- 0

    x <- magpiesort(mbind(x_capacity, x_other))
    x[is.na(x)] <- 0

    # rename technologies according to REMIND ----
    m <- c(
      "Wind" = "wind", "Solar" = "spv", "Biomass" = "bioigcc",
      "Nuclear" = "tnrs", "Hydro" = "hydro"
    )
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
