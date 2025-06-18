#' Calculates air pollutant emissions, activities and emission factors
#' for all scenarios and SSPs available from GAINS, including both
#' legacy and 2025 variants of GAINS data. This generates the actual
#' files needed for REMIND in the proper format, for the scenario logic,
#' see `GAINS2025scenarios`.
#'
#' @return Activity levels, emissions or emission factors
#' @author Gabriel Abrahao
#' @param subtype Either "emissions", "emission_factors", or "emission_factors_remindsectors"
#'
calcGAINS2025forREMIND <- function(subtype) {

  # Binds new and old versions of GAINS data, adding an "ssp" dimension
  # to old data and makes the new data conform to the old shape besides the
  # new dimension
  bindNewOld <- function(innew, inold) {
    # Assume a dummy "GAINSlegacy" SSP for the old data to fill the dimension
    newlastdim <- 3 + (as.integer(substr(utils::tail(names(getSets(inold)), 1), 4, 4)) + 1) * 1e-1
    old <- toolAddDimensions(inold, "GAINSlegacy", "ssp", newlastdim)
    getSets(innew) <- c("region", "year", "scenario", "ssp", "sector", "emi")
    outdimorder <- match(getSets(innew), getSets(old))[3:length(getSets(innew))] - 2
    innew <- dimOrder(innew, outdimorder)

    # Sectors absent in new data, should all have zero in the old data
    # Pad new data with these old sectors
    wstsecs <- setdiff(getItems(old, "sector"), getItems(innew, "sector"))
    dummy <- old[, , wstsecs][, , "SSP2"]
    if (!all(dummy == 0)) {
      warning(paste0(
        "When padding sectors in GAINS2025 with those from GAINSlegacy, found non-zero values in sectors: ",
        paste(wstsecs, collapse = " ")
      ))
    }
    dummy <- mbind(lapply(getItems(innew, "scenario"), \(x) setItems(dummy, "scenario", x)))
    dummy <- mbind(lapply(getItems(innew, "ssp"), \(x) setItems(dummy, "ssp", x)))

    out <- mbind(old, innew, dummy)
    getComment(out)[1] <- getFromComment(innew, "description")
    getComment(out)[2] <- getFromComment(innew, "unit")
    return(out)
  }

  if (subtype == "emissions") {
    inold <- calcOutput("GAINSEmi", subtype = "emissions", aggregate = FALSE)
    innew <- calcOutput("GAINS2025scenarios", subtype = "emissions", aggregate = FALSE)
    out <- bindNewOld(innew, inold)

    wgt <- NULL
    desc <- getFromComment(innew, "description")
    unit <- getFromComment(innew, "unit")
  } else if (subtype == "emission_factors") {
    linold <- calcOutput("GAINSEmi", subtype = "emission_factors", aggregate = FALSE, supplementary = TRUE)
    inold <- linold$x
    linnew <- calcOutput("GAINS2025scenarios", subtype = "emission_factors", aggregate = FALSE, supplementary = TRUE)
    innew <- linnew$x
    out <- bindNewOld(innew, inold)

    # Weights, GAINS2025 uses activities that are per SSP but not per scenario, pad the dimensions
    # TODO: address warnings negative weights and zero weight sum
    wgt <- bindNewOld(linnew$weight, linold$weight)
    desc <- getFromComment(innew, "description")
    unit <- getFromComment(innew, "unit")
  } else if (subtype == "emissions_starting_values") {
    stop(paste0(
      "subtype: ",
      subtype, " is not yet implemented, use instead the old starting values implementation from calcGAINSemi"
    ))
  } else if (subtype == "emission_factors_remindsectors") {
    # ==============================================================================================================
    # GAINS2025
    # ==============================================================================================================
    # Input emission factors
    linnew <- calcOutput("GAINS2025scenarios", subtype = "emission_factors", aggregate = FALSE, supplementary = TRUE)
    innew <- linnew$x
    wgtnew <- linnew$weight

    # Apparently REMIND expects TgS internally, but not in exoGAINS
    conv_ktSO2_to_ktS <- 1 / 2 # 32/(32+2*16)
    innew[, , "SO2"] <- innew[, , "SO2"] * conv_ktSO2_to_ktS

    # This makes it easier to line up with the old standard
    fixDims <- function(mag) {
      getSets(mag) <- c("region", "year", "scenario", "ssp", "sector", "emi")
      mag <- dimOrder(
        mag, match(getSets(mag), c("region", "year", "sector", "emi", "scenario", "ssp"))[3:length(getSets(mag))] - 2
      )
      return(mag)
    }
    innew <- fixDims(innew)
    wgtnew <- fixDims(wgtnew)

    # Mapping from GAINS2025 sectors to REMIND GAMS subsectors
    secmap <- toolGetMapping(type = "sectoral", name = "mappingGAINS2025toREMINDsectors.csv", where = "mrremind")

    # Drop sectors not in mapping
    dropsectors <- getItems(innew, "sector")[!(getItems(innew, "sector") %in% secmap$gains)]
    # Also drop sectors that are not mapped with sufficient detail. Those that can be
    # used will have a dot "." splitting the specific technologies
    dropsectors <- c(secmap$gains[!grepl("\\.", secmap$remind)], dropsectors)
    innew <- innew[, , dropsectors, invert = TRUE]
    wgtnew <- wgtnew[, , dropsectors, invert = TRUE]

    # Aggregate to REMIND sectors
    filtsecmap <- secmap[!(secmap$gains %in% dropsectors), ]
    outnew <- toolAggregate(innew, filtsecmap, weight = NULL, from = "gains", to = "remind", dim = "sector")
    wgtnew <- toolAggregate(wgtnew, filtsecmap, weight = NULL, from = "gains", to = "remind", dim = "sector")

    # Use abind to split the dimensions of specific technologies into subsectors
    splitTechs <- function(mag) {
      hasssp <- "ssp" %in% getSets(mag)
      mag <- as.magpie(abind::abind(clean_magpie(mag)), spatial = 1, temporal = 2)
      splitsectors <- c("region", "year", "sector1", "sector2", "sector3", "sector4", "emi", "scenario")
      if (hasssp) splitsectors <- c(splitsectors, "ssp")
      getSets(mag) <- splitsectors
      return(mag)
    }
    outnew <- splitTechs(outnew)
    wgtnew <- splitTechs(wgtnew)

    # Drop sectors not used in REMIND anymore
    outnew <- outnew[, , c("pcc", "pco"), invert = TRUE]
    wgtnew <- wgtnew[, , c("pcc", "pco"), invert = TRUE]
    # ==============================================================================================================
    # GAINSlegacy
    # ==============================================================================================================
    # GAINSlegacy does not actually follow its stated dimensions, so they are meaningless from this point on
    linold <- calcOutput(
      "EmissionFactors",
      subtype = "emission_factors", warnNA = FALSE, aggregate = FALSE, supplementary = TRUE
    )
    inold <- linold$x
    wgtold <- linold$weight

    # Split dimensions
    outold <- splitTechs(inold)
    wgtold <- splitTechs(wgtold)

    # Pad with SSP dimension
    nextdim <- as.numeric(paste0("3.", length(getSets(outold)) - 1))
    outold <- add_dimension(outold, nextdim, add = "ssp", nm = "GAINSlegacy")
    wgtold <- add_dimension(wgtold, nextdim, add = "ssp", nm = "GAINSlegacy")

    # GAINSlegacy weights were for a fixed year, expand the time dimension
    # so that it can be merged with the new data
    wgtold <- mbind(lapply(getYears(wgtnew), \(x) setYears(wgtold, x)))

    # ==============================================================================================================
    # Combining GAINSlegacy and GAINS2025
    # ==============================================================================================================
    out <- mbind(outold, outnew)
    # TODO: address weight sum warning
    wgt <- mbind(wgtold, wgtnew)
    desc <- getFromComment(innew, "description")
    unit <- getFromComment(innew, "unit")

  } else if (subtype == "emissions_exo_waste") {
    stop("Subtype not implemented yet.")
  } else if (subtype == "emissions_exo_landuse") {
    stop("Subtype not implemented yet.")
  } else {
    stop(paste0("Unknown subtype: ", subtype))
  }

  return(list(
    x = out,
    weight = wgt,
    unit = unit,
    description = desc
  ))
}
