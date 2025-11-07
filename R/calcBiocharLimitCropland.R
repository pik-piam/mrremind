#' @title calcBiocharLimitCropland
#' @description Provides back-of-envelope upper limits of biochar application on cropland.
#'
#' Estimates upper limits as either cumulative stock capacity (t) over a
#' compliance window, or as annual flow ceiling (t/yr) for cropland soils,
#' using physical cropland area, adoption shares, and application-rate limits per area.
#'
#' @param dataBCLimit Character scalar selecting the application-rate limit source.
#' Options:
#' "CRCF_draft_2025": cumulative application limit of 50 t/ha over any 10-year period.
#' "Conservative": annual application rate of 1 t/ha/yr (illustrative, non-regulatory)
#' @param adoptShare Adoption share of cropland treated with biochar.
#' @param refYear Character scalar selecting the reference year (e.g. "y2015").
#' @param annualLimit Logical. If TRUE: compute annual flow ceiling (t/yr).
#'              If FALSE: compute cumulative stock capacity (t) for compliance window.
#'              Note: If data source contains an annual rate, the compliance window
#'              is interpreted as 1 year (only annual limits make semantic sense here).
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#'
#' @author Isabelle Weindl
#' @seealso
#' [mrlandcore::calcCroparea()]
#' @examples
#' \dontrun{
#' calcOutput("BiocharLimitCropland",
#'            dataBCLimit = "CRCF_draft_2025",
#'            adoptShare = 1,
#'            refYear = "y2020",
#'            annualLimit = TRUE)
#' }

calcBiocharLimitCropland <- function(dataBCLimit = "CRCF_draft_2025",
                                     adoptShare = 1,
                                     refYear = "y2020",
                                     annualLimit = TRUE) {

  ## Mapping and specification of application limit source:
  limitMap <- list(
    "CRCF_draft_2025" = list(value = 50, window = 10), # 50 t/ha over any 10y period
    # Note: cumulative application limit per area
    # Source: European Commission. Joint Stakeholder Proposal — Biochar Carbon Removal
    # in the CRCF (Draft), March 2025.

    "Conservative" = list(value = 1, window = 1)    # 1 t/ha/yr (illustrative)
    # Note: annual application limit per area (--> compliance window is 1 year)
    # Source note: illustrative operational rate (non-regulatory).
  )

  if (!dataBCLimit %in% names(limitMap))
    stop("Unknown 'dataBCLimit'. Choose one of: ",
         paste(names(limitMap), collapse = ", "))

  limitSpec <- limitMap[[dataBCLimit]]


  ## Check adoption share:
  if (!is.finite(adoptShare) || adoptShare < 0 || adoptShare > 1) {
    stop("'adoptShare' must be a single finite value within [0,1].")
  }


  ## Cropland data input:
  cropArea <- calcOutput("Croparea", sectoral = "kcr", physical = TRUE, aggregate = FALSE)
  cropLand <- dimSums(cropArea, dim = 3)

  if (refYear %in% getYears(cropLand)) {
    refCropL <- cropLand[, refYear, ]
  } else {
    stop("Selected year not available.")
  }


  ## Calculate limits:
  if (!annualLimit && limitSpec$window == 1) {
    warning("Selected source is an annual rate (window=1) but 'annualLimit=FALSE'. ",
            "Returning a 1-year cumulative number numerically equal to the annual rate.")
  }

  if (annualLimit) {
    annualAreaLimit <- limitSpec$value / limitSpec$window
    massLimit <- refCropL * adoptShare * annualAreaLimit
  } else {
    cumulativeAreaLimit <- limitSpec$value
    massLimit <- refCropL * adoptShare * cumulativeAreaLimit
  }


  ## Units: Mt for cumulative; Mt/yr for annual limit
  if (annualLimit) {
    unit <- "Mt/yr"
  } else {
    unit <- paste0("Mt over ", limitSpec$window, " years")
  }


  ## Description:
  description <- paste(
    "Upper limit of biochar application on cropland.",
    "Default case ('dataBCLimit = \"CRCF_draft_2025\"') applies a cumulative limit",
    "of 50 t/ha over any 10-year period, following the EC CRCF biochar draft (2025).",
    "This cumulative limit is translated to a constant annual limit."
  )


  return(list(
    x = massLimit,
    weight = NULL,
    unit = unit,
    description = description
  ))
}
