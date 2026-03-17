#' calc capacity bounds
#'
#' This function gathers project pipeline data for different technologies to generate REMIND input data
#' for setting historical and near-term bounds. The output is technology capacity in three different categories:
#' "operational": installed capacity that is operational in the respective year
#' "construction": capacity that is currently under construction but expected to be completed in the respective year
#' "planned": capacity that is planned to be completed by this year but not yet under construction. The exact definition of this category
#' (e.g. whether only projects with FID are included)may vary by technology and is documented in calcProjectPipelines.
#' Moreover, the function provides project pipeline data for the historical mif file.
#'
#' @author Felix Schreyer
#'
#' @param subtype either `historical` for data until 2024 or `pipeline`
#' for projections in 2020, 2025 and 2030 (including some redistribution on EU/NEU level)
#'
calcCapacityBounds <- function(subtype) {
  # 1. Historical and Near-term Projects used for REMIND Bounds ----
  if (subtype == "pipeline") {
    ## 1.1 CO2 Storage Projects ----

    ccs <- calcOutput("ProjectPipelines",
      subtype = "CCS",
      aggregate = FALSE, warnNA = FALSE
    )

    # convert to REMIND native units (from MtCO2 to GtC)
    ccs <- ccs / 3.67 / 1000

    # map to REMIND technology
    x <- add_dimension(collapseDim(ccs), dim = 3.1, add = "technology", nm = "ccsinjeon")


    ## 1.2 Coal Power Projects ----
    coal <- calcOutput("ProjectPipelines",
      subtype = "coal",
      aggregate = FALSE, warnNA = FALSE
    )

    # since GEM coal project data is incomplete and misses small coal power plants,
    # do not used operational coal project data and set to 0
    coal[, , "operational"] <- 0
    # convert to REMIND native units (TW)
    coal <- coal / 1000

    # map to REMIND technology
    x <- mbind(
      x,
      add_dimension(collapseDim(coal), dim = 3.1, add = "technology", nm = "pc")
    )


    ## 1.3 Prepare Data ----

    # used as input-data for bounds
    x <- x[, c(2020, 2025, 2030), c("operational", "construction", "planned")]
    # remove "model", "variable" and "unit" dimension
    x <- collapseDim(x, keepdim = c("technology", "status"))
  }


  # 2. Historical and Near-Term Projects used for Historical Mif File ----

  ## 2.1 CO2 Storage Projects ----
  if (subtype == "historical") {
    # project pipeline snapshot from April 2025
    x <- readSource("IEA_CCUS", subtype = "historical")
  }

  return(list(
    x = x,
    weight = NULL,
    unit = "  TW(output) for energy conversion technologies,
              GtC/yr for carbon management technologies",
    description = "Historical and near-term capacities for REMIND technologies depending on project status"
  ))
}
