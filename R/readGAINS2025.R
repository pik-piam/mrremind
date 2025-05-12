#' Read air pollution emissions, activities and emission factors from GAINS data
#'
#' @return Activity levels, emissions or emission factors
#' @author Gabriel Abrahao
#' @param subtype "emission_factors", "emissions","emissions_starting_values"
#'
#' @importFrom magclass as.magpie
#' @importFrom tidyr pivot_longer drop_na
readGAINS2025 <- function(subtype, subset = "baseline.det") {

  # Interpreting subtype as the codes used in the database
  subtypecode <- ifelse(subtype == "emissions", "EMISSION", "ACTIVITY")

  scenario <- strsplit(subset, "\\.")[[1]][1]
  agglevel <- strsplit(subset, "\\.")[[1]][2]  

  if (scenario == "baseline") {
    if (subtype == "emifacs") {
      stop("Only emission and activities are available for the historical baseline")
    }
    # Reading baseline scenario activities and emissions
    inbaseactemi <- read.csv(paste0("IMAGE_emf_", agglevel, "_activity_emission_2025-03-25.csv"))

    # Drop scenario dimension as we only have the baseline in the file
    inbaseactemi <- inbaseactemi[, -1]
    longbaseactemi <- pivot_longer(
      inbaseactemi, 5:length(names(inbaseactemi)),
      names_prefix = "X", names_to = "year"
    )
    head(longbaseactemi)
    names(longbaseactemi) <- c("region", "sectorGAINS", "species", "vartype", "year", "value")
    baseactemi <- as.magpie(longbaseactemi, spatial = "region", temporal = "year")

    out <- collapseDim(baseactemi[, , subtypecode], "vartype")
    comment(out) <- paste0("GAINS2025 ", subtype, " for scenario ", scenario, " at aggregation level ", agglevel)
  } else if (scenario %in% c("cle", "middle", "mtfr")) {
    if (subtype != "emifacs") {
      stop("Only emission factors are available for scenarios other than the historical baseline")
    }
    # Reading scenario emission factors. Here there is also a SSP dimension
    inefs <- read.csv(paste0("SSPs_IMAGE_emf_", agglevel, "_", scenario, "_rev_2025-03-25.csv"))

    # Convert to long format 
    longefs <- pivot_longer(inefs, 5:length(names(inefs)), names_prefix = "X", names_to = "year")

    names(longefs) <- c("ssp", "region", "sectorGAINS", "species", "year", "value")
    longefs$scenario <- scenario
    longefs <- longefs[,c(7, 1:6)]

    out <- as.magpie(longefs, spatial = "region", temporal = "year")
    comment(out) <- paste0("GAINS2025 emission factors for scenario ", scenario, " and all SSPs at aggregation level ", agglevel)
  }

  return(out)
}


