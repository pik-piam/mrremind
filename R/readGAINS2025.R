#' Read air pollution emissions, activities and emission factors from GAINS data
#'
#' There's no associated convert function, as the disaggregation takes
#' a combination of subtypes, and it is easier to carry out most
#' calculations at the GAINS regional level first and then disaggregate
#' the results
#'
#' @return Activity levels, emissions or emission factors. Alternatively,
#' a Government Capacity Index (GCI) used for deriving some scenario
#' extensions.
#' @author Gabriel Abrahao
#' @param subtype "emifacs", "emissions","activities", "GCI"
#' @param subset scenario and aggregation level ("agg" or "det"), separated by a dot
#'
readGAINS2025 <- function(subtype, subset = "baseline.det") {
  # Interpreting subtype as the codes used in the database
  subtypecode <- ifelse(subtype == "emissions", "EMISSION", "ACTIVITY")

  scenario <- strsplit(subset, "\\.")[[1]][1]
  agglevel <- strsplit(subset, "\\.")[[1]][2]

  if (subtype %in% c("emifacs", "emissions", "activities")) {
    if (scenario == "baseline") {
      if (subtype == "emifacs") {
        # Reading emission factors for the ScenarioMIP combinations
        # GA: This if condition is temporary, until we get a file for the detailed aggregation level
        # for the FINAL_2025-07-01 version. But the other version is compatible with it, so this ultimately
        # only means that Municipal Waste EFs are read from the old version in calcGAINS2025scenarios
        inefs <- read.csv(paste0("emission_factors_", agglevel, "_hist_final_2025-07-02.csv"))

        # Convert to long format
        longefs <- pivot_longer(inefs, 7:length(names(inefs)), names_prefix = "X", names_to = "year")
        longefs <- longefs[, -1]

        names(longefs) <- c("ssp", "scenario", "region", "sectorGAINS", "species", "year", "value")

        out <- as.magpie(longefs, spatial = "region", temporal = "year")
        comment(out) <- paste0(
          "GAINS2025 emission factors for ScenarioMIP scenarios and selected SSPs at aggregation level ", agglevel
        )
      } else {
        # Reading baseline scenario activities and emissions
        inbaseactemi <- read.csv(paste0("IMAGE_emf_", agglevel, "_activity_emission_2025-07-02.csv"))

        # Drop scenario dimension as we only have the baseline in the file
        inbaseactemi <- inbaseactemi[, -1]
        longbaseactemi <- pivot_longer(
          inbaseactemi, 5:length(names(inbaseactemi)),
          names_prefix = "X", names_to = "year"
        )

        names(longbaseactemi) <- c("region", "sectorGAINS", "species", "vartype", "year", "value")
        baseactemi <- as.magpie(longbaseactemi, spatial = "region", temporal = "year")

        out <- collapseDim(baseactemi[, , subtypecode], "vartype")
        comment(out) <- paste0("GAINS2025 ", subtype, " for scenario ", scenario, " at aggregation level ", agglevel)
      }
    } else if (!(scenario %in% c("baseline", "scenariomip"))) {
      if (subtype != "emifacs") {
        stop("Only emission factors are available for scenarios other than the historical baseline")
      }
      # Reading scenario emission factors. Here there is also a SSP dimension
      inefs <- read.csv(paste0("SSPs_IMAGE_emf_", agglevel, "_", scenario, "_2025-07-02.csv"))

      # Convert to long format
      longefs <- tidyr::pivot_longer(inefs, 5:length(names(inefs)), names_prefix = "X", names_to = "year")

      names(longefs) <- c("ssp", "region", "sectorGAINS", "species", "year", "value")
      longefs$scenario <- scenario
      longefs <- longefs[, c(7, 1:6)]

      out <- as.magpie(longefs, spatial = "region", temporal = "year")
      comment(out) <- paste0(
        "GAINS2025 emission factors for scenario ", scenario, " and all SSPs at aggregation level ", agglevel
      )
    } else if (scenario == "scenariomip") {
      if (subtype != "emifacs") {
        stop("Only emission factors are available for scenarios other than the historical baseline")
      }
      # Reading emission factors for the ScenarioMIP combinations
      # GA: This if condition is temporary, until we get a file for the detailed aggregation level
      # for the FINAL_2025-07-01 version. But the other version is compatible with it, so this ultimately
      # only means that Municipal Waste EFs are read from the old version in calcGAINS2025scenarios
      inefs <- read.csv(paste0("emission_factors_", agglevel, "_ssp_variant_final_2025-07-02.csv"))

      # Convert to long format
      longefs <- pivot_longer(inefs, 7:length(names(inefs)), names_prefix = "X", names_to = "year")
      longefs <- longefs[, -1]

      names(longefs) <- c("ssp", "scenario", "region", "sectorGAINS", "species", "year", "value")

      out <- as.magpie(longefs, spatial = "region", temporal = "year")
      comment(out) <- paste0(
        "GAINS2025 emission factors for ScenarioMIP scenarios and selected SSPs at aggregation level ", agglevel
      )
    }
  } else if (subtype == "GCI") {
    # GCI data for each SSP, Contains NAs
    ingci <- read.csv("SSP_GI_2024_proj.csv")
    longgci <- ingci[, c("scen", "iso3c", "year", "st_cap_ha")]
    names(longgci) <- c("ssp", "country", "year", "value")
    out <- as.magpie(longgci, spatial = "country", temporal = "year")
  } else if (subtype == "sectorlist") {
    insec <- readxl::read_excel("EMF_sector_Unit_2025.xlsx")
    out <- list(
      x = as.data.frame(insec[!is.na(insec[, 1]), ]),
      class = "data.frame"
    )
  } else {
    stop(paste0("Unknown subtype: ", subtype))
  }
  return(out)
}
