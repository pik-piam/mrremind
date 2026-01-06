#' Reads NPI policy database with technology capacity target from the Policy data base (v4 August 2024)
#' by PBL that translate the high impact policies of https://climatepolicydatabase.org/.

#' @description Reads excel sheet with NPi (National Policies Implemented)
#' data on different policy targets (capacity, production, emissions) with different variations.
#' NPI targets only include targets that are based on implemented policy instruments.

#' @author Rahel Mandaroux, Léa Hayez, Falk Benke
#' @param subtype Capacity_YYYY_cond or Capacity_YYYY_uncond for Capacity Targets, Emissions_YYYY_cond or
#'   Emissions_YYYY_uncond for Emissions targets, RenShareTargets for renewable energy share targets,
#'   with YYYY NDC version year, determines the database version to be read in
#' @param subset A string (or vector of strings) designating the scenario(s) to be returned (only used in convert).
#'
readNewClimate <- function(subtype, subset) {
  NPIfile <- dplyr::case_when(
    grepl("2025", subtype, fixed = TRUE) ~ "C:/Users/leaha/Documents/Coding/NPi_2025-12-11.xlsx",
    .default = "NPi_2025-12-11.xlsx"
  )

  if (grepl("Capacity", subtype, fixed = TRUE)) {

    data <- read_excel(
      NPIfile,
      sheet = "Capacity_target_PBL_2025",
      col_types = c(
        "text", "skip", "numeric", "text", "text", "numeric",
        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
        "numeric", "numeric", "numeric", "numeric", "skip", "skip", "skip"
      )
    )

    targetTypes <- c("AC-Absolute", "Production-Absolute", "TIC-Absolute", "FE-Production-Share")

    if (!all(unique(data$`Type of target`) %in% targetTypes)) {
      stop(
        subtype, ": Table read from NewClimate contains unknown target types: ",
        unique(data$`Type of target`)[which(!(unique(data$`Type of target`) %in% targetTypes))]
      )
    }

    x <- as.magpie(data, spatial = 1, temporal = 2, datacol = 3)

  } else if (grepl("Emissions", subtype, fixed = TRUE)) {

    input <- readxl::read_excel(
      NPIfile,
      sheet = "EmissionTargets", skip = 3, na = c("?", ""), progress = FALSE
    ) %>%
      suppressMessages() %>%
      select(
        "ISO_Code" = 2, "Reference_Year" = 7,
        "BAU_or_Reference_emissions_in_MtCO2e" = 8, "Target_Year" = 9,
        "Type" = 10, "Unconditional Relative" = 11, "Conditional Relative" = 12,
        "Unconditional Absolute" = 13, "Conditional Absolute" = 14
      ) %>%
      toolProcessClimateTargetDatabase(database = "NewClimate", subtype = subtype)

    x <- as.magpie(input, spatial = "ISO_Code", temporal = "Target_Year")
    # read in energy share targets from policy modeling protocol
  } else if (grepl("RenShareTargets", subtype, fixed = TRUE)) {
    data <- readxl::read_excel(
      NPIfile,
      sheet = "EnergyShareTargets",
      col_names = TRUE
    )

    # filter only for energy share targets (ShareTarget column is 1)
    # and columns relevant for target implementation:
    # country, target year, target type (e.g. share in electricity or final energy),
    # MinMax (whether minimum or maximum value, values are different only for targets with range),
    # value
    data <- data %>%
      filter(.data$ShareTarget == 1) %>%
      select("ISO-3", "Target Year", "TargetType", "Model Target Value Min", "Model Target Value Max") %>%
      pivot_longer(
        cols = dplyr::starts_with("Model Target Value"),
        names_to = "MinMax",
        values_to = "value"
      ) %>%
      mutate("MinMax" = gsub("Model Target Value ", "", .data$MinMax))

    x <- as.magpie(data, spatial = "ISO-3", temporal = "Target Year")
  } else {
    stop("Incorrect subtype, please use Capacity_YYYY_cond or Emissions_YYYY_cond (or uncond).")
  }
  return(x)
}
