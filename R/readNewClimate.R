#' Reads NPI policy database with technology capacity target from the Policy data base (v4 August 2024)
#' by PBL that translate the high impact policies of https://climatepolicydatabase.org/.

#' @description Reads excel sheet with NPi (National Policies Implemented)
#' data on different policy targets (capacity) with different variations:
#' unconditional for minimal and conditional for maximum targets.
#' NPI targets only include targets that are based on implemented policy instruments.

#' @author  Rahel Mandaroux, Léa Hayez, Falk Benke
#' @param subtype Capacity_YYYY_cond or Capacity_YYYY_uncond for Capacity Targets, Emissions_YYYY_cond or
#'   Emissions_YYYY_uncond for Emissions targets, with YYYY NDC version year,
#'   determines the database version to be read in
#' @param subset A string (or vector of strings) designating the scenario(s) to be returned.
#'
readNewClimate <- function(subtype, subset) {

  NPIfile <- dplyr::case_when(
    grepl("2025", subtype, fixed = TRUE) ~ "NPi_2025-03-25.xlsx",
    .default = "NPi_2025-03-25.xlsx"
  )

  if (grepl("Capacity", subtype, fixed = TRUE)) {
    # Capacity/Additional Capacity targets are in GW.
    NPI <- read_excel(
      NPIfile,
      sheet = "Capacity_target_PBL_2025",
      col_types = c(
        "text", "skip", "numeric", "text", "text", "numeric",
        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
        "numeric", "numeric", "numeric", "skip", "skip", "skip", "skip"
      )
    )
    x <- as.magpie(NPI, spatial = 1, temporal = 2, datacol = 3)
  } else if (grepl("Emissions", subtype, fixed = TRUE)) {

    input <- readxl::read_excel(
      NPIfile,
      sheet = "EmissionTargets", skip = 3, na = c("?", ""), progress = FALSE
    ) %>%
      suppressMessages() %>%
      select(
        "ISO_Code" = 2, "Reference_Year" = 7,
        "BAU_or_Reference_emissions_in_MtCO2e" = 8, "Target_Year" = 9,
        "Type" = 10, "Unconditional Absolute" = 11, "Conditional Absolute" = 12,
        "Unconditional Relative" = 13, "Conditional Relative" = 14
      ) %>%
      toolProcessClimateTargetDatabase(database = "NewClimate", subtype = subtype)

    x <- as.magpie(input, spatial = "ISO_Code", temporal = "Target_Year")
  } else {
    stop("Incorrect subtype, please use Capacity_YYYY_cond or Emissions_YYYY_cond (or uncond).")
  }
  return(x)
}
