#' Reads NDC policy database with capacity, emission, and share targets, originally based on Rogelj et al. 2017
#'
#' @description Reads excel sheet with NDC (Nationally Determined Contributions)
#'  data on different policy targets (capacity, emission, and share targets) with different variations.
#'
#' @author Aman Malik, Christoph Bertram, Oliver Richters, Sophie Fuchs, Rahel Mandaroux
#' @param subtype Capacity_YYYY_cond or Capacity_YYYY_uncond for Capacity Targets, Emissions_YYYY_cond or
#'   Emissions_YYYY_uncond for Emissions targets, with YYYY NDC version year,
#'   determines the database version to be read in
#' @param subset A string (or vector of strings) designating the scenario(s) to be returned.
#'
readUNFCCC_NDC <- function(subtype, subset) {

  NDCfile <- dplyr::case_when(
    grepl("2018", subtype, fixed = TRUE) ~ "NDC_2018.xlsx",
    grepl("2021", subtype, fixed = TRUE) ~ "NDC_2021.xlsx",
    grepl("2022", subtype, fixed = TRUE) ~ "NDC_2022-12-31.xlsx",
    grepl("2023", subtype, fixed = TRUE) ~ "NDC_2023-11-29.xlsx",
    grepl("2024", subtype, fixed = TRUE) ~ "NDC_2024-08-31.xlsx",
    .default = "NDC_2024-08-31.xlsx"
  )

  if (grepl("Capacity", subtype, fixed = TRUE)) {
    NDC <- readxl::read_excel(
      NDCfile,
      sheet = "Capacity_target",
      col_types = c(
        "text", "skip", "numeric", "text", "text", "numeric",
        "numeric", "numeric", "numeric", "numeric", "skip", "skip", "skip"
      )
    )
    x <- as.magpie(NDC, spatial = 1, temporal = 2, datacol = 3)
    return(x)

  } else if (grepl("Emissions", subtype, fixed = TRUE)) {

    input <- readxl::read_excel(
      NDCfile, sheet = "Emissions", skip = 3, na = c("?", ""), progress = FALSE) %>%
      suppressMessages() %>%
      select(
        "ISO_Code" = 2, "Reference_Year" = 7,
        "BAU_or_Reference_emissions_in_MtCO2e" = 8, "Target_Year" = 9,
        "Type" = 10, "Unconditional Absolute" = 11, "Conditional Absolute" = 12,
        "Unconditional Relative" = 13, "Conditional Relative" = 14
      ) %>%
      toolProcessClimateTargetDatabase(database = "UNFCCC_NDC", subtype = subtype)

    x <- as.magpie(input, spatial = "ISO_Code", temporal = "Target_Year")

    return(x)
  } else {
    stop("Incorrect subtype, please use Capacity_YYYY_cond or Emissions_YYYY_cond (or uncond).")
  }
}

