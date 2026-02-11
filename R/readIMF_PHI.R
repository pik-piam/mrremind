# This function reads country-level financial efficiency data from the IMF
# and computes the PHI parameter by assigning the most efficient country
# a value of 0.87 and the least efficient country a value of 0.60, with all
# other countries mapped proportionally between these bounds. The PHI bounds
# are determined based on IMF data.

#' @author Diamantis Koutsandreas

readIMF_PHI <- function() {
  
  # ------------------------------------------------------------
  # Read raw country-level dataset
  # ------------------------------------------------------------
  raw <- readxl::read_xlsx(
    "Source_data_PHI.xlsx",
    sheet = "Country_PHI"
  )
  
  # ------------------------------------------------------------
  # Clean numeric columns
  # ------------------------------------------------------------
  raw_clean <- raw %>%
    dplyr::mutate(
      Efficiency = as.numeric(.data$Efficiency),
      GDP        = as.numeric(gsub(",", "", .data$GDP))
    )
  
  # ------------------------------------------------------------
  # Min–max normalization of Efficiency → Phi (country level)
  # ------------------------------------------------------------
  eff_min <- min(raw_clean$Efficiency, na.rm = TRUE)
  eff_max <- max(raw_clean$Efficiency, na.rm = TRUE)
  
  raw_phi <- raw_clean %>%
    dplyr::mutate(
      phi_country =
        0.60 +
        (.data$Efficiency - eff_min) *
        (0.87 - 0.60) /
        (eff_max - eff_min)
    )
  
  # ------------------------------------------------------------
  # GDP-weighted regional Phi
  # ------------------------------------------------------------
  phi_region <- raw_phi %>%
    dplyr::group_by(.data$Remind_Region) %>%
    dplyr::summarise(
      value =
        sum(.data$phi_country * .data$GDP, na.rm = TRUE) /
        sum(.data$GDP, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(region = .data$Remind_Region)
  
  # ------------------------------------------------------------
  # Convert to magpie object (no time dimension)
  # ------------------------------------------------------------
  return(as.magpie(phi_region))
}
