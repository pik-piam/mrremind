# This function reads country-level financial efficiency data from the IMF
# and computes the PHI parameter by assigning the most efficient country
# a value of 0.87 and the least efficient country a value of 0.60, with all
# other countries mapped proportionally between these bounds. The PHI bounds
# are determined based on IMF data.
# setwd("C:/Users/adamanti/madrat/sources/IMF_PHI")

#' @author Diamantis Koutsandreas

readIMF_PHI <- function() {
  
  library(dplyr)
  library(readxl)
  
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
    mutate(
      Efficiency = as.numeric(Efficiency),
      GDP        = as.numeric(gsub(",", "", GDP))
    ) %>%
    filter(
      !is.na(Remind_Region),
      !is.na(Efficiency),
      !is.na(GDP)
    )
  
  # ------------------------------------------------------------
  # Min–max normalization of Efficiency → Phi (country level)
  # ------------------------------------------------------------
  eff_min <- min(raw_clean$Efficiency, na.rm = TRUE)
  eff_max <- max(raw_clean$Efficiency, na.rm = TRUE)
  
  raw_phi <- raw_clean %>%
    mutate(
      phi_country =
        0.60 +
        (Efficiency - eff_min) *
        (0.87 - 0.60) /
        (eff_max - eff_min)
    )
  
  # ------------------------------------------------------------
  # GDP-weighted regional Phi
  # ------------------------------------------------------------
  phi_region <- raw_phi %>%
    group_by(Remind_Region) %>%
    summarise(
      value =
        sum(phi_country * GDP, na.rm = TRUE) /
        sum(GDP, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(region = Remind_Region)
  
  # ------------------------------------------------------------
  # Convert to magpie object (no time dimension)
  # ------------------------------------------------------------
  return(as.magpie(phi_region))
}
