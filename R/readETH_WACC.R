#' This function reads WACC markup data across technologies for each of the REMIND regions
#library(dplyr)
# library(tidyr)
# library(mrcommons)
# library(mrremind)
# # 
# setwd("C:/Users/adamanti//madratsources/ETH_WACC")

#' @author Diamantis Koutsandreas
# readETH_WACC <- function() {
#   data <- readxl::read_xlsx("Source_data_WACC.xlsx", sheet = "wacc_markups_coal", range = "A1:V13") %>%
#     pivot_longer(cols = 2:10, names_to = "technology", values_to = "value")
#   return(as.magpie(data))
# }

readETH_WACC <- function() {
  # Define the year vector
  years <- c(2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050,
             2055, 2060, 2070, 2080, 2090, 2100, 2110, 2130, 2150)
  
  # Read and pivot the WACC data
  data <- readxl::read_xlsx("Source_data_WACC.xlsx", 
                            sheet = "wacc_markups_b", 
                            range = "A1:CI13") %>%
    pivot_longer(cols = 2:86, names_to = "tewacc", values_to = "value") %>%
    rename(reg = 1) # Rename the region column to 'reg'
  
  # Repeat each region-technology pair for each year
  data_expanded <- data %>%
    slice(rep(1:n(), each = length(years))) %>%
    mutate(t = rep(years, times = nrow(data)))
  
  # Reorder columns to: t, reg, tewacc, value
  data_expanded <- data_expanded %>%
    select(t, reg, tewacc, value)
  
  # Convert to magpie object
  return(as.magpie(data_expanded))
}

# Assuming your dataframe is named 'df'
# write.csv(data_expanded, "C:/Users/adamanti/madrat/data.csv", row.names = FALSE)

  



  
