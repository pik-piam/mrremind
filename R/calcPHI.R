calcCostOfCapital <- function() {
  
  output <- readSource("ETH_WACC")
  
  # ------------------------------------------------------------
  # Manual overrides for specific regions
  # ------------------------------------------------------------
  # Convert to data.frame for safe manipulation
  df <- as.data.frame(output)
  
  df <- df %>%
    mutate(
      value = case_when(
#values for India are not available in IMF, and are taken from here instead: https://en.macromicro.me/charts/33445/india-bank-loan-minus-deposit-spread 
        reg == "IND" ~ 3.01,
#values for USA are not available in IMF, and are set equal to those of Canada 
        reg == "USA" ~ 2.62,
        TRUE         ~ value
      )
    )
  
  # Convert back to magpie object
  output_adj <- as.magpie(df)
  
  return(
    list(
      x = output_adj,
      unit = "%",
      description = paste(
        "share of investments in each region that are not transformed into productive capital",
      ),
      isocountries = FALSE
    )
  )
}
