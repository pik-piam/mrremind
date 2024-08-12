calcMacroInvestments <- function() {

  # Read in investment share and SSP2 GDP in 2005
  shInv <- readSource("PWT")[, 2005, "csh_i"]
  gdp <- calcOutput("GDP", scenario = "SSP2", years = 2005, aggregate = FALSE)

  shInv_new <- toolFillWithRegionAvg(shInv, valueToReplace = 0, weight = gdp)

  # Calculate macro investments
  data <- shInv_new * gdp

  # Convert from million to trillion
  data <- data * 1e-6

  getYears(data) <- NULL
  getNames(data) <- NULL

  return(list(
    x = data,
    weight = NULL,
    unit = "trillion US$2017",
    description = "Investments in the macro-economic capital stock in constant 2017 US$MER"
  ))
}
