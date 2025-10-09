calcCostOfCapital <- function() {
  output <- readSource("ETH_WACC")
  return(
    list(
      x = output, unit = "%",
      description = "add some descriptive text here",
      isocountries = FALSE
    )
  )
}
