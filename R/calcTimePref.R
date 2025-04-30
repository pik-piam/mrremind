calcTimePref <- function() {

  x <- readSource("ExpertGuess", subtype = "prtp")
  getNames(x) <- NULL
  getYears(x) <- NULL

  # Use half pop and half gdp for weight
  pop <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE)[, 2005, ]
  gdp <- calcOutput("GDP", scenario = "SSP2", aggregate = FALSE)[, 2005, ]
  w <- pop / dimSums(pop, dim = 1) + gdp / dimSums(gdp, dim = 1)

  list(x = x, weight = w, unit = "dimensionless", description = "pure rate of time preference")
}
