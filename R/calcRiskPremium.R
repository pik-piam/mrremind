calcRiskPremium <- function() {
  # Read risk class data from OECD
  x <- readSource("OECD")

  # Convert into percent
  x <- x / 100

  # Delete dimensions that are same and not in the GAMS-code
  getNames(x) <- NULL
  getYears(x) <- NULL

  # Use GDP in 2005 as weight
  w <- calcOutput("GDP", scenario = "SSP2", aggregate = FALSE)[, 2005, ]

  list(x           = x,
       weight      = w,
       unit        = "Dimensionless",
       description = "Risk premium that lowers the use of capital imports")
}
