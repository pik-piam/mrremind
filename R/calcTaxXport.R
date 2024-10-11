calcTaxXport <- function() {
  "!# @monitor GDPuc::convertGDP"

  x <- readSource("REMIND_11Regi", subtype = "xpres_tax")

  # average weight
  weight <- x
  weight[, , ] <- 1

  # convert data from $2005 to $2017
  x <- GDPuc::convertGDP(
    gdp = x,
    unit_in = "constant 2005 US$MER",
    unit_out = mrdrivers::toolGetUnitDollar(),
    replace_NAs = "with_USA"
  )

  x <- time_interpolate(x, c(seq(2010, 2150, 5)),
                        extrapolation_type = "constant",
                        integrate_interpolated_years = TRUE)

  return(list(
    x = x,
    weight = weight,
    unit = "US$2017/GJ",
    description = "resource export taxes, not used in default settings."
  ))
}
