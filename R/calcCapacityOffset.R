calcCapacityOffset <- function() {
  x <- readSource("REMIND_11Regi", subtype = "deltacapoffset")
  getYears(x) <- "y2010"

  list(x = x,
       weight = NULL,
       unit = "TW",
       description = "Global offset of 200MW multiplied with the regional share of PE2SE capacities")
}
