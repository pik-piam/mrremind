calcFossilPolyRent <- function() {
  x <- readSource("REMIND_11Regi", subtype = "ffPolyRent")

  # use pe as weigth for aggregation
  w <- new.magpie(getRegions(x), getYears(x), getNames(x))
  oil <- readSource("BGR", subtype = "oil")[, , "Remaining_Potential"]
  getNames(oil) <- "peoil"
  gas <- readSource("BGR", subtype = "gas")[, , "Remaining_Potential"]
  getNames(gas) <- "pegas"
  coal <- readSource("BGR", subtype = "coal")[, , "Remaining_Potential"]
  getNames(coal) <- "pecoal"
  # put the data for all fossils together
  bgr <- mbind(oil, coal, gas)
  bgr[is.na(bgr)] <- 0
  for (s in getNames(x, dim = 1)) {
    w[, , s] <- bgr[, , s]
  }

  return(list(
    x = x,
    weight = w,
    unit = "none",
    description = "vintages, installed capacities"
  ))
}
