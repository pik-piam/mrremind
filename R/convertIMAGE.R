convertIMAGE <- function(x) {
  w <- readSource("EDGAR",subtype="HFC")
  w[is.na(w)] <- 0
  w <- dimSums(w[,2005,],dim=3)["ATA",,,invert=TRUE]
  y <- toolAggregate(x, "regionmappingIMAGE.csv", weight=w)
  y <- toolCountryFill(y,fill=0)
  return(y)
}