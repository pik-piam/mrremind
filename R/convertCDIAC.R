#' @importFrom madrat toolCountry2isocode toolISOhistorical toolAggregate

convertCDIAC <- function(x) {

  # Nations names to ISO country codes
  getRegions(x) <- toolCountry2isocode(getRegions(x))
  # delete XXX-Regions
  x <- x["XXX",,,invert=TRUE]  # FIXME better solution for XXX-regions 

  # CDIAC(XIT) -> ISO(ITA + SMR)
  # CDIAC(XFR) -> ISO(FRA + MCO)
  m <- matrix(c(c("XIT","XIT","XFR","XFR"),c("ITA","SMR","FRA","MCO")),4)
  w <- calcOutput("Population",years=2005,aggregate=FALSE)[c("ITA","SMR","FRA","MCO"),,"pop_SSP2"]
  x_split <- toolAggregate(x[c("XIT","XFR"),,],m,weight=w)

  # delete XIT and XFR from x
  x <- x[c("XIT","XFR"),,invert=TRUE]
  x <- mbind(x,x_split)
  
  # sort years
  x <- x[,sort(getYears(x)),]
  
  # split Germany before 2045
  x_DE <- x["DEU",,]
  getRegions(x_DE) <- "XDE"
  x_DE[getYears(x_DE,as.integer=TRUE)>=1945]   <- 0   # set to 0 for the time after 1945 
  x["DEU",,][getYears(x,as.integer=TRUE)<1945] <- 0   # set to 0 for the time before 1945 
  x <- mbind(x,x_DE)
  
  # FIXME: ANT and XAN exist in CDIAC -> has to be distributed properly (for a starting point see ISOhistorical.csv)
  
  x[is.na(x)]<-0
  x <- toolISOhistorical(x,overwrite=TRUE)
  x <- toolCountryFill(x,fill=0)
  getSets(x) <- getSets(new.magpie())

  return(x)
}  