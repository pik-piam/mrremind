#' @importFrom dplyr %>%

calcRegion2MAGICC <- function() {
  
  # read in data used as weight
  w   <- readSource("EDGAR",subtype="SO2")
  # sum over all types
  w <- dimSums(w,dim=3)
  getSets(w)[1] <- "region"
  
  # read in regional mapping
  map  <- toolGetMapping(type = "regional", name = "regionmappingRCP.csv", where = "mappingfolder")
  map$RegionCode <- paste0("R5",map$RegionCode)
  
  # make magpie-objects in the right dimension
  x <- new.magpie(getRegions(w),getYears(w),unique(map$RegionCode),fill=0)
  for(i in 1:length(map$CountryCode)) {
    x[map$CountryCode[i],,map$RegionCode[i]] <- 1
  }
  
  getYears(x) <- NULL
  
  return(list(x=x,weight=w,unit="Percent",
              description="percentage of REMIND-region that is allocated to R5-Regions, weighted by EDGAR-SO2-data"))
}
