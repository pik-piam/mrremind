calcEmissionsForScalingECLIPSE <- function() {

  a <- calcOutput(type="Emissions",datasource="CEDS16",aggregate = FALSE)
  
  # for the scaling only 2005 data is needed
  a <- a[,"y2005",]
  # remove emissions we don't need for scaling
  a <- a[,,c("CH4","N2O","CO2","NH3"), invert = TRUE]
  
  map_CEDS16_to_CEDS9  <- read.csv(toolMappingFile("sectoral", "mappingCEDS16toCEDS9.csv"), stringsAsFactors=FALSE)

  # remove sectors from mapping that are not in the data because otherwise toolAggregate complains
  remove_sector <- setdiff(map_CEDS16_to_CEDS9$CEDS16,getNames(a,dim=1))
  if (length(remove_sector)>0) {
    row_remove <- which(map_CEDS16_to_CEDS9$CEDS16 %in% remove_sector)
    map_CEDS16_to_CEDS9 <- map_CEDS16_to_CEDS9[-row_remove,] 
    cat("Removed",remove_sector,"from sector mapping map_CEDS16_to_CEDS9.\n")
  }

  a <- toolAggregate(x=a,weight = NULL, dim=3.1, rel = map_CEDS16_to_CEDS9, from="CEDS16",to="CEDS9")
  
  # define shortnames for sectors
  shortnames <- c("Energy Sector" = "power", "Industrial Sector" = "indst", "Residential Commercial Other" = "res", "Transportation Sector" = "trans")
  # choose only sectors that are relevant for the scaling
  a <- a[,,names(shortnames)]
  # rename to shortnames
  getNames(a,dim=1) <- shortnames[getNames(a,dim=1)]
  # change to lower case and reverse order (for exporting) power.SO2 -> SO2.power
  getNames(a) <- tolower(gsub("^([^\\.]*)\\.([^\\.]*)(.*$)","\\2.\\1",getNames(a)))
  
  
  return(list(x=a,
              weight=NULL,
              unit="Mt",
              description="historic emissions in 2005"))
  
  }