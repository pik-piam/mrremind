convertMAgPIE <- function(x, subtype) {
  
  if (subtype == "EmiAirPoll") {
    
    # read CEDS emissions data from sources (only airpollutants)
    bc    <- readSource("CEDS",subtype="BC")
    ch4   <- readSource("CEDS",subtype="CH4")
    co    <- readSource("CEDS",subtype="CO")
    nh3   <- readSource("CEDS",subtype="NH3")
    nox   <- readSource("CEDS",subtype="NOx")
    nmvoc <- readSource("CEDS",subtype="NMVOC")
    oc    <- readSource("CEDS",subtype="OC")
    so2   <- readSource("CEDS",subtype="SO2")
    
    y <- "y2005"
    ap <- mbind(bc[,y,],ch4[,y,],co[,y,],nh3[,y,],nox[,y,],nmvoc[,y,],oc[,y,],so2[,y,])
    
    # convert to sectoral mapping
    map  <- toolMappingFile("sectoral", "mappingCEDS59toSectors.csv", readcsv = TRUE)
    
    ap <- toolAggregate(ap, map, dim=3.1, from = "CEDS59", to = "Sectors")
    
    weight <- dimSums(ap[,,c("agriculture", "luc")], dim = 3)
    
    mappingfile <- toolMappingFile("regional","regionmappingMAgPIE.csv")
    mapping <- read.csv2(mappingfile)
    
  } else if (subtype == "macBase") {
    
    mappingfile <- toolMappingFile("regional","regionmappingMAgPIE.csv")
    mapping <- read.csv2(mappingfile)
    primap <- readSource("PRIMAPhist", subtype ="hist")
    weight <- dimSums(primap[,"y2005",c("CAT4", "CAT5")][,,c("n2o_n", "ch4")], dim = 3)
    
  } else if (subtype %in% c("co2tax","macBaseCO2luc")) {
    
    mapping <- toolMappingFile("regional","regionmappingH12.csv")
    # use total land area as weight
    weight <- calcOutput("LanduseInitialisation",aggregate=FALSE)[,2005,] 
    # sum over 3.dimension
    weight <- dimSums(weight,dim=3)
    getYears(weight) <- NULL
    
  } else if (subtype == "supplyCurve_magpie_40") {

    if(getSets(x)[1]=="region_fallback"){
      # if supply curves for current region mapping were not available use data with H12 MAgPIE regions
      mapping <- toolMappingFile("regional","regionmappingH12.csv")  
    } else {
      mapping <- toolMappingFile("regional",getConfig("regionmapping"))
    }
    weight = NULL

  } else if (subtype == "MAgPIEReport_extensive") {
    # remove GLO values because they cannot be handled by the disaggregation to ISO countries below
    x <- x["GLO",,,invert=TRUE]
    
    # currently the MAgPIE data is only available in H12 resolution
    mapping <- toolMappingFile("regional","regionmappingH12.csv")
    
    # use total land area as weight for regional disaggregation
    weight <- calcOutput("LanduseInitialisation",aggregate=FALSE)[,2005,] 
    # sum over 3.dimension
    weight <- dimSums(weight,dim=3)
    getYears(weight) <- NULL
    
  }
  
  if (subtype != "abatparam_co2") {
    y <- toolAggregate(x, mapping, weight = weight)
    y <- toolCountryFill(y, fill = 0)
  } else {
    # abatparam_co2
    y <- x
  }
  
  return(y)
}