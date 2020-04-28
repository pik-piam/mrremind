#' Convert MAgPIE data
#' 
#' Convert MAgPIE data to country-level.
#' 
#' @param x input MAgPIE data on region-level 
#' @param subtype Either "EmiAirPoll", "macBase", "co2tax", "supplyCurve_magpie_40", "abatparam_co2"
#' @importFrom madrat regionscode
#' @return magpie object
#' @author David Klein, Felix Schreyer
#' @export
#' @importFrom magclass as.magpie dimReduce
#' @importFrom dplyr %>% mutate select rename left_join 
#' @importFrom quitte as.quitte 
#' 
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
    
    # variable definitions needed for data.frame operations below
    region <- NULL
    value <- NULL
    Total <- NULL
    CountryCode <- NULL
    RegionCode <- NULL
    
    # FS: Disaggregation of MAgPIE biomass supply curves to iso-countries:
    # We assume that countries that are part of a MAgPIE region have the same fix cost to produce the first unit of biomass (same offset parameter).
    # For the slope parameter, we assume that biomass can be produced at lower cost in countries that currently have a large agricultural areas
    # We therefore divide the slope parameter of the MagPIE region by the share in agricultural area of the iso country relative to the MAgPIE region 
    # to obtain the disaggregated slope parameter of the iso-country. 
    # Note: This disaggregation does not work if iso-countries cover more than one MAgPIE region. For this case, this needs to be reworked!

    # regionmapping of input MAgPIE data, needs to be changed if MAgPIE data changes regionmapping
    mapping <- toolMappingFile("regional","regionmappingH12.csv", readcsv = T)
    
    # aggregate to from regions to iso-countries -> just copies regional values to countries
    y <- toolAggregate(x, mapping, weight = NULL)
    
    # calculate share in agricultural land area of countries relative to MAgPIE regions
    # get agricultural land for iso-countries in 2010 from FAO
    AgrLandIso <- calcOutput("FAOLand", aggregate = F)[,"y2010","6610|Agricultural area.area"]
    # aggregate agricultural land to regions in regionmapping
    AgrLandReg <- toolAggregate(AgrLandIso, mapping)
    
    # calculate share of agricultural land area for each iso-country relative to the MAgPIE region it is in
    AgrLandShare <- as.quitte(AgrLandIso) %>% 
                  select(region, value) %>% 
                  rename(CountryCode = region) %>% 
                  left_join(mapping) %>% 
                  left_join((as.quitte(AgrLandReg) %>% 
                               select(region, value) %>% 
                               rename(RegionCode = region, Total = value))) %>% 
                  mutate(value = value / Total) %>% 
                  # if no agricultural area at all -> assume very low share of 1e-5
                  mutate( value = ifelse(value == 0, 1e-5, value)) %>% 
                  select(CountryCode, value) %>% 
                  as.magpie(spatial = 1, datacol=2) %>% 
                  dimReduce()
    
    # divide slope parameter b by agricultural land share
    y[,,"b"] <- y[,,"b"] / AgrLandShare
    x <- y
                      
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
  
  # supplyCurve_magpie_40 disaggregated above
  if (! (subtype %in% c("abatparam_co2","supplyCurve_magpie_40"))) {
    y <- toolAggregate(x, mapping, weight = weight)
    y <- toolCountryFill(y, fill = 0)
  } else if (subtype %in% c("abatparam_co2")) {
    # abatparam_co2
    y <- x
  }
  
  return(y)
}