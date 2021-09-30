#' toolBiomassSupplyAggregate
#' The function aggregates biomass supply curves to regionmapping different from H12. 
#' It only works if all regions are subregions of H12 regions. The offset parameter (a) is taken from the H12 region. 
#' The slope parameter (b) is multiplied by a weight. The weight is the inverse of the share of agricultural area of the subregion in the H12 region.
#' 
#' @return return: returns region aggregated biomass supply curve data
#' 
#' @param x magclass object that should be aggregated
#' @param rel relation matrix containing a region mapping.
#' @author Felix Schreyer
#' @export
#' @importFrom magclass as.magpie dimReduce
#' @importFrom dplyr %>% mutate select rename left_join 
#' @importFrom quitte as.quitte 



toolBiomassSupplyAggregate <- function(x, rel=NULL){
  
  # variable definitions needed for data.frame operations below
  region <- NULL
  value <- NULL
  Total <- NULL
  CountryCode <- NULL
  RegionCode <- NULL
  
  # FS: aggregate from iso-countries to regions
  # inverse of the disaggregation described in convertMAgPIE.R (subytpe = "supplyCurve_magpie_40")
  # copy offset parameter a from iso-country to region
  # multiply agricultural land share of iso-country in region with iso-country slope parameter b for
  # one iso-country to obtain slope parameter b of region
  
  # get regionmapping to aggregate to
  mapping <- toolGetMapping(type = "regional", name = getConfig("regionmapping"))
  
  # calculate share in agricultural land area of countries relative to MAgPIE regions
  # get agricultural land for iso-countries in 2010 from FAO
  AgrLandIso <- calcOutput("FAOLand", aggregate = F)[,,"6610",pmatch=TRUE][,"y2010",]
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
  
  # multiply slope parameter with agricultural land share
  x <- x
  x[,,"b"] <- x[,,"b"] * AgrLandShare
  
  # select one country per region (first country in alphabet) to set weight = 1, 
  # rest of countries weight 0
  mapping.sort <- mapping %>% 
                    arrange(RegionCode)
  iso.sel <- mapping.sort$CountryCode[cumsum(table(mapping.sort$RegionCode))]
  
  weight <- new.magpie(getRegions(x), fill = 0)
  weight[iso.sel,,] <- 1
  
  # aggregate to regionmapping, take a and b value only of one country within the region, 
  # other weights are 0
  y <- toolAggregate(x, mapping, weight = weight)
  
  return(y)
}