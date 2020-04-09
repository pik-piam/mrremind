#' toolBiomassSupplyAggregate
#' The function aggregates biomass supply curves to regionmapping different from H12. 
#' It only works if all regions are subregions of H12 regions. The offset parameter (a) is taken from the H12 region. 
#' The slope parameter (b) is multiplied by a weight. The weight is the inverse of the share of agricultural area of the subregion in the H12 region.
#' 
#' @return return: returns region aggregated biomass supply curve data
#' 
#' @author Felix Schreyer
#' @export
#' @importFrom magclass as.magpie
#' @importFrom dplyr %>% mutate select rename  left_join group_by ungroup summarise
#' @importFrom quitte as.quitte 



toolBiomassSupplyAggregate <- function(x, rel=NULL){
  
  # variable definitions for dataframe operations
  RegionCode <- NULL
  RegionCodeH12 <- NULL
  region <- NULL
  period <- NULL
  scenario <- NULL
  char <- NULL
  data <- NULL
  value <- NULL
  value.H12 <- NULL
  Share <- NULL
  
  # get H12 mapping (of MagPIE supply curves) and current regionmapping
  mappingH12 <- toolGetMapping("regionmappingH12.csv")
  mapping <- toolGetMapping(getConfig("regionmapping"), type="regional")
  
  
  # mapping of new regions to H12 regions
  # (note: this presumes that all regions of non-H12 mapping are subregions or regions in H12)
  mapping.join <- mappingH12 %>% 
                    rename(RegionCodeH12 = RegionCode) %>% 
                    left_join(mapping) 
  
  # test if all regions of non-H12 mapping are subregions or regions in H12, 
  # if not this disaggregation is no possible
  if (anyDuplicated(unique(mapping.join[,c(3,4)])) != 0) {
    stop("Error: regionmapping contains regions that are not a subset of one H12 region, but of multiple H12 regions. In this case the disaggregation does not work.")
  } else {
    # reduce mapping to mapping of regioncodes to H12 regioncodes
    mapping.join <- mapping.join %>% 
                      group_by(RegionCode, RegionCodeH12) %>% 
                      summarise() %>% 
                      ungroup()
}

  # aggregate biomass supply curves to both mappings (current mapping and H12), 
  # weight 1 -> transfer iso values to regional values as they are
  x_reg <- toolAggregate(x, mapping, weight = new.magpie(getRegions(x), fill = 1))
  x_H12 <- toolAggregate(x, mappingH12, weight = new.magpie(getRegions(x), fill = 1))
  
  # read in 2010 agricultural area for weights of disaggregation/aggregation
  FAOAgrLand <- calcOutput("FAOLand", aggregate = F)[,"y2010","6610|Agricultural area.area"]
  # aggregate to both mappings (current and H12)
  FAOAgrLandReg <- toolAggregate(FAOAgrLand, mapping)
  FAOAgrLandH12 <- toolAggregate(FAOAgrLand, mappingH12)
  
  # calculate share of agricultural land in new regionmapping relative to H12 regionmapping
  # (given that all new regions are subregions of H12 regions)
  df.AgrLandShare <- as.quitte(FAOAgrLandReg) %>% 
                       rename(RegionCode = region) %>% 
                       select(RegionCode, value) %>% 
                       left_join(mapping.join) %>%  
                       left_join(as.quitte(FAOAgrLandH12) %>% 
                         select(region, value) %>% 
                         rename(value.H12 = value), 
                         by=c("RegionCodeH12"="region")) %>% 
                       mutate( Share = value / value.H12) %>% 
                       select(RegionCode, Share)
                       
  # regional (non-H12) biomass supply curve:
  # a = a(H12 region) (offset)
  # b = b / AgrShare(region, H12region)  (slope)
  # (AgrShare = share of agricultural area of H12 subregion in H12 region)
  df.bio.Supply.reg <- as.quitte(x_reg) %>%  
                        select(region, period, scenario, char, data, value) %>%  
                        left_join(df.AgrLandShare, by = c("region" = "RegionCode")) %>% 
                        mutate(value = ifelse(data=="b",value / Share, value)) %>% 
                        select(-Share)
  x_reg_weighted <- as.magpie(df.bio.Supply.reg, spatial=1, temporal=2, datacol=6)
  
  return(x_reg_weighted)
}