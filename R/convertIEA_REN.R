#' Reads the distributed solar pv capacity from IEA Renewables report (2019). 
#' @details Capacity in GW. Distributed solar, defined in the IEA Renewables (2019), includes 
#' rooftop residential (0-10 kW, grid-connected), tooftop and ground-mounted commercial
#'  and industrial (10-1000kW, grid-connected), and off-grid (8W - 100 kW)
#' @author Aman Malik
#' @importFrom readxl read_excel
#' @return magpie object with country-wise distributed solar pv capacity
#' @param x input magpie object

convertIEA_REN <- function(x){
  country <- NULL
  region <- NULL
  

  mapping_rem <- toolGetMapping(getConfig()[1],where = "mappingfolder",type = "regional")
  # mapping file
  mapping_IEA <- toolGetMapping("mappingIEA-REN.csv",where = "mappingfolder",type = "regional")
  mapping_IEA$country <- gsub("Bolivarian Republic of Venezuela \\(Venezuela\\)","Venezuela",mapping_IEA$country)
  mapping_IEA$country <- gsub("Kingdom of Eswatini Lesotho","Lesotho",mapping_IEA$country)
  #mapping_IEA$country <- grep(pattern = "Lao" ,value = T, mapping_IEA$country)
                               
  mapping_IEA$country <- gsub("Plurinational State of Bolivia \\(Bolivia\\)","Bolivia",mapping_IEA$country)
  mapping_IEA$country <- toolCountry2isocode(mapping_IEA$country)
  mapping_IEA <- mapping_IEA %>% filter(country!="KOS")
  # IEA Regions - Asia-Pacific, MENA, North and Central America, South America,
  # Europe, Eurasia, Sub-saharan Africa
  #x <- readSource("IEA_REN",convert=F)
  getRegions(x) <- gsub("Central and South America","Latin America",getRegions(x))

  # new magpie object with only country values, excluding region names and World
  y <- new.magpie(setdiff(getRegions(x),c(unique(mapping_IEA$region),"World")),
                  years = getYears(x),names = getNames(x),fill = 0)
  y[,,] <- x[getRegions(y),,]
  getRegions(y) <- toolCountry2isocode(getRegions(y))
  
  # wt by solar pv capacity in 2018
  wt <-  calcOutput("Capacity", subtype="capacityByTech",aggregate =F)
  wt <- wt[,"y2018","spv"]
  
  output <- new.magpie(mapping_rem$CountryCode,years = getYears(x),names=getNames(x),fill=0)
  
  for (i in c("Europe","Asia-Pacific","Latin America")){
      
      cont_iea <- mapping_IEA[mapping_IEA$region==i,]$country
      reg <- new.magpie(i,years = getYears(x),names = getNames(x),fill = 0)
      cont_y <- intersect(getRegions(y),cont_iea)
      reg[,,] <- x[i,,]-dimSums(y[cont_y,,],dim = 1)
      rel=mapping_IEA %>% filter(!country %in% cont_y,region==i)
      reg <- toolAggregate(x = reg,
                              rel=rel,
                              weight = wt[rel$country,,],
                              from="region",
                              to="country")
      output[getRegions(reg),,] <- reg
      }

# other regions

  for (j in c("Eurasia","MENA","Sub-Saharan Africa","North America")){
  reg <- x[j,,]
  rel= mapping_IEA %>% filter(region==j)
  reg <- toolAggregate(x = reg,
                       rel = rel,
                       weight = wt[rel$country,,],
                       from="region",
                       to="country")
  output[getRegions(reg),,] <- reg
                      
  }
  
output[getRegions(y),,] <- y  
x <- toolCountryFill(output,fill=0)

 
# # part of resulting magpie object, mo gets difference of region and countries which
# # already have data
# Europe <- new.magpie("Europe" ,years = getYears(x),fill = 0)
# # countries with region "Europe" in magpie object
# eu_count_in_y <- intersect(getRegions(y),mapping_IEA[mapping_IEA$region=="Europe",]$country)
# Europe[,,] <- x["Europe",,]-dimSums(y[eu_count_in_y,,],dim = 1)
# 
# eu_count_iea <- mapping_IEA[mapping_IEA$region=="Europe",]$country
# 
# 
# 
# Europe <- toolAggregate(x = Europe,
#                         rel=mapping_IEA %>% filter(!country %in% eu_count_in_y,region=="Europe"),
#                         weight = wt[setdiff(eu_count_iea,eu_count_in_y),,],
#                         from="region",
#                         to="country")
return (x)
}

