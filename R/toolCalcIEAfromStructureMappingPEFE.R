#' @title toolCalcIEAfromStructureMappingPEFE
#' @description mapping IEA reported data to ReMIND-MAgPIE items
#'
#' @param data data to map
#' @param structureMapping mapping to use
#' @param subtype remind (default), edge, pfu or magpie
#' 
#' @return MAgPIE object with completed time dimensionality.
#' @author Anastasis Giannousakis, Lavinia Baumstark, Isabelle Weindl 
#' 
#' @importFrom stats na.omit
#' @importFrom dplyr %>%
#' @importFrom dplyr filter_
#' @export

toolCalcIEAfromStructureMappingPEFE<-function(data, structureMapping, subtype = "remind")  {
   
  #choose the name of the column which should be targeted in the structureMapping
  if  (subtype == "remind") {
    targetName = "REMINDitems_out"
  } else if (subtype == "edge") {
    targetName = "EDGEitems"  
  } else if (subtype == "pfu") {
    targetName = "pfu"  
  } else if (subtype == "magpie") {
    targetName = "magpie_items"   
  } else stop("valid subtypes are 'remind', 'edge', 'pfu' and 'magpie'")
  
ieamatch <- na.omit(read.csv2(structureMapping, stringsAsFactors=F)[c("iea_product", "iea_flows", targetName, "Weight")])

# take only the items that are asigned to model categories
ieamatch <- subset(ieamatch,!grepl("not_used",ieamatch[[targetName]]))

regions <- getRegions(data)
years <- getYears(data)

# take only the items at the intersection of data and ieamatch
flows_intersect = intersect(getNames(data, dim = "FLOW"), unique(ieamatch[["iea_flows"]]))
prod_intersect = intersect(getNames(data, dim = "PRODUCT"), unique(ieamatch[["iea_product"]]))

ieamatch = ieamatch %>% filter_(~iea_flows %in% flows_intersect,
                               ~iea_product %in% prod_intersect)

#create an empty mapgie object that will be filled with aggregated items
outputnames<- paste(rep(unique(ieamatch[[targetName]]), each = length(flows_intersect)), flows_intersect, sep = ".")
targetitems <- as.magpie(array(dim=c(length(regions), length(years), length(outputnames)),dimnames=list(regions, years, outputnames)))


for (item in getNames(targetitems,dim=1)){
  map = ieamatch[ieamatch[[targetName]]==item,c("iea_product","iea_flows")]
  map_sub = paste(map[["iea_product"]],map[["iea_flows"]],sep = ".")
  data[,,map_sub] <- data[,,map_sub] * as.numeric(ieamatch[rownames(map),"Weight"])
  targetitems[,,item]<-dimSums(data[,,map_sub],dim=3.1,na.rm = TRUE)
}

return(targetitems)
}
