#'@title convertWFaS
#' @description Convert subtypes of the WFaS data
#' 
#' Convert subtypes on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing WFaS data on native level
#' @param subtype data subtype. available subtypes are:
#' \itemize{
#' \item dom_ww_ssp1_rcp4.5
#' \item dom_ww_ssp2_rcp6.0
#' \item dom_ww_ssp3_rcp6.0
#' \item dom_use_ssp1_rcp4.5
#' \item dom_use_ssp2_rcp6.0
#' \item dom_use_ssp3_rcp6.0
#' \item ind_ww_ssp1_rcp4.5
#' \item ind_ww_ssp2_rcp6.0
#' \item ind_ww_ssp3_rcp6.0
#' \item ind_use_ssp1_rcp4.5
#' \item ind_use_ssp2_rcp6.0
#' \item ind_use_ssp3_rcp6.0
#' \item dom_ww_ssp1_rcp4.5_gridded
#' \item dom_ww_tech_ssp1_rcp4.5_gridded
#' \item dom_use_ssp1_rcp4.5_gridded
#' \item dom_use_tech_ssp1_rcp4.5_gridded
#' \item ind_ww_ssp1_rcp4.5_gridded
#' \item ind_ww_tech_ssp1_rcp4.5_gridded
#' \item ind_use_ssp1_rcp4.5_gridded
#' \item ind_use_tech_ssp1_rcp4.5_gridded
#' \item dom_ww_ssp2_rcp2.6_gridded_2100
#' \item dom_ww_tech_ssp2_rcp2.6_gridded_2100
#' \item dom_use_ssp2_rcp2.6_gridded_2100
#' \item dom_use_tech_ssp2_rcp2.6_gridded_2100
#' \item ind_ww_ssp2_rcp2.6_gridded_2100
#' \item ind_ww_tech_ssp2_rcp2.6_gridded_2100
#' \item ind_use_ssp2_rcp2.6_gridded_2100
#' \item ind_use_tech_ssp2_rcp2.6_gridded_2100
#' \item dom_ww_ssp2_rcp4.5_gridded_2100
#' \item dom_ww_tech_ssp2_rcp4.5_gridded_2100
#' \item dom_use_ssp2_rcp4.5_gridded_2100
#' \item dom_use_tech_ssp2_rcp4.5_gridded_2100
#' \item ind_ww_ssp2_rcp4.5_gridded_2100
#' \item ind_use_ssp2_rcp4.5_gridded_2100
#' \item ind_use_tech_ssp2_rcp4.5_gridded_2100
#' \item dom_ww_ssp2_rcp6.0_gridded_2100
#' \item dom_ww_tech_ssp2_rcp6.0_gridded_2100
#' \item dom_use_tech_ssp2_rcp6.0_gridded_2100
#' \item ind_ww_ssp2_rcp6.0_gridded_2100
#' \item ind_ww_tech_ssp2_rcp6.0_gridded_2100
#' \item ind_use_ssp2_rcp6.0_gridded_2100
#' \item ind_use_tech_ssp2_rcp6.0_gridded_2100
#' \item dom_ww_ssp2_rcp6.0_gridded
#' \item dom_ww_tech_ssp2_rcp6.0_gridded
#' \item dom_use_ssp2_rcp6.0_gridded
#' \item dom_use_tech_ssp2_rcp6.0_gridded
#' \item ind_ww_ssp2_rcp6.0_gridded
#' \item ind_ww_tech_ssp2_rcp6.0_gridded
#' \item ind_use_ssp2_rcp6.0_gridded
#' \item ind_use_tech_ssp2_rcp6.0_gridded
#' \item dom_ww_ssp3_rcp6.0_gridded
#' \item dom_ww_tech_ssp3_rcp6.0_gridded
#' \item dom_use_ssp3_rcp6.0_gridded
#' \item dom_use_tech_ssp3_rcp6.0_gridded
#' \item ind_ww_ssp3_rcp6.0_gridded
#' \item ind_ww_tech_ssp3_rcp6.0_gridded
#' \item ind_use_ssp3_rcp6.0_gridded
#' \item ind_use_tech_ssp3_rcp6.0_gridded
#' \item dom_use_historic_gridded
#' \item dom_ww_historic_gridded
#' \item ind_use_historic_gridded
#' \item ind_ww_historic_gridded
#' }
#' @return WFaS data as MAgPIE object for all subtypes on country level
#' @author Stephen Wirth
#' @note Spatial Information of the gridded Data is lost. For detailed Information on Data the subtypes contain, see help file for readWFaS.
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' a <- readSource("WFaS","dom_ww_ssp1_rcp4.5")
#' a <- readSource("WFaS","dom_ww_ssp1_rcp4.5_gridded")
#' a <- readSource("WFaS","dom_use_tech_ssp2_rcp6.0_gridded_2100")
#' }
#' 
convertWFaS <- function(x,subtype)
{
  
  
  
  
  
  if(grepl("gridded|historic", subtype))
  {
    map <- toolMappingFile(type="cell", readcsv=T, name="CountrytoCellMapping.csv")
    # y <- toolAggregate(x, map, partrel = T, from=1 ,to=2, dim=1) 
    # y <- toolCountryFill(y)
    # # map <- "regionmappingMAgPIE.csv"
    # # z <- toolAggregate(y,map, partrel = T, from=2, to=3)
    # # z <- toolCountryFill(z)
    # return(y)
    #x[x==-9999] <- NA
    getRegions(x) <- map$cell
    y <- toolAggregate(x, map, from=1, to=3, dim=1)
    y <- toolCountryFill(y, fill=NA)
    return(y)
  }
  else
  {
    map <- toolMappingFile(type="regional", readcsv=T, name="regionmappingMAgPIE.csv")
    countries <- read.csv("country_mapping.csv", header = T, sep=";", stringsAsFactors = F)
    countries <- countries[1:232,]
   # map <- "regionmappingMAgPIE.csv"  
  
  y <- toolAggregate(x,countries, partrel=T, from=2, to=3)
  z <- toolAggregate(y, map, partrel = T, from=1 ,to=2, dim=1)
  z <- toolCountryFill(z)
  z["GGY",,] <- z["JEY",,]
  z[c("BES","SXM"),,]<- z["CUW",,]
  message("Data for Serbia (SRB) is for Serbia an Montenegro (MNE) in order to use the data as modelinput dissaggregation is necessary!")
  
  return(z)
  }
 # message("Please contact Anne Biewald regarding application of this Dataset!")
 
  }