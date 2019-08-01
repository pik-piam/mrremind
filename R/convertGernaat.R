#' Convert Hydro-Potential data from Gernaat et. al 2017
#' 
#' Convert magpie object of hydropotential data from Gernaat et. al 2017. Technical potential
#' defined as <0.5 $/kWh and economic potential as <0.1 $/kWh.
#' 
#' @param x magpie object derived from readGernaat()
#' @param subtype Possibilities for subtype are "Technical_Potential" or "Economic_Potential"
#' @return magpie object of converted hydropotential data from Gernaat et. al 2017
#' @author Atreya Shankar, Renato Rodrigues
#' @source Obtained from Nature Energy volume 2 (2017), pp 821–828, with following source: https://www.nature.com/articles/s41560-017-0006-yy
#' @importFrom demystas vectorEnum
#' @examples
#' \dontrun{a <- convertGernaat(x, "Technical_Potential")}

convertGernaat <- function(x, subtype){
  
  sub <- c("Technical_Potential", "Economic_Potential")
  
  if(length(subtype) == 1 & all(subtype %in% sub)){
    
    if(subtype == "Economic_Potential"){
      x <- x[which(x[,,3] < 0.1)]
    }
    
    remind <- read.csv2(paste0(getConfig()$mappingfolder, "/regional/regionmappingH12.csv"), stringsAsFactors = FALSE)[,2]
    y <- mbind(x, new.magpie(paste0(remind[which(!remind %in% getRegions(x))], ".1"), getYears(x), getNames(x), fill = 0))
    return(y)
    
  } else stop("invalid subtype!")
}