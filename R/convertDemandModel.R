#' Convert DemandModel data
#' 
#' Convert DemandModel data on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing DemandModel data country-region resolution
#' @return DemandModel data as MAgPIE object aggregated to country level
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{ a <- convertDemandModel(x)
#' }
#' 
convertDemandModel <- function(x) {
  # add Kosovo("KSV") to Serbia("SRB") 
  add_v <- c("hunger","hhwaste","fish","material","food_livestock","kcal","gdp","pop","food")
  x["SRB",,add_v] <- setCells(x["KSV",,add_v],getCells(x["SRB",,add_v]))+x["SRB",,add_v]
  # deal with the intensive variables
  x["SRB",,"gdp_pc"]             <- x["SRB",,"gdp"]/x["SRB",,"pop"]
  x["SRB",,"hunger_shr"]         <- x["SRB",,"hunger"]/x["SRB",,"pop"]
  x["SRB",,"waste_shr"]          <- x["SRB",,"hhwaste"]/x["SRB",,"food"]
  x["SRB",,"food_livestock_shr"] <- x["SRB",,"food_livestock"]/x["SRB",,"food"]
  # delete KSV
  x <- x["KSV",,,invert=TRUE]
  # fill all the rest with 0
  x <- toolCountryFill(x,fill=0)
  return(x)
}  
