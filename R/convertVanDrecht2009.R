#' convertVanDrecht2009
#' 
#' Reads a dataset containing values for sewage
#' 
#' @param x MAgPIE object containing incomplete country-region resolution
#' @return A MAgPIE object containing sewage quantities and losses
#' @author Benjamin Leon Bodirksy
#' @examples
#' 
#' 
#'   \dontrun{
#'     x <- readSource("VanDerWerf2010")
#'   }
#' 
#' @importFrom reshape2 melt
convertVanDrecht2009 <- function(x){
  mapping<-toolMappingFile(type="regional",name="regionmappingVanDrecht.csv",readcsv = T)
  x<-x[,,"pop",invert=TRUE]
  x<-toolAggregate(x = x,rel = mapping,from="RegionCode",to="CountryCode")
  x[,,c("hum_n_pp","hum_p_pp","det_p_pp" )]=x[,,c("hum_n_pp","hum_p_pp","det_p_pp" )]/1000
  x[,,c("sewage_shr","sewage_n_removal_shr","sewage_p_removal_shr")]=x[,,c("sewage_shr","sewage_n_removal_shr","sewage_p_removal_shr")]/100
  return(x)
}
