#' @title calcEmiLULUCFCountryAcc
#' @description hisorical LULUCF emissions following country accounting
#' @return Magpie object with historical LULUCF emissions
#' @param subtype Valid subtypes are 'PRIMAPhist'
#' @author Felix Schreyer
#' @importFrom magclass dimReduce
#' @importFrom madrat readSource
#' @import mrcommons

calcEmiLULUCFCountryAcc <- function(subtype){
  
  
  if (subtype=="PRIMAPhist") {
    
    
    # take PRIMAPhist LULUCF data for now
    # note: these historical LULUCF emissions data correspond neither to UNFCCC (https://di.unfccc.int/detailed_data_by_party) 
    # nor to EEA (https://cdr.eionet.europa.eu/de/eu/mmr/art07_inventory/ghg_inventory/envxh8awg/index_html?&page=1)
    primap <- readSource("PRIMAPhist","hist")
    
    # LULUCF CO2 emissions from PRIMAP hist database from 1990 to 2015, convert to Mt CO2/yr
    out <- dimReduce(primap[,paste0("y",seq(1990,2015,1)),"co2_c"][,,"CAT5"]) / 12*44
    
  } else {
    "Please define a valid subtype for this function."
    out <- NULL
  }
    
  return(
      list(x=out,
                weight=NULL,
                unit="Mt CO2/yr",
                description="Historical LULUCF CO2 emissions data following country accounting taken from PRIMAPhist database")
    )
}
    
    

  
    
 