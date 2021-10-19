#' @title calcEmiLULUCFCountryAcc
#' @description hisorical LULUCF emissions following country accounting
#' @return Magpie object with historical LULUCF emissions
#' @param subtype Valid subtypes are 'PRIMAPhist'
#' @author Felix Schreyer
#' @importFrom magclass dimReduce
#' @importFrom madrat readSource
#' @import mrcommons

calcEmiLULUCFCountryAcc <- function(subtype){
  
  
  if (subtype=="UNFCCC") {
    
    
    # read in UNFCCC CRF emissions data
    unfccc <- readSource("UNFCCC")
    
    # LULUCF CO2 emissions from UNFCCC database from, convert to Mt CO2/yr
    out <- collapseNames(unfccc[,,"Total LULUCF|CO2"]) / 1000
    # replace NA by 0
    out[is.na(out)] <- 0
    
  } else {
    "Please define a valid subtype for this function."
    out <- NULL
  }
    
  return(
      list(x=out,
                weight=NULL,
                unit="Mt CO2/yr",
                description="Historical LULUCF CO2 emissions data following country accounting taken from UNFCCC database")
    )
}
    
    

  
    
 