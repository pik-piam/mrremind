#' @title calc ETS Reference Emissions
#' @description provides region specific ETS Reference Emissions
#'
#' @param subtype type of reference emissions used to define emission reduction targets for European regulations: EEA_GHG
#' @return 2005 reference emissions to calculate ETS targets
#' @author Renato Rodrigues
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ETSRefEmi",subtype="EEA_GHG")
#' }
#' 

calcETSRefEmi <- function(subtype){
  
  if(subtype=="EEA_GHG"){
    
    e <- setNames(dimSums(readSource("EEA_EuropeanEnvironmentAgency", subtype="ETS")[,2005,c("2_ Verified emissions.20-99 All stationary installations","3_ Estimate to reflect current ETS scope for allowances and emissions.20-99 All stationary installations")]),"Emi|GHG|ETS (Mt CO2-equiv/yr)")
    e[is.na(e)] <- 0
    description <- "ETS reference 2005 emissions in Mt CO2-equiv from EEA data" 
    unit <- "Mt CO2-equiv"
    
  }
  getNames(e) <- NULL
  
  return(list(x=e, weight=NULL,unit=unit,description=description)) 
}
