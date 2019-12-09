#' Employment factors for various power production technologies
#' @description All countries, technologies included - SolarPV,SolarCSP,Wind, hydropower, oil, gas, coal, nuclear, biomass. 
#' Activities included - manufacturing, construction and installation, operations and maintainence, fuel supply and heat supply. 
#' @note Depending on the activity, the employment factors can have different units.
#' @author Aman Malik
#' @param x MAgPIE object to be converted
#' @importFrom stats runif
#' @return magpie object of emplyoment factors for all countries per technology and activity


convertEmplyomentfactor <- function(x){
  
  # x <- readSource("Employmentfactor",convert=F)
  getNames(x) <- gsub(pattern = "SolarPV",replacement = "Solar|PV",getNames(x),fixed = T)
  techs <- c("Solar|CSP","Wind","Hydro","Biomass","Coal","Gas","Nuclear","Oil")
  activity <- c("Manf","CI","OM","Fuel_supply","Heat_supply")
  x <- add_columns(x=x,addnm = techs,dim=3.1)
  x <- add_columns(x=x,addnm = activity,dim=3.2)
  x <- toolCountryFill(x)
  x[,,] <- runif(22410,0,1) # assigning random values between 0 and 1
 return (x)

}