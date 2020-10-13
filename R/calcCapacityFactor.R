#' @title calc Capacity Factor
#' @description provides capacity factor values
#'
#' @return magpie object of the capacity factor data
#' @author Renato Rodrigues
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("CapacityFactor")
#' }
#' 

calcCapacityFactor <- function(){
  
  # Read capacity factor inputs
  global <- readSource("REMIND_11Regi", subtype="capacityFactorGlobal", convert = FALSE)
  # Read capacity factor rules
  rules <- readSource("REMIND_11Regi", subtype="capacityFactorRules")
  # Merging global and rules values
  #   Creating new MAgPIE object to store the final capacity values
  output <- new.magpie(getRegions(rules),seq(2005,2150,5),getNames(global)) 
  # Filling MagPIE object with global values
  output[,,getNames(global)] <- global[,,getNames(global)]
  # Overwritting MAgPie object with rules values
  output[getRegions(rules),getYears(rules),getNames(rules)] <- ifelse(rules[getRegions(rules),getYears(rules),getNames(rules)]!=0, rules[getRegions(rules),getYears(rules),getNames(rules)], output[getRegions(rules),getYears(rules),getNames(rules)]) 
  # Define weight aggregation for capacity factors
  # using final energy as a proxy for the existent capacity factor to weight the capacity factor aggregation (it should be changed if the information about the existent capacity factor become available in the future)
  weight <- calcOutput("FE",aggregate=FALSE)[,2005,"FE (EJ/yr)"]
  # Return regions aggreggation weighted by final energy 
  return(list(x=output, weight=weight,
               unit="% of capacity", 
               description="Installed capacity availability - capacity factor (fraction of the year that a plant is running)"              
  ))
  
}

