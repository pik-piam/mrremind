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

calcCapacityFactorHist <- function(){
  #mapping of remind technology names to IRENA categories
  rem_Irena_map <- data.frame(rem=c("hydro","wind","spv","csp","bioigcc","geohdr"),
                              irena= c("Hydropower","Wind","Solar photovoltaic","Concentrated solar power", "Bioenergy","Geothermal"))
  # Read capacity factor inputs
  hist_cap <- readSource(type="IRENA",subtype="Capacity")/1000 # converting from MW to GW
  hist_gen <- readSource("IRENA", subtype = "Generation")# Units are GWh
  # Calculate 2015 capacity factor for relevant technologies  
  cf_realworld <- hist_gen[,2015,rem_Irena_map$irena]/(8760*hist_cap[,2015,rem_Irena_map$irena]) 
  #rename
  getNames(cf_realworld) <- rem_Irena_map$rem
  #check data
  max(cf_realworld[,,"hydro"],na.rm = T)
  max(cf_realworld[,,"wind"],na.rm = T) #INF spm, >1 AZE
  max(cf_realworld[,,"spv"],na.rm = T)
  max(cf_realworld[,,"csp"],na.rm = T)
  max(cf_realworld[,,"bioigcc"],na.rm = T) #>1 CHL, JPN, POL 
  max(cf_realworld[,,"geohdr"],na.rm = T)
  
  
  
  #correct SPM infinite value
  cf_realworld[is.infinite(cf_realworld)] <- 0.8
  #correct AZE,CHL,JPN,POL >1 value
  cf_realworld[cf_realworld > 1] <- 0.8
  #get rid of NAs
  cf_realworld[is.na(cf_realworld)] <- 0
  
  
  #weight: historic generation
  hist_gen <- hist_gen[,2015,rem_Irena_map$irena]
  getNames(hist_gen) <- rem_Irena_map$rem
  hist_gen[is.na(cf_realworld)] <- 0
  return(list(x=cf_realworld, weight=hist_gen,
               unit="% of capacity", 
               description="Installed capacity availability in 2015 - capacity factor (fraction of the year that a plant is running)"              
  ))
  
}

