#' @title calc Capacity Factor
#' @description provides capacity factor values
#'
#' @return magpie object of the capacity factor data
#' @author Renato Rodrigues, Stephen Bi
#' @examples
#' 
#' @importFrom dplyr summarise
#' \dontrun{ 
#' calcOutput("CapacityFactor")
#' }



calcCapacityFactorHist <- function(subtype){
  
  if (subtype == "wind") {
    
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
  
  } else if (subtype == "windoff") {
    
    #mapping of remind technology names to IRENA categories
    rem_Irena_map <- data.frame(rem=c("hydro","wind","windoff","spv","csp","bioigcc","geohdr"),
                                irena= c("Hydropower","Onshore wind energy", "Offshore wind energy","Solar photovoltaic","Concentrated solar power", "Bioenergy","Geothermal"))
    # Read capacity factor inputs
    hist_cap <- readSource(type="IRENA",subtype="Capacity")/1000 # converting from MW to GW
    hist_gen <- readSource("IRENA", subtype = "Generation")# Units are GWh

    # Calculate 2015 capacity factor for relevant technologies  
    # sum over 5 years of generation
    hist_gen2 = hist_gen[,seq(2013,2017,1),rem_Irena_map$irena]
    hist_gen3 <- as.data.frame(hist_gen2)
    hist_gen <- hist_gen3 %>%
      group_by(Region, Data1) %>%
      summarise(Value=sum(Value)) %>% 
      mutate(Year="2015") %>% 
      select(Year,Region,Data1,gen=Value) 
    
    # sum over 5 years of capacity
    hist_cap2 = hist_cap[,seq(2013,2017,1),rem_Irena_map$irena]
    hist_cap3 <- as.data.frame(hist_cap2)
    hist_cap <- hist_cap3 %>%
      group_by(Region, Data1) %>%
      summarise(Value=sum(Value)) %>% 
      mutate(Year="2015") %>% 
      select(Year,Region,Data1,cap=Value) 
    
    #get average capfac
    cf_realworld <- list(hist_gen, hist_cap) %>%
      reduce(full_join) %>% 
      mutate(Value = gen/(8760*cap)) %>% 
      select(Year,Region,Data1,Value) %>% 
      as.magpie()
    
    #rename
    getNames(cf_realworld) <- rem_Irena_map$rem
    #check data
    max(cf_realworld[,,"hydro"],na.rm = T)
    max(cf_realworld[,,"wind"],na.rm = T)#INF spm, >1 AZE
    max(cf_realworld[,,"windoff"],na.rm = T) 
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
    
    # cf_realworld <- as.data.frame(cf_realworld)
    
    #weight: historic generation
    hist_gen <- hist_gen %>%
      as.magpie()
    
    getNames(hist_gen) <- rem_Irena_map$rem
    hist_gen[is.na(cf_realworld)] <- 0
    
    return(list(x=cf_realworld, weight=hist_gen,
                unit="% of capacity", 
                description="Installed capacity availability in 2015 - capacity factor (fraction of the year that a plant is running)"              
    ))
  }
  
}

