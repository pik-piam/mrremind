#' Employment factors for various power production technologies from Rutovitz et al. 2015
#' @description Rutovitz, J., Dominish, E., & Downes, J. (2015). Calculating global energy sector jobs—2015 methodology update. Institute for Sustainable Futures, University of Technology, Sydney. https://opus.lib.uts.edu.au/bitstream/10453/43718/1/Rutovitzetal2015Calculatingglobalenergysectorjobsmethodology.pdf
#' @author Aman Malik
#' @importFrom  tidyr pivot_longer
#' @importFrom dplyr rename add_row filter_ mutate_ select_ left_join filter mutate
#' @importFrom readr read_csv
#' @importFrom mgsub mgsub
#' @return magpie object of employment factors for different technologies and activities in Jobs/MW (all except fuel_supply) or Jobs/PJ (fuel_supply). Subtype "regional_mult" is a regional multiplier without units.
#' @param subtype Either "oecd_ef","regional_ef","coal_ef","gas_ef", "regional_mult"


readRutovitz2015 <- function(subtype){
  # note: Employment factors from rutovitz2015 et al. are wt. average values from OECD countries. (Regional) Exceptions are input in subtype "regional_ef", "coal_ef" and "gas_ef"  
  CI <- NULL # construction and installation
  Fuel_supply <- NULL # process of extraction of resource
  Manf <- NULL # manufacturing
  OM <- NULL # operation and maintenance
  activity <- NULL # all 4 categories above are "activities"
  region <- NULL
  tech <- NULL # power production technology
  duration <- NULL # average time (years) from start of construction to commission.
  Productivity <- NULL
  Year <- NULL
  notes <- NULL
  value <- NULL
  vars <- NULL
  if (subtype == "oecd_ef")
    {
    
    input <- read_csv(file = "oecd_ef.csv",na = "",col_types = "cddddc") %>% 
      rename(tech=1,duration=2,CI=3,Manf=4,OM=5, Fuel_supply=6) %>% 
      filter(!is.na(tech)) %>% 
      add_row(tech="CoalHP",duration=5,CI=11.2,Manf=5.4,OM=0.14*1.5,Fuel_supply="Regional") %>%  # for *HP technologies, multiply OM EF by 1.5 as in rutovitz
      add_row(tech="GasHP",duration=2,CI=1.3,Manf=0.93,OM=0.14*1.5,Fuel_supply="Regional") %>% 
      add_row(tech="BiomassHP",duration=2,CI=14,Manf=2.9,OM=1.5*1.5,Fuel_supply="Regional") %>% 
      add_row(tech="Oil",CI=1.3,Manf=0.93,OM=0.14,Fuel_supply="Regional",duration=2) %>% # oil EF= Gas EF as in rutovitz
      filter(!tech %in% grep("Ocean|decommissioning|heat|diesel",
                             x =tech,value=T)) %>%  # removing  techs not relevant
      mutate(tech=mgsub::mgsub(tech, c("Solar Photovoltaics","Solar thermal"),
                             c("Solar|PV","Solar|CSP"))) %>%  ## renaming techs
      mutate(across(c(CI,Manf,OM,duration),as.numeric)) %>% 
      mutate(Fuel_supply=ifelse(Fuel_supply == "0.001 jobs/GWh final demand",0.001,Fuel_supply))  %>% 
      #mutate_(CI=~CI/duration) %>% # dividing employment intensity by construction period
      #mutate_(Manf=~Manf/duration) %>% 
      select(-duration)   %>% 
      mutate(`Fuel_supply`=ifelse(`Fuel_supply` == "Regional",0,`Fuel_supply`),
             `Fuel_supply`=as.numeric(`Fuel_supply`)) %>% 
      pivot_longer(names_to =  "activity",values_to= "value",c(2:5))   %>% 
      # regional values exist for coal and gas and are read later
      mutate(across(c(tech,activity),as.factor)) %>% 
      mutate(value=as.numeric(value))
    
      x <- as.magpie(input,temporal=NULL,spatial=NULL,datacol=3)
    return (x)
  }
  

  if (subtype == "regional_ef")
    {
    

  input <- read_csv("regional_ef.csv",na="",col_types = "ccdddd") %>% 
    rename(tech=1,region=2,CI=3,Manf=4,OM=5,Fuel_supply=6) %>% 
    filter(!is.na(tech)) %>% 
 #   mutate(across(c(tech,region),as.character)) %>%
  #  mutate(across(c(CI,Manf,OM,Fuel_supply),as.numeric)) %>%
    mutate(tech=mgsub::mgsub(tech, c("Solar PV","Solar Thermal power","Wind-offshore","Wind-onshore"),
                                   c("Solar|PV","Solar|CSP","Wind offshore","Wind onshore"))) %>% 
    pivot_longer(c("CI","Manf","OM","Fuel_supply"),names_to = "activity",values_to = "value") %>% 
    filter(!grepl("average",region)) %>%  # removed OECD average values
    na.omit() %>% 
    #left_join(const,by="tech") %>% 
    #mutate(region=ifelse(region=="OECD North America","OECD Americas",region))
    #mutate(value=ifelse(!activity %in% c("Fuel_supply","OM"),value/duration,value)) %>% 
    select(region,tech,activity,value)
    

  x <- as.magpie(input,spatial=1,temporal=NULL,datacol=4)

  return (x)
}

  if(subtype=="coal_ef")
    {
   input <- read_csv("coal_ef.csv",col_types = "cddd") %>% 
     select(-Year,-Productivity) %>% 
     rename(region=1,value=2) %>% 
     filter(!is.na(region)) %>% 
     mutate(value=ifelse(region=="Middle East",39.7,value)) %>% 
     filter(!grepl("World",region)) %>% 
     mutate(region=ifelse(region=="Eastern Europe/Eurasia","Eurasia",region)) %>% 
     mutate(tech="Coal") %>% 
     mutate(activity="Fuel_supply") %>% 
     mutate(value=as.numeric(value))
   input <- input[,c("region","tech","activity","value")]
   
   x <- as.magpie(input,temporal=NULL,datacol=4,spatial=1)
   
   return (x)
 }

  if(subtype=="gas_ef")
    {
  input <- read_csv("gas_ef.csv",na="",col_types = "cdcc") %>% 
    rename(region=1,value=2,notes=3) %>% 
    select(-notes,-Sources) %>% 
    rbind(data.frame(region=c("India","Latin America","Developing Asia","Middle East"),value=15.1)) %>% # using world average values from dataset
    filter(!is.na(value)) %>% 
    filter(!grepl("World",region))  %>% 
    mutate(region=ifelse(region=="Eastern Europe/Eurasia","Eurasia",region)) %>% 
    mutate(tech="Gas") %>% 
    mutate(activity="Fuel_supply")
  
  input <- input[,c("region","tech","activity","value")]
  
  x <- as.magpie(input,temporal=NULL,datacol=4,spatial=1)
  
  return(x)  
}  
  if(subtype=="regional_mult"){
    input <- read_csv("regional_mult.csv",na="",col_types = "c") %>% 
    pivot_longer(cols = c(`2015`,`2020`,`2030`), names_to = "year") %>% 
    mutate(region=ifelse(region=="Eastern Europe/Eurasia","Eurasia",region))
  
  x <- as.magpie(input,spatial=1,temporal=2)
  
  return (x)
  
  }
  
}

   
