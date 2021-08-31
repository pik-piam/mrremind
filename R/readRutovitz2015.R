#' Employment factors for various power production technologies from Rutovitz et al. 2015
#' @author Aman Malik
#' @importFrom  tidyr gather_
#' @importFrom dplyr rename add_row filter_ mutate_ select_ mutate_at left_join filter mutate
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
                             x =tech,value=T)) %>%  # removing not relevant techs 
      mutate(tech=mgsub::mgsub(tech, c("Solar Photovoltaics","Solar thermal"),
                             c("Solar|PV","Solar|CSP"))) %>%  ## renaming techs
      mutate_at(vars(CI,Manf,OM,duration),as.numeric) %>% 
      mutate(Fuel_supply=ifelse(Fuel_supply == "0.001 jobs/GWh final demand",0.001,Fuel_supply))  %>% 
      #mutate_(CI=~CI/duration) %>% # dividing employment intensity by construction period
      #mutate_(Manf=~Manf/duration) %>% 
      select(-duration)   %>% 
      mutate(`Fuel_supply`=ifelse(`Fuel_supply` == "Regional",0,`Fuel_supply`),
             `Fuel_supply`=as.numeric(`Fuel_supply`)) %>% 
      pivot_longer(names_to =  "activity",values_to= "value",c(2:5))   %>% 
      # regional values exist for coal and gas and are read later
      mutate_at(vars(tech,activity),as.factor) %>% 
      mutate(value=as.numeric(value))
    
      x <- as.magpie(input,temporal=NULL,spatial=NULL,datacol=3)
    return (x)
  }
  

  if (subtype == "regional_ef")
    {
    
  #const <- data.frame(tech=c("Nuclear","Biomass","Hydro","Wind","Solar|PV","Solar|CSP","Wind offshore"),
       #                 duration=c(10,2,2,2,1,2,4))
  input <- read_csv("regional_ef.csv",na="") %>%
    rename(tech=1,region=2,CI=3,Manf=4,OM=5,Fuel_supply=6) %>%
    filter(!is.na(tech)) %>%
    mutate_at(vars(tech,region),as.character) %>%
    mutate_at(vars(CI,Manf,OM,Fuel_supply),as.numeric) %>%
    mutate(tech=mgsub::mgsub(tech, c("Solar PV","Solar Thermal power","Wind-offshore","Wind-onshore"),
                                   c("Solar|PV","Solar|CSP","Wind offshore","Wind onshore"))) %>%
    gather_(gather_cols= c("CI","Manf","OM","Fuel_supply","value"),key_col = "activity",value_col= "value") %>% 
    filter(!grepl("average",region))%>% 
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
   input <- read_csv("coal_ef.csv",na="") %>% 
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
  input2 <- data.frame(region=c("India","Latin America","Developing Asia","Middle East"),value=15.1)
  
  input <- read_csv("gas_ef.csv",na="") %>% 
    rename(region=1,value=2,notes=3) %>% 
    select(-notes)  %>% 
    rbind(input2) %>% 
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
    input <- read_csv("regional_mult.csv",na="") %>% 
    rename(region=1) %>% 
    gather(2:4,key="year",value="value") %>% 
    mutate(region=ifelse(region=="Eastern Europe/Eurasia","Eurasia",region))
  
  x <- as.magpie(input,spatial=1,temporal=2)
  
  return (x)
  
  }
  
}

   