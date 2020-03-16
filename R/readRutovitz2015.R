#' Employment factors for various power production technologies from Rutovitz et al. 2015
#' @author Aman Malik
#' @importFrom  tidyr gather_
#' @importFrom dplyr rename add_row filter_ mutate_ select_ mutate_at left_join filter mutate
#' @importFrom mgsub mgsub
#' @importFrom readr read_csv
#' @return magpie object of emplyoment factors for different technologies in Jobs/MW
#' @param subtype Either "oecd_ef","regional_ef","coal_ef","gas_ef", "regional_mult



readRutovitz2015 <- function(subtype){
  
  CI <- NULL
  Fuel_supply <- NULL
  Manf <- NULL
  OM <- NULL
  activity <- NULL
  region <- NULL
  tech <- NULL
  duration <- NULL
  Productivity <- NULL
  Year <- NULL
  notes <- NULL
  value <- NULL
  if (subtype == "oecd_ef")
    {
    
    input <- read_csv("oecd_ef.csv",na = "") %>% 
      # rename_(tech=~"X1",duration=~`Construction times`,CI=~`Construction/ Installation`,
      #         Manf=~`Manufacturing`,OM=~`Operations & Maintenance`, Fuel_supply=`Fuel – PRIMARY ENRGY DEMAND\nEnergy Demand`) %>% 
      rename(tech=1,duration=2,CI=3,Manf=4,OM=5, Fuel_supply=6) %>% 
      filter_(~!is.na(tech)) %>% 
      add_row(tech="CoalHP",duration=5,CI=11.2,Manf=5.4,OM=0.14*1.5,Fuel_supply="Regional") %>% 
      add_row(tech="GasHP",duration=2,CI=1.3,Manf=0.93,OM=0.14*1.5,Fuel_supply="Regional") %>% 
      add_row(tech="BiomassHP",duration=2,CI=14,Manf=2.9,OM=1.5*1.5,Fuel_supply=29.9) %>% 
      add_row(tech="Oil",CI=1.3,Manf=0.93,OM=0.14,Fuel_supply="Regional") %>% # oil EF= Gas EF
      filter_(~!tech %in% grep("Ocean|decommissioning|small|Oil|heat",
                             x =tech,value=T)) %>%  # removing 
    mutate_(tech=~mgsub(tech, c("Hydro-large", "Wind onshore","Solar Photovoltaics","Solar thermal"),
                             c("Hydro", "Wind","Solar|PV","Solar|CSP"))) %>%  ## renaming techs
     # mutate(CI=as.numeric(CI)) %>% 
      #mutate(Manf=as.numeric(Manf)) %>% 
    mutate_at(vars(CI,Manf,OM,duration),as.numeric) %>% 
      #mutate_(CI=~CI/duration) %>% # dividing employment intensity by construction period
      #mutate_(Manf=~Manf/duration) %>% 
      select_(~-duration) %>% 
      gather_(gather_cols= c("CI","Manf","OM","Fuel_supply","value"),key_col = "activity",value_col= "value") %>% 
      mutate(value=ifelse(value == "Regional",0,value))  %>% 
      #mutate_(unit=ifelse(activity %in% c("CI","Manf"), yes = "Job-years/MW", no = ifelse(activity=="Fuel_supply","Jobs/PJ","Jobs/MW"))) %>% # adding units column
     # filter_( ~tech!="Nuclear") %>% 
      #mutate(life_span = ifelse(tech=="Coal",40,
       #                         ifelse(tech=="Gas",30,
        #                               ifelse(tech %in% c("Nuclear","Hydro"),60,
         #                                     ifelse(tech %in% c("Solar|PV","Solar|CSP","Wind"),25,30))))) %>% 
      mutate_at(vars(tech,activity),as.factor) %>% 
      mutate(value=as.numeric(value))
    
   
    
      x <- as.magpie(input,temporal=NULL,spatial=NULL,datacol=3)
    return (x)
  }
  

  if (subtype == "regional_ef")
    {
    
  const <- data.frame(tech=c("Nuclear","Biomass","Hydro","Wind","Solar|PV","Solar|CSP","Wind offshore"),
                        duration=c(10,2,2,2,1,2,4))
  input <- read_csv("regional_ef.csv",na="") %>%
    rename(tech=1,region=2,CI=3,Manf=4,OM=5,Fuel_supply=6) %>%
    filter(!is.na(tech)) %>%
    mutate_at(vars(tech,region),as.character) %>%
    mutate_at(vars(CI,Manf,OM,Fuel_supply),as.numeric) %>%
    mutate(tech=mgsub(tech, c("Hydro-large", "Wind-onshore","Solar PV","Solar Thermal power","Wind-offshore"),
                                   c("Hydro", "Wind","Solar|PV","Solar|CSP","Wind offshore"))) %>%
    gather_(gather_cols= c("CI","Manf","OM","Fuel_supply","value"),key_col = "activity",value_col= "value") %>% 
    filter(!grepl("average",region))%>% 
    mutate(region=mgsub(region,c("OECD North America","OECD Pacific"),
                        c("OECD Americas","OECD Asia Oceania"))) %>% 
    na.omit() %>% 
    left_join(const,by="tech") %>% 
    #mutate(region=ifelse(region=="OECD North America","OECD Americas",region))
    mutate(value=ifelse(!activity %in% c("Fuel_supply","OM"),value/duration,value)) %>% 
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

   