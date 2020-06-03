#' @title convertRutovitz2015
#' @param x MAgPIE object to be converted
#' @param subtype "oecd_ef" or "regional_mult" or "regional_ef" or "coal_ef" or "gas_ef"
#' @author Aman Malik
#' @importFrom stats na.omit


convertRutovitz2015 <- function(x,subtype) {
  country = NULL
  year = NULL
  region <- NULL
  value <- NULL
  tech <- NULL
  activity <- NULL
  
  mapping <- toolMappingFile(type = "regional",name = "regionalmappingWEO2014.csv",readcsv = T)
  colnames(mapping) <- c("region","country")
  mapping$country <- toolCountry2isocode(mapping$country)  
  
  if (subtype == "oecd_ef")  {
    # x <- readSource(type = "Rutovitz2015",subtype = "oecd_ef",convert = F)
  
  # oecd regions in mapping file
  oecd <- c("OECD Europe","OECD Americas","OECD Asia Oceania")
  # oecd countries from mapping file
  oecd_con <- mapping[mapping$region %in% oecd,]$country
  
  # since x cannot directly be changed to accept OECD countries and years,
  # make a new magpie object
  oecd_ef <- new.magpie(unique(oecd_con),c(2015,2020,2030),names = getNames(x))
  
  # assign all countries and techs, same value as x
  oecd_ef[,,] <- x
  oecd_ef[is.na(oecd_ef)] <- 0
  x <- toolCountryFill(oecd_ef,fill=0)
  }
  if (subtype=="regional_mult"){
    
  # x <- readSource(type = "Rutovitz2015",subtype = "regional_mult",convert=F)
    reg_mult <- as.data.frame(x) %>% 
    select(2,3,5) %>% 
    rename(region=1,year=2,value=3)
    
    mapping$region <- gsub("OECD Europe|OECD Asia Oceania|OECD Americas","OECD",x = mapping$region)
    mapping$region <- gsub("Central Africa|West Africa|Southern Africa|East Africa|North Africa","Africa",x = mapping$region)
    
    
    # assign countries to all regions for regional multipliers
      reg_mult2 <- left_join(reg_mult,mapping,by="region") %>% 
      filter(!(region=="Non-OECD Asia" & country=="CHN")) %>% # removing duplicates
      filter(!(region=="Non-OECD Asia" & country=="IND")) %>% # removing duplicates
      select(country,year,value) 
    
    x <- as.magpie(reg_mult2)
    x <- toolCountryFill(x,fill=0)
    x["CYP",,] <- as.numeric(x["DEU",,]) # cyprus gets oecd values
  
  }
  if (subtype == "regional_ef"){

    # x <- readSource(type = "Rutovitz2015",subtype = "regional_ef",convert = F)
    
    mapping$region <- gsub("Central Africa|West Africa|Southern Africa|East Africa|North Africa","Africa",x = mapping$region)

    
    # assign countries to all regions in Rutovitz for specific regional EFs
    
    x_df <- as.data.frame(x) %>% select(2,4,5,6) %>% 
        rename(region=1,tech=2,activity=3,value=4) %>%
        na.omit() %>% 
        left_join(mapping,by="region") %>% 
        select(country,tech,activity,value) 
    
    x <- as.magpie(x_df,spatial=1,temporal=NULL,datacol=4)
    x[is.na(x)] <- 0
    x <- toolCountryFill(x,fill=0)
    getYears(x) <- 2015
 
  }
  if(subtype=="coal_ef"){
    #x <- readSource(type = "Rutovitz2015",subtype = "coal_ef",convert = F) 
    getRegions(x) <- gsub("OECD North America","OECD Americas",x = getRegions(x))
    getRegions(x) <-  gsub("OECD Pacific","OECD Asia Oceania",x = getRegions(x))
    getRegions(x) <-  gsub("Developing Asia","Non-OECD Asia",x = getRegions(x))
    
    
    # assign countries to all regions in Rutovitz for coal employment factors
      x_df <- as.data.frame(x) %>% 
      select(2,4,5,6) %>% 
      rename(region=1,tech=2,activity=3,value=4) %>% 
      left_join(mapping,by="region") %>% 
      na.omit() %>% 
      select(country,tech,activity,value,-region)
    
    x <- as.magpie(x_df,spatial=1,temporal=NULL,datacol=4)
    getYears(x) <- 2015
    x <- toolCountryFill(x,fill=0)
    
   }
  if(subtype=="gas_ef"){  
 #   x <- readSource(type = "Rutovitz2015",subtype = "gas_ef",convert = F)
    getRegions(x) <- gsub("OECD North America","OECD Americas",x = getRegions(x))
    getRegions(x) <-  gsub("OECD Pacific","OECD Asia Oceania",x = getRegions(x))
    getRegions(x) <-  gsub("Developing Asia","Non-OECD Asia",x = getRegions(x))
    
    mapping$region <- gsub("Central Africa|West Africa|Southern Africa|East Africa|North Africa","Africa",x = mapping$region)
    mapping <- mapping %>% filter(!country %in% getRegions(x) )
    
    x_df<-  as.data.frame(x) %>% 
      select(2,4,5,6) %>% 
      rename(region=1,tech=2,activity=3,value=4) %>% 
      filter(!region %in% unique(mapping$region)) %>% 
      filter(region!="Africa")
    
    x_df$region <- toolCountry2isocode(x_df$region)
    
    x_df_2 <- as.data.frame(x) %>% 
      select(2,4,5,6) %>% 
      rename(region=1,tech=2,activity=3,value=4) %>% 
      filter(region %in% unique(mapping$region)) %>% 
      left_join(mapping,by="region") %>% 
      filter(!country %in% x_df$region) %>% 
      na.omit() %>% 
      select(-region) %>% 
      rename(region=country) %>% 
      select(region,tech,activity,value)
    
      x_df <- bind_rows(x_df,x_df_2)
      
      x <- as.magpie(x_df,spatial=1,temporal=NULL,datacol=4)
      getYears(x) <- 2015
      x <- toolCountryFill(x,fill=0)

    
  }
  
  return (x)
}

