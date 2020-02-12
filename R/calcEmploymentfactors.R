#' @title calcEmploymentfactors
#' Emplyoment factors for different technologies and activities. For all activities except Fuel_supply units
#' are Jobs/MW. For Fuel supply, units are Jobs/PJ
#' @author Aman Malik
#' @param improvements Either "None", CEEW", "Dias" or "Dias+CEEW". The latter three are "improvements" over Rutovitz (None).
calcEmploymentfactors <- function(improvements){

  rutovitz_common <- function(){ # Common data for Rutovitz
    mapping <- toolMappingFile(type = "regional",name = "regionalmappingWEO2014.csv",readcsv = T)
    colnames(mapping) <- c("region","country")
    mapping$country <- toolCountry2isocode(mapping$country)  
    
    oecd <- c("OECD Europe","OECD Americas","OECD Asia Oceania")
    oecd_con <- mapping[mapping$region %in% oecd,]$country
    non_oecd <- mapping[!mapping$country %in% oecd_con,]$country
    non_oecd <- non_oecd[!non_oecd %in% c("KOS","ANT")]
  
    x1 <- readSource(type = "Rutovitz2015",subtype = "oecd_ef")# EFs for OECD countries
    x2 <- readSource(type = "Rutovitz2015",subtype = "regional_mult")# regional multipliers for non-OECD countries
    x3 <- readSource(type = "Rutovitz2015",subtype = "regional_ef")# EFs for specific regions and techs
    x4 <- readSource(type = "Rutovitz2015",subtype = "coal_ef")# EFs for coal fuel supply
    x5 <- readSource(type = "Rutovitz2015",subtype = "gas_ef")# EFs for gas fuel supply
  
  
    # For all non-OECD countries the EFs are zero. Their EFs are calculated by multiplying  their
    # regional multipliers with OECD values.
  
    x1[non_oecd,,] <-  1 # initialising to 1 for non-oecd countries
    x1[non_oecd,,] <- x1["DEU",,] # arbitary OECD country value
    x1[non_oecd,,] <- x1[non_oecd,,] * x2[non_oecd,"y2015",] # multiplying by regional multiplier
  
  
  # Replacing EFs in x1 for common elements in x1 and x3
  
  
    for (i in getRegions(x3)){
      for (j in getNames(x3)){
        if (x3[i,,j]!=0) # for all non-zero values in x3, replace data in x1 with x3
        (x1[i,,j] <- x3[i,,j])
      }
    }
  
  
  # Replacing coal fuel supply EFs in x1 with those of x4
  # 
      for (i in getRegions(x4)){
        for (j in getNames(x4)){
          if (x4[i,,j]!=0)
            (x1[i,,j] <- x4[i,,j])
        }
      }
  
  
  # Replacing gas EFs in x1 with those of x5
      for (i in getRegions(x4)){
        for (j in getNames(x5)){
          if (x5[i,,j]!=0)
            (x1[i,,j] <- x5[i,,j])
        }
      }
        
    return (x1)
  }
  ceew <- function(){  
  x6 <- readSource(type = "CEEW",subtype = "Employment factors", convert = F) # EFs for India from CEEW
  getRegions(x6) <- "IND"
  getNames(x6) <- gsub(x = getNames(x6),pattern = "Large Hydro",replacement = "Hydro")
  
  # using CEEW data
  com_var <- getNames(x1)[getNames(x1) %in% getNames(x6)] # variables common to CEEW and Rutovitz
  x1["IND",,com_var] <- x6[,,com_var]
  return (x1)
  }
  dias <- function(){
    x7 <- readSource(type = "Dias", subtype = "Employment factors", convert = F) # EFs for coal and coal mining
    getRegions(x7) <- toolCountry2isocode(getRegions(x7))
    # using Dias et al. data      
    x1[getRegions(x7),,getNames(x7)] <- x7
    return (x1)
  }
  
  if (improvements=="None")
    {
    x1 <- rutovitz_common()
  }
  if(improvements=="CEEW")
    {
    x1 <-  rutovitz_common()
    x1 <- ceew()
    
  }
  
  if (improvements=="Dias")
    {
    x1 <- rutovitz_common()
    x1 <- dias()
  
  }
  
  if (improvements=="Dias+CEEW")
  {
    x1 <- rutovitz_common()
    x1 <- ceew()  
    x1 <- dias()
  }
  
  # using gdp per capita fpr regional aggregation
  gdp <-   calcOutput("GDPppp",   years=2015, aggregate = F)
  gdp <- gdp[,,"gdp_SSP2"]
  
  pop <-  calcOutput("Population", years=2015,aggregate = F) 
  pop <- pop[,,"pop_SSP2"]
  
  gdppercap <- gdp/pop


return(list(x=x1, weight=gdppercap,  unit="",  description="Employment factors"))
}
