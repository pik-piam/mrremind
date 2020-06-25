#' @title calcEmploymentfactors
#' @description Employment factors for different technologies and activities. For all activities except Fuel_supply, units
#' are Full Time Equivalent (FTE)/MW. For Fuel supply, units are Jobs/PJ
#' @author Aman Malik
#' @param improvements Either "None", CEEW", "Dias" or "Dias+CEEW". The latter three are "improvements" over only Rutovitz (None).

calcEmploymentfactors <- function(improvements){

  rutovitz_common <- function(){
    # Common data for Rutovitz
    mapping <- toolMappingFile(type = "regional",name = "regionalmappingWEO2014.csv",readcsv = T)
    mapping_remind <- toolMappingFile(type = "regional",name = "regionmappingH12.csv",readcsv = T)
    colnames(mapping) <- c("region","country")
    mapping$country <- toolCountry2isocode(mapping$country)  
    
    oecd <- c("OECD Europe","OECD Americas","OECD Asia Oceania")
    oecd_con <- mapping[mapping$region %in% oecd,]$country
    # all countries not in OECD (difference taken between countries listed in remindH12 mapping file and 
    # IEA region/country definition assumed in Rutovitz et al.)
    non_oecd <- setdiff(mapping_remind$CountryCode,oecd_con)

    x1 <- readSource(type = "Rutovitz2015",subtype = "oecd_ef")[,2015,]# EFs for OECD countries
   
    x2 <- readSource(type = "Rutovitz2015",subtype = "regional_mult") [,2015,]# regional multipliers for non-OECD countries
   
     # countries for which regional multiplier is 0. There are actually no "0" values, but some countries are left out
    # by Rutovitz which automatically get 0 value after the end of the convert function.
    ## all those countries with no regional multipliers get values for Africa (here Ghana taken as representative)
    x2[which(x2==0),] <- as.numeric(x2["GHA",,])
    
    # interpolating and extrapolating regional multipliers. Value in 2050 is 1, i.e., same as OECD
  
    x3 <- readSource(type = "Rutovitz2015",subtype = "regional_ef")[,2015,] # EFs for specific regions and techs
    x4 <- readSource(type = "Rutovitz2015",subtype = "coal_ef")[,2015,]# EFs for coal fuel supply
    x5 <- readSource(type = "Rutovitz2015",subtype = "gas_ef")[,2015,]# EFs for gas fuel supply
    
    ###### Step 1-3 are ONLY for 2015!!
      
    # Step 1: Give all non-oecd countries, oecd ef values for 2015
    for (i in non_oecd){
    x1[i,,] <- as.numeric(x1["DEU",,]) # arbitary OECD country value
    }
    #x1[non_oecd,,] <- 1 # replacing 0 (value) with 1 for multiplication later
    
    # Step 2: Multiply all non-oecd values by a regional multiplier
    #  for 2015 (for oecd countries, this factor is 1)
    x1[non_oecd,,] <- x1[non_oecd,,] * setNames(x2[non_oecd,,],nm = NULL) 
  
  # Step 3: overwrite efs (values) for 2015 for countries for which there is
  # better data
  
  # 3a.Replacing EFs in x1 for common elements in x1 and x3, only for 2015
    for (i in getRegions(x3)){
      for (j in getNames(x3)){
        if (x3[i,,j]!=0) # for all non-zero values in x3, replace data in x1 with x3
        (x1[i,,j] <- x3[i,,j])
      }
    }
  
  
  # 3b. Replacing coal fuel supply EFs in x1 with those of x4, only for 2015
      for (i in getRegions(x4)){
        for (j in getNames(x4)){
          if (x4[i,,j]!=0)
            (x1[i,,j] <- x4[i,,j])
        }
      }
  
  
  # 3c. Replacing gas EFs in x1 with those of x5,only or 2015
      for (i in getRegions(x4)){
        for (j in getNames(x5)){
          if (x5[i,,j]!=0)
            (x1[i,,j] <- x5[i,,j])
        }
      }
  # Step 4: For future values the regional multiplication factors 
  # which are provided for 2020 and 2030 are normalised to 2015 values and interpolcated/extrapolated until 2050
  # x2[,"y2020",] <- x2[,"y2020",]/setYears(x2[,"y2015",],NULL)
  # x2[,"y2030",] <- x2[,"y2030",]/setYears(x2[,"y2015",],NULL)
  x2 <- readSource(type = "Rutovitz2015",subtype = "regional_mult")# regional multipliers for non-OECD countries
  x2[which(x2==0),] <- as.numeric(x2["GHA",,])
  x2_tmp <- new.magpie(getRegions(x2),c(getYears(x2),"y2050"),getNames(x2))
  x2_tmp[,getYears(x2),] <- x2
  x2_tmp[,2050,] <- 1
  x2_tmp <- time_interpolate(x2_tmp,c(2025,2035,2040,2045),integrate_interpolated_years = T)
  x2 <- x2_tmp
  
  x2[,,] <- x2[,,]/setYears(x2[,"y2015",],NULL)

  # Step 5: Future values found by multiplying with 2015 values
  # adding future/missing years to x1
  x1_tmp <- new.magpie(getRegions(x1),seq(2015,2050,5),getNames(x1),fill=1)
  x1_tmp[,getYears(x1),] <- x1
  x1 <- x1_tmp
  x1[,,] <- setNames(x2[,,],NULL)*setYears(x1[,2015,],NULL)
  
  # x1[non_oecd,2020,] <- setYears(x1[non_oecd,2015,],NULL) * setNames(x2[non_oecd,2020,],NULL) # multiplying by regional multiplier
  # x1[non_oecd,2030,] <- setYears(x1[non_oecd,2015,],NULL) * setNames(x2[non_oecd,2030,],NULL) # multiplying by regional multiplier
  # # For OECD countries, the multiplier is 1 in 2020 and 2030
  # x1[oecd_con,"y2020",]<- as.numeric(x1[oecd_con,"y2015",]) 
  # x1[oecd_con,"y2030",]<- as.numeric(x1[oecd_con,"y2015",]) 
  
 # x1 <- collapseNames(x1)
 
  
  return (x1)
  } 
  
  ceew <- function(){  
  x6 <- readSource(type = "CEEW",subtype = "Employment factors") # EFs for India from CEEW
  # Replacing Large Hydro with Hydro (Note!!)
  getNames(x6) <- gsub(x = getNames(x6),pattern = "Large Hydro",replacement = "Hydro")
  getRegions(x6) <- "IND"
  getYears(x6) <- 2015
  
  factors <- x1[,,]/setYears(x1[,2015,],NULL)
  
  for (i in getRegions(x6)){
    for (j in intersect(getNames(x6),getNames(x1))){
      x1[i,,j] <- setYears(x6[i,,j],NULL)*factors[i,,j]
    }
  }
  # 2020 value same as 2015 value
  x1[getRegions(x6),2020,intersect(getNames(x6),getNames(x1))] <- as.numeric(x1[getRegions(x6),2015,intersect(getNames(x6),getNames(x1))])
  
  
  # overwriting Rutovitz (x1) values with "better" or region-specific data
  # from CEEW (India) for some common variables
  # note than all years from x1 (2015,2020,2030)  get CEEW values
  # com_var <- getNames(x1)[getNames(x1) %in% getNames(x6)]
  # x1["IND",,com_var] <- x6["GLO",,com_var]
  return (x1)
  }
  
  dias <- function(){
    x7 <- readSource(type = "Dias", subtype = "Employment factors") # EFs for coal and coal mining
    # using Dias et al. data, regions with non-zero values 
    regs <- getRegions(x7)[which(x7>0)]
    # Overwriting all efs in x1 for which better data exists in x7
    # for all years
    x1[regs,,getNames(x7)] <- setYears(x7[regs,,],NULL)
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
  # gdp <-   calcOutput("GDPppp",   years=2015, aggregate = F)
  # gdp <- gdp[,,"gdp_SSP2"]
  # 
  pop <-  calcOutput("Population",aggregate = F) 
  pop <- pop[,seq(2015,2050,5),"pop_SSP2"]
  # 
  # gdppercap <- gdp/pop

return(list(x=x1, weight=pop,  unit="FTE/MW or FTE/PJ",
            description="Employment factors"))
}

