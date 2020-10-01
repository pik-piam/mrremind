#' @title calcEmploymentfactors
#' @description Employment factors for different technologies and activities. For all activities except Fuel_supply, units
#' are Full Time Equivalent (FTE)/MW. For Fuel supply, units are Jobs/PJ; for nuclear Jobs/GWh
#' @author Aman Malik
#' @param improvements Either "None", "CEEW", "Dias", "Rutovitz_aus","Solar_found" or "All". Use "All" for all improvements.
#' @param multiplier source of regional multiplier. Either "Ram" or "Rutovitz"

calcEmploymentfactors <- function(improvements,multiplier){
  if (improvements=="None"||improvements=="CEEW"||improvements=="Dias"||improvements=="Rutovitz_aus"||improvements=="Solar_found"||improvements=="All"){
    mapping <- toolMappingFile(type = "regional",name = "regionalmappingWEO2014.csv",readcsv = T)
    mapping_remind <- toolMappingFile(type = "regional",name = "regionmappingH12.csv",readcsv = T)
    colnames(mapping) <- c("region","country")
    mapping$country <- toolCountry2isocode(mapping$country)  
    
    oecd <- c("OECD Europe","OECD Americas","OECD Asia Oceania")
    oecd_con <- mapping[mapping$region %in% oecd,]$country
    # all countries not in OECD (difference taken between countries listed in remindH12 mapping file and 
    # IEA region/country definition assumed in Rutovitz et al.)
    non_oecd <- setdiff(mapping_remind$CountryCode,oecd_con)
    
    x1 <- readSource(type = "Rutovitz2015",subtype = "oecd_ef")[,2020,]# EFs for OECD countries in 2020
   
    if (multiplier=="Rutovitz"){
      x2 <- readSource(type = "Rutovitz2015",subtype = "regional_mult") [,2020,]# regional multipliers for non-OECD countries
      
      # countries for which regional multiplier is 0. There are actually no "0" values, but some countries are left out
      # by Rutovitz which automatically get 0 value after the end of the convert function.
      ## all those countries with no regional multipliers get values for Africa (here Ghana taken as representative) as these are mostly small island nations
      x2[which(x2==0),] <- as.numeric(x2["GHA",,])
    }
    
    if (multiplier=="Ram"){
      x2 <- readSource("Ram")[,2020,]
    }

    x3 <- readSource(type = "Rutovitz2015",subtype = "regional_ef")[,2015,] # EFs for specific regions and techs
    getYears(x3) <- 2020
    x4 <- readSource(type = "Rutovitz2015",subtype = "coal_ef")[,2015,]# EFs for coal fuel supply
    getYears(x4) <- 2020
    x5 <- readSource(type = "Rutovitz2015",subtype = "gas_ef")[,2015,]# EFs for gas fuel supply
    getYears(x5) <- 2020
    if (improvements=="CEEW"||improvements=="All"){
      x6 <- readSource(type = "CEEW",subtype = "Employment factors") # EFs for India from CEEW
      getRegions(x6) <- "IND"
      getYears(x6) <- 2020
    }
    if (improvements=="Dias"||improvements=="All"){
      x7 <- readSource(type = "Dias", subtype = "Employment factors") # EFs for coal and coal mining
      # using Dias et al. data, regions with non-zero values 
      getYears(x7) <- 2020
      regs <- getRegions(x7)[which(x7>0)]
      x7 <- x7[regs,,]
    }
    
    # if (improvements=="Fragkos"||improvements=="All"){
    #   mapping_remind <- toolMappingFile(type = "regional",name = "regionmappingH12.csv",readcsv = T)
    #   
    #   # Fragkos is the average data for OM from Fragkos and Paroussos, "Employment creation in EU related to renewables expansion", Applied Energy, 2018
    #   # only those OM values are used which deviate significantly from Rutovitz et al.
    #   x8 <- new.magpie(mapping_remind[mapping_remind$RegionCode=="EUR",]$CountryCode,2020, 
    #                       names = c("Solar|PV-utility.OM","Wind onshore.OM", "Biomass.OM","Solar|PV-rooftop.OM"))
    #   x8[,,"Wind onshore.OM"] <- 0.19
    #   x8[,,"Solar|PV-utility.OM"] <- 0.15
    #   x8[,,"Solar|PV-rooftop.OM"] <- 2*0.15
    #   x8[,,"Biomass.OM"] <- 0.25
    # }
    
    if (improvements=="Rutovitz_aus"||improvements=="All"){
       # Data from Renewable Energy Jobs in Australia: Stage One, June 2020 by Rutovitz et al.
      x9 <- new.magpie(cells_and_regions = "AUS",2020, 
                       names = c("Wind onshore","Solar|PV-utility","Solar|PV-rooftop","Storage|Battery","Hydro-large"))
      x9 <- add_dimension(x9,dim = 3.2,nm = c("CI","OM"))
      x9[,,"Wind onshore",pmatch=T] <- c(2.8,0.2)
      x9[,,"Solar|PV-utility",pmatch=T] <- c(2.3,0.1)
      x9[,,"Solar|PV-rooftop",pmatch=T] <- c(5.8,0.2)
      x9[,,"Storage|Battery",pmatch=T] <- c(4.7,1.2)
      x9[,,"Hydro-large",pmatch=T] <- c(7.4,0.1)
      
    }
    
    if (improvements=="Solar_found"||improvements=="All"){
      # from “National Solar Jobs Census 2018, The Solar Foundation, available at: SolarJobsCensus.org”
      x10 <- new.magpie(cells_and_regions = "USA",2020, 
                       names = c("Solar|PV-utility.CI","Solar|PV-rooftop.CI"))
      
      x10[,,] <- c(3.3,31) # 31 is taken as an average for residential (38.7) and non-residential (21.9)
      
    }
    
    ###### Step 1-3 are  for 2020 ONLY!!
    
    # differentiate Solar PV into utility and rooftop
    getNames(x1) <- gsub(x = getNames(x1),replacement = "Solar|PV-utility",pattern = "Solar\\|PV")
    getNames(x3) <- gsub(x = getNames(x3),replacement = "Solar|PV-utility",pattern = "Solar\\|PV")
    
    # Changes for OECD countries in 2020
    x1[,,"Solar|PV-utility.CI"] <- 3.7 # from IRENA (2017), Renewable energy benefits: Leveraging local capacity for solar PV,
    x1[,,"Solar|PV-utility.Manf"] <- 4.4 # from IRENA (2017), Renewable energy benefits: Leveraging local capacity for solar PV,
    x1[,,"Wind onshore.OM"] <- 0.19 # Fragkos is the average data for OM from Fragkos and Paroussos, "Employment creation in EU related to renewables expansion", Applied Energy, 2018
    # only those OM values are used which deviate significantly from Rutovitz et al. These are assumed to be for all OECD countries
    x1[,,"Solar|PV-utility.OM"] <- 0.15 # Fragkos
    x1[,,"Biomass.OM"] <- 0.25 # Fragkos
    
    x1 <- add_columns(x1,"Solar|PV-rooftop", dim = 3.1) 
    # Rooftop gets 2 X Utility values, but for CI and OM only - from Supplementary info Ram et al. who cites a Solar Power Europe 2020 study
    x1[,,"Solar|PV-rooftop"][,,c("OM","CI")] <- 2*x1[,,"Solar|PV-utility"][,,c("OM","CI")]
    # for Manf, values same as spv utility. Fuel supply is anyway 0
    x1[,,"Solar|PV-rooftop"][,,c("Manf","Fuel_supply")] <- x1[,,"Solar|PV-utility"][,,c("Manf","Fuel_supply")]
    x1 <- add_columns(x1,"Storage|Battery",dim = 3.1)
    x1[,,"Storage|Battery.Manf"] <- 16.9 # values for Battery storage large scale from Supplementary info. (sheet Employment factors) Ram et al. 2019
    x1[,,"Storage|Battery.CI"] <- 10.8
    x1[,,"Storage|Battery.OM"] <- 0.4
    x1[,,"Storage|Battery.Fuel_supply"] <- 0 # No fuel supply component
    
    # Step 1: all countries get OECD values
    x1[,,] <- x1["DEU",,]

    # Step 2: Multiply all non-oecd countries by a regional multiplier
    #  for 2020 (for oecd countries, this factor is 1)
    if (multiplier=="Rutovitz"){
      x1[non_oecd,,] <- x1[non_oecd,,] * setNames(x2[non_oecd,,],nm = NULL)
    }
      
    if(multiplier=="Ram"){
      x1[non_oecd,,] <- x1[non_oecd,,] * setNames(x2[non_oecd,,],nm = NULL)
  
     }
    # Step 3: overwrite efs (values) for 2020 for countries for which there is
    # better data
    
    # 3a.Replacing EFs in x1 for common elements in x1 and x3, only for 2020
    for (i in getRegions(x3)){
      for (j in setdiff(getNames(x3),c("Solar|PV-utility.CI","Solar|PV-utility.OM"))){ # all except solar pv utility for OECD North America as this seems to be over-estimated
        if (x3[i,,j]!=0) # for all non-zero values in x3, replace data in x1 with x3
        (x1[i,,j] <- x3[i,,j])
      }
    }
  
  
    # 3b. Replacing coal fuel supply EFs in x1 with those of x4, only for 2020
    for (i in getRegions(x4)){
      for (j in getNames(x4)){
        if (x4[i,,j]!=0)
            (x1[i,,j] <- x4[i,,j])
      }
    }
  
  
    # 3c. Replacing gas and oil EFs in x1 with those of x5, only or 2020
      for (i in getRegions(x5)){
        for (j in getNames(x5)){
          if (x5[i,,j]!=0)
            (x1[i,,j] <- x5[i,,j])
        }
      }
  
    if (improvements=="CEEW"||improvements=="All"){
      x1[getRegions(x6),,getNames(x6)] <- x6[,,] 
    }
    
    if (improvements=="Dias"||improvements=="All"){
      x1[regs,,getNames(x7)] <- x7[regs,,]
    }
    
    # if(improvements=="Fragkos"||improvements=="All"){
    #   x1[getRegions(x8),,getNames(x8)] <- x8[,,]
    # }
    
    if (improvements=="Rutovitz_aus"||improvements=="All"){
      x1[getRegions(x9),,getNames(x9)] <- x9[,,]
    }
    
    if (improvements=="Solar_found"||improvements=="All"){
      x1[getRegions(x10),,getNames(x10)] <- x10[,,]
    }
    
    ## Step 4: Regional multipliers for year > 2020, normalised to 2020 and extrapolated until 2050 (only for Rutovitz)
  
    if (multiplier=="Rutovitz"){  
      x2 <- readSource(type = "Rutovitz2015",subtype = "regional_mult")# regional multipliers for non-OECD countries
      x2[which(x2==0),] <- as.numeric(x2["GHA",,])
      x2_tmp <- new.magpie(getRegions(x2),c(getYears(x2),"y2050"),getNames(x2))
      x2_tmp[,getYears(x2),] <- x2
      x2_tmp[,2050,] <- 1
      x2_tmp <- time_interpolate(x2_tmp,c(2025,2035,2040,2045),integrate_interpolated_years = T)
      x2 <- x2_tmp
      x2[,,] <- x2[,,]/setYears(x2[,"y2020",],NULL)
      
      ## Step 5: For years > 2020, EF (T)= EF_2020 X Regional multiplier (T)
      
      x1_tmp <- new.magpie(getRegions(x1),seq(2015,2050,5),getNames(x1),fill=1)
      x1_tmp[,getYears(x1),] <- x1
      x1 <- x1_tmp
      x1[,,] <- setNames(x2[,,],NULL)*setYears(x1[,2020,],NULL)
    }
   
    if (multiplier=="Ram"){
     x2 <- readSource("Ram")
     x1_tmp <- new.magpie(getRegions(x1),seq(2015,2050,5),getNames(x1),fill=1)
     x1_tmp[,getYears(x1),] <- x1
     x1 <- x1_tmp
     x2[,,] <- x2[,,]/setYears(x2[,"y2020",],NULL)
     x2[oecd_con,,] <- 1 # EF for OECD countries do not change
     x1[,,] <- setNames(x2[,,],NULL)*setYears(x1[,2020,],NULL)
     
    }
  
  x1 <- collapseNames(x1)
  
}


 # using gdp per capita fpr regional aggregation
 # gdp <-   calcOutput("GDPppp",   years=2020, aggregate = F)
 # gdp <- gdp[,,"gdp_SSP2"]
 #
 pop <-  calcOutput("Population",aggregate = F)
 pop <- pop[,seq(2015,2050,5),"pop_SSP2"]
 #
 # gdppercap <- gdp/pop

 return(list(x=x1, weight=pop,  unit="FTE/MW or FTE/PJ",
             description="Employment factors"))
 
}
