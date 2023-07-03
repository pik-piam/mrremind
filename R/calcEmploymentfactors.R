#' @title calcEmploymentfactors
#' @description Collects and combines employment factors for different technologies and activities, from different sources. For all activities except Fuel_supply, units are Full Time Equivalent (FTE)/MW. For Fuel supply, units are FTE/PJ; for nuclear FTE/GWh. The param improvements denotes specific country-level data and is considered an improvement over stock data from rutovitz2015(none)
#' @author Aman Malik
#' @param improvements Either "None", "CEEW", "Dias", "Rutovitz_aus","Solar_found" or "All". Use "All" for all improvements. 
#' @param multiplier source of regional multiplier. Either "Ram", "Rutovitz", "own" or "static". By default use "own"

#' @return employment factors aggregated by region for certain techs and activities

calcEmploymentfactors <- function(improvements,multiplier){
  if (improvements=="None"||improvements=="CEEW"||improvements=="Dias"||improvements=="Rutovitz_aus"||improvements=="Solar_found"||improvements=="All"){
    mapping <- toolGetMapping(type = "regional",name = "regionalmappingWEO2014.csv", where = "mappingfolder")
    mapping_remind <- toolGetMapping(type = "regional",name = "regionmappingH12.csv", where = "mappingfolder")
    colnames(mapping) <- c("region","country")
    mapping$country <- toolCountry2isocode(mapping$country)  
    
    oecd <- c("OECD Europe","OECD Americas","OECD Asia Oceania")
    oecd_con <- mapping[mapping$region %in% oecd,]$country
    # all non-OECD countries (difference taken between countries listed in remindH12 mapping file and IEA region/country definition assumed in Rutovitz et al.)
    non_oecd <- setdiff(mapping_remind$CountryCode,oecd_con)
    
    
    #x1->oecd efs, x2 -> regional multiplier, x3-> regional_ef, x4-> coal_ef, x5-> gas_ef, x6-> IND specofoc EF,
    #x7-> EU_specific_coal EF, x9-> AUS specific EF, x10-> US_specific EF
    
    x1 <- readSource(type = "Rutovitz2015",subtype = "oecd_ef")[,2020,]# EFs for OECD countries in 2020. The 2020 values taken from Rutovitz.
   
    if (multiplier=="Rutovitz"){
      x2 <- readSource(type = "Rutovitz2015",subtype = "regional_mult") # regional multipliers for non-OECD countries
      
      # for countries left out by rutovitz2015, they should get values depending on their location. Since these are mostly small island nations, they get values for africa (here ghana is taken as reprs.)
      x2[which(x2==0),] <- as.numeric(x2["GHA",,])
      x2_tmp <- new.magpie(getRegions(x2),c(getYears(x2),"y2050"),getNames(x2))
      x2_tmp[,getYears(x2),] <- x2
      x2_tmp[,2050,] <- 1
      x2_tmp <- time_interpolate(x2_tmp,c(2025,2035,2040,2045),integrate_interpolated_years = T)
      x2 <- x2_tmp
      
    }
    
    if (multiplier=="Ram"){
      x2 <- readSource("Ram")
    }
    
    if (multiplier=="own"||multiplier=="static"){
      # LP is labor productivity
      y <- readSource("ConferenceBoard")
      #y <- collapseNames(y,collapsedim = 2)
      y_lp <- y[,"y2019","Output per person (without agriculture)"]
      y_lp <- y_lp/ magclass::colMeans(y_lp[oecd_con,,],dims = 1) # all LP relative to average oecd LP
      x2 <- new.magpie(mapping_remind$CountryCode,seq(2015,2050,5)) # will eventually be the regional multiplier from 2015 to 2050
      x2[,2020,] <- 1/y_lp # 2020 regional multiplier based on inverse of 2020 labour productivity
      x2[oecd_con,2020,] <- 1 # all oecd countries have the same multiplier because they have the same Employment factor 
      y_gdpcc <- calcOutput("GDPpc", naming = "scenario", aggregate = F)[ , seq(2015, 2050, 5), "SSP2"] # SSP2 gdp per capita projections
      y_gdpcc <- y_gdpcc/setYears(y_gdpcc[,2020,],NULL)# relative to 2020 since we only want the growth and not absolute values
      x2[,c(2015,seq(2025,2050,5)),] <- setYears(x2[,2020,],NULL)*(1/y_gdpcc[,c(2015,seq(2025,2050,5))])
      
      }

    x3 <- readSource(type = "Rutovitz2015",subtype = "regional_ef")[,2015,] # EFs for specific regions and techs
    getYears(x3) <- 2020
    # x4 <- readSource(type = "Rutovitz2015",subtype = "coal_ef")[,2015,]# EFs for coal fuel supply
    # getYears(x4) <- 2020
    # Coal fuel_supply employment factors
    x4 <- calcOutput("CoalLabourProductivity",subtype = "Employment_factor",aggregate = F)[,2020,]
    # Gas fuel supply employment factors
    x5 <- readSource(type = "Rutovitz2015",subtype = "gas_ef")[,2015,]# EFs for gas fuel supply
    getYears(x5) <- 2020
    
    if (improvements=="CEEW"||improvements=="All"){
      x6 <- readSource(type = "CEEW",subtype = "Employment factors") # EFs for India from CEEW
      getRegions(x6) <- "IND"
      getYears(x6) <- 2020
    }
    
    if (improvements=="Dias"||improvements=="All"){
      x7 <- readSource(type = "Dias", subtype = "Employment factors") # EFs for coal and coal mining for EU
      # using Dias et al. data, regions with non-zero values 
      getYears(x7) <- 2020
      regs <- getRegions(x7)[which(x7>0)]
      x7 <- x7[regs,,]
    }
    
    if (improvements=="Rutovitz_aus"||improvements=="All"){
       # Data from Renewable Energy Jobs in Australia: Stage One, June 2020 by Rutovitz et al.
      x9 <- new.magpie(cells_and_regions = "AUS",2020, 
                       names = c("Wind onshore","Solar|PV-utility","Solar|PV-rooftop","Storage|Battery","Hydro-large"))
      x9 <- add_dimension(x9,dim = 3.2,nm = c("CI","OM"))
      x9[,,"Wind onshore",pmatch=T] <- c(2.8,0.2)
      x9[,,"Solar|PV-utility",pmatch=T] <- c(2.3,0.1)
      x9[,,"Solar|PV-rooftop",pmatch=T] <- c(5.8,0.2)
      x9[,,"Storage|Battery",pmatch=T] <- c(4.7,1.2)
      x9[,,"Hydro-large",pmatch=T] <- c(6,0.15)

    }
    
    if (improvements=="Solar_found"||improvements=="All"){ 
      # from “National Solar Jobs Census 2018, The Solar Foundation, available at: SolarJobsCensus.org” for USA
      # oil and gas EF from the total oil and gas extraction jobs (US bureua of Labour statistics) and BP Production 2020
      x10 <- new.magpie(cells_and_regions = "USA",2020, 
                       names = c("Solar|PV-utility.CI","Solar|PV-rooftop.CI","Hydro-large.OM","Hydro-small.OM","Oil.Fuel_supply","Gas.Fuel_supply"))
      
      x10[,,] <- c(3.3,31,0.1,2.5,3.3,3.3) # 31 is taken as an average for residential (38.7) and non-residential (21.9)
      # hydro numbers from Job Creation Opportunities in Hydropower, Navigant consulting 2009 for USA
    }
    

    
    # solar pv numbers are actually for utility scale
    getNames(x1) <- gsub(x = getNames(x1),replacement = "Solar|PV-utility",pattern = "Solar\\|PV")
    getNames(x3) <- gsub(x = getNames(x3),replacement = "Solar|PV-utility",pattern = "Solar\\|PV")
    
    # Changes for OECD countries in 2020 using updated data
    x1[,,"Solar|PV-utility.CI"] <- 3.2 # from IRENA (2017), Renewable energy benefits: Leveraging local capacity for solar PV,
    x1[,,"Solar|PV-utility.Manf"] <- 3.4 # from IRENA (2017), Renewable energy benefits: Leveraging local capacity for solar PV,
    x1[,,"Wind onshore.CI"] <- 3.3 # from IRENA (2017)   Renewable energy benefits: Leveraging capacity for wind onshore
    x1[,,"Wind onshore.Manf"] <- 1.7 # 
    x1[,,"Wind onshore.OM"] <- 0.19 # Fragkos is the average data for OM from Fragkos and Paroussos, "Employment creation in EU related to renewables expansion", Applied Energy, 2018
      # only those OM values are used which deviate significantly from Rutovitz et al. These are assumed to be for all OECD countries
    x1[,,"Solar|PV-utility.OM"] <- 0.15 # Fragkos
    x1[,,"Biomass.OM"] <- 0.25 # Fragkos
    
    # differentiate Solar PV into utility and rooftop
      x1 <- add_columns(x1,"Solar|PV-rooftop", dim = 3.1) 
    # Rooftop gets 2 X Utility values, but for CI and OM only - from Supplementary info Ram et al. who cite a Solar Power Europe 2020 study
    x1[,,"Solar|PV-rooftop"][,,c("OM","CI")] <- 2*x1[,,"Solar|PV-utility"][,,c("OM","CI")]
    # for Manf, values same as spv utility. Fuel supply is anyway 0
    x1[,,"Solar|PV-rooftop"][,,c("Manf","Fuel_supply")] <- x1[,,"Solar|PV-utility"][,,c("Manf","Fuel_supply")]
    x1 <- add_columns(x1,"Storage|Battery",dim = 3.1)
    
    # add battery storage jobs
    x1[,,"Storage|Battery.Manf"] <- 16.9 # values for Battery storage large scale from Supplementary info. (sheet Employment factors) Ram et al. 2019
    x1[,,"Storage|Battery.CI"] <- 10.8
    x1[,,"Storage|Battery.OM"] <- 0.4
    x1[,,"Storage|Battery.Fuel_supply"] <- 0 # No fuel supply component
    x1[,,"Hydro-large.CI"] <- 5  # changed because REMIND numbers in 2020 are much higher than IRENA numbers (from 7.4 to 5)
    x1[,,"Hydro-large.OM"] <- 0.1     # changed because REMIND numbers in 2020 are much higher than IRENA numbers (from 0.2 to 0.1)
    x1[,,"Hydro-small.OM"] <- 2.5 # changed because REMIND numbers in 2020 are much higher than IRENA numbers (from 4.9 to 2)
    x1[,,"Biomass.Fuel_supply"] <- 25
    
    
    ###### Step 1-3 are  for 2020 ONLY!!
    # Step 1: all countries get OECD values, DEU as reprs.
    x1[,,] <- x1["DEU",,]
  
    # Step 2: Multiply all non-oecd countries by a regional multiplier, relative to OECD
    #  for 2020 
    if (multiplier=="Rutovitz"){
      x1[non_oecd,,] <- x1[non_oecd,,] * setNames(x2[non_oecd,2020,],nm = NULL)
    }
      
    if(multiplier=="Ram"){
      x1[non_oecd,,] <- x1[non_oecd,,] * setNames(x2[non_oecd,2020,],nm = NULL)
  
    }
    
    if(multiplier=="own"||multiplier=="static"){
      x1[non_oecd,,] <- x1[non_oecd,,] * setNames(x2[non_oecd,2020,],nm = NULL)
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
    x1[,,"Coal.Fuel_supply"] <- setNames(x4[,,],NULL)
  
  
    # 3c. Replacing gas and oil EFs in x1 with those of x5, only for 2020
      for (i in getRegions(x5)){
        for (j in getNames(x5)){
          if (x5[i,,j]!=0)
            (x1[i,,j] <- x5[i,,j])
        }
      }
    # for india
    if (improvements=="CEEW"||improvements=="All"){
      x1[getRegions(x6),,getNames(x6)] <- x6[,,] 
    }
    # For EU - coal o&m
    if (improvements=="Dias"||improvements=="All"){
      x1[regs,,getNames(x7)] <- x7[regs,,]
    }
    # For AUS - battery, hydro etc.
    if (improvements=="Rutovitz_aus"||improvements=="All"){
      x1[getRegions(x9),,getNames(x9)] <- x9[,,]
    }
    # For USA - solar
    if (improvements=="Solar_found"||improvements=="All"){
      x1[getRegions(x10),,getNames(x10)] <- x10[,,]
    }
    
    ## Step 4: Regional multipliers for year > 2020, normalised to 2020 and extrapolated until 2050 (only for Rutovitz)
  
    x1_tmp <- new.magpie(getRegions(x1),seq(2015,2050,5),getNames(x1),fill=1)
     x1_tmp[,getYears(x1),] <- x1
     x1 <- x1_tmp
     x2[,,] <- x2[,,]/setYears(x2[,"y2020",],NULL)
     if (multiplier=="Ram"){x2[oecd_con,,] <- 1} # EF for OECD countries does not change
     if(multiplier=="static"){     # efs in all future years for all countries remains same as in 2020
       x2[,seq(2020,2050,5),] <- x2[,2020,]
     }
     # future values based on regional multipliers
     x1[,,] <- setNames(x2[,,],NULL)*setYears(x1[,2020,],NULL)


  x1 <- collapseNames(x1)
  
}
# for regional aggregation. The weights for different technologies are based on
# either the total production/generation of that fuel/resource or employment
gen <- readSource("BP",subtype = "Generation")
prod <- readSource("BP",subtype = "Production")
wt <- x1
y <- readSource("ConferenceBoard")
wt[,,c("Solar|PV","Storage"),pmatch = T] <- gen[,"y2019","Generation|Solar (TWh)"]
wt[,,"Hydro",pmatch = T] <- gen[,"y2019","Generation|Hydro (TWh)"]
wt[,,c("Coal.OM","Coal.CI","Coal.Manf")] <- gen[,"y2019","Generation|Electricity|Coal (TWh)"]
wt[,,c("Gas.OM","Gas.CI","Gas.Manf")] <- gen[,"y2019","Generation|Electricity|Gas (TWh)"]
wt[,,c("Oil.OM","Oil.CI","Oil.Manf")] <- gen[,"y2019","Generation|Electricity|Oil (TWh)"]
wt[,,"Wind",pmatch = T] <- gen[,"y2019","Generation|Wind (TWh)"]
wt[,,"Nuclear",pmatch = T] <- gen[,"y2019","Generation|Nuclear (TWh)"]
wt[,,"Coal.Fuel_supply"] <- prod[,"y2019","Coal Production (EJ)"]
wt[,,"Gas.Fuel_supply"] <- prod[,"y2019","Gas Production (EJ)"]
wt[,,"Oil.Fuel_supply"] <- prod[,"y2019","Oil Production (million t)"]
wt[,,"Biomass.Fuel_supply"] <- y[,"y2019","Employment in agriculture"]


 # using gdp per capita fpr regional aggregation
 # gdp <-   calcOutput("GDP",   years=2020, aggregate = F)
 # gdp <- gdp[,,"gdp_SSP2"]

 # pop <-  calcOutput("Population",aggregate = F)
 # pop <- pop[,seq(2015,2050,5),"pop_SSP2"]

 # gdppercap <- gdp/pop

 return(list(x=x1, weight=wt,  unit="FTE/MW or FTE/PJ",
             description="Employment factors"))
 
}
