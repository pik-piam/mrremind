#' Output for 2 policy cases
#' @author Aman Malik, Christoph Bertram
#' @param subtype "share_uncond", "share_cond", "multiplier_uncond",  "multiplier_cond", "ghg_uncond" or "ghg_cond"
#' @importFrom magclass getNames
#' @importFrom madrat setConfig getConfig

calcEmiTarget <- function(subtype) {
  
  enable_cache <- getConfig("enablecache")
  setConfig(enablecache=F)
  ### Import NDC information
    if(length(grep("uncond",subtype))==0){##default: conditional (so "uncond" does not appear in subtype string)
       reduction <- readSource("Rogelj2017","Emissions_Red_con",convert = FALSE)
    } else {### only if "uncond" does appear, take the unconditional target values
      reduction <- readSource("Rogelj2017","Emissions_Red_unc",convert = FALSE)
    }
  
    RefYear <- readSource("Rogelj2017","Emissions_Ref",convert = FALSE)
    
    BauRefEmi <- readSource("Rogelj2017","Emissions_Emi",convert = FALSE)
    
    Type <- readSource("Rogelj2017","Emissions_Typ",convert = FALSE)
    
    TarYear <- readSource("Rogelj2017","Emissions_Tar",convert = FALSE)
    
    
    setConfig(enablecache=enable_cache)
    
  ### Import historical data (gdp and emi) needed for the calculations 
    
    # Historical emissions for 1990-2015 - co2 (excl LU),ch4,n2o (so far no Fgas historic time series)
    ceds <- calcOutput("Emissions",datasource="CEDS2REMIND",aggregate = FALSE)
    gwpCH4 <- 28 #"Global Warming Potentials of CH4, AR5 WG1 CH08 Table 8.7"     /28/
    gwpN2O <- 265 #"Global Warming Potentials of N2O, AR5 WG1 CH08 Table 8.7"     /265/
    #calculate GHG total of CO2, CH4 and N2O [unit Mt CO2eq]
    ghg <- ceds[,seq(1990,2015,1),c("Emi|CO2|Energy and Industrial Processes (Mt CO2/yr)")]+
            +gwpN2O/1000*dimSums(ceds[,seq(1990,2015,1),c("Emi|N2O|Energy and Industrial Processes (kt N2O/yr)",
                                                          "Emi|N2O|Land Use|Agriculture and Biomass Burning (kt N2O/yr)",
                                                          "Emi|N2O|Land Use|Forest Burning (kt N2O/yr)",
                                                          "Emi|N2O|Land Use|Grassland Burning (kt N2O/yr)",
                                                          "Emi|N2O|Waste (kt N2O/yr)")],dim=3) +
      +gwpCH4*dimSums(ceds[,seq(1990,2015,1),c("Emi|CH4|Energy and Industrial Processes (Mt CH4/yr)",
                                              "Emi|CH4|Land Use|Agriculture and Biomass Burning (Mt CH4/yr)",
                                              "Emi|CH4|Land Use|Forest Burning (Mt CH4/yr)",
                                              "Emi|CH4|Land Use|Grassland Burning (Mt CH4/yr)",
                                              "Emi|CH4|Waste (Mt CH4/yr)")],dim=3)
    #create global data for checking plausability of data
    glob_ghg <- dimSums(ghg,dim=1)
    
    # Future GDP values
    gdp <- calcOutput("GDPppp",aggregate = FALSE)
    
    
    
  ### calculations
    
    #default setting: exclude countries without reduction target (unconditional case) and those with target relative to BAU and no information on BAU emission
    exclCountries <- NULL
    for (r in getRegions(TarYear)) {
      if(is.na(reduction[r,,])){
        exclCountries = c(exclCountries,r)
      } else if(RefYear[r,,] == "BAU" & is.na(BauRefEmi[r,,])){
        exclCountries = c(exclCountries,r)
      }
      #make reduction numeric
      reduction[r,,] <- as.numeric(reduction[r,,])
    }
    #only consider rest of countries for quantitative target calculation
    targetCountries <- setdiff(getRegions(TarYear),exclCountries)
      reduction <- reduction[targetCountries,,]
      BauRefEmi <- BauRefEmi[targetCountries,,]
      RefYear  <- RefYear[targetCountries,,]
      Type  <- Type[targetCountries,,]
      TarYear  <- TarYear[targetCountries,,]
      
 
      
    #special treatment for EU countries
      
    eu28_countries <- c("POL", "CZE", "ROU", "BGR", "HUN", "SVK", "HRV", "LTU", "EST", "SVN", 
                      "LVA", "DEU", "GBR", "FRA", "ITA", "ESP", "NLD", "BEL", "GRC", "AUT",
                      "PRT", "FIN", "SWE", "IRL", "DNK", "LUX", "CYP", "MLT")  
      
    #for all countries with a quantitative target that can be translated into an emissions trajectory factor, this script will calculate the Factor of target year emissions vs. 2005 emissions
    factor <- gdp[c(getRegions(Type),eu28_countries),unique(TarYear),]
    #set all to 1 as default
    factor[,,] <- 0
    
    #define groups with same target type
      r2030_1 <- NULL
      r2030_2 <- NULL
      r2030_3 <- NULL
      r2030_4 <- NULL
      r2025_1 <- NULL
      r2025_2 <- NULL
      r2025_3 <- NULL
      r2025_4 <- NULL
      r2035_1 <- NULL 
      r2035_2 <- NULL
      r2035_3 <- NULL
      r2035_4 <- NULL
      for (r in getRegions(TarYear)) {
        if(TarYear[r,,] == "2030" & Type[r,,] %in% c("CO2/GDP","GHG/GDP")){
          r2030_1 = c(r2030_1,r)
        }
        if(TarYear[r,,] == "2030" & Type[r,,] %in% c("GHG-Absolute")){
          r2030_2 = c(r2030_2,r)
        }
        if(TarYear[r,,] == "2030" & Type[r,,] %in% c("GHG")){
          r2030_3 = c(r2030_3,r)
        }
        if(TarYear[r,,] == "2030" & Type[r,,] %in% c("GHG/CAP")){
          r2030_4 = c(r2030_4,r)
        }
        if(TarYear[r,,] == "2025" & Type[r,,] %in% c("CO2/GDP","GHG/GDP")){
          r2025_1 = c(r2025_1,r)
        }
        if(TarYear[r,,] == "2025" & Type[r,,] %in% c("GHG-Absolute")){
          r2025_2 = c(r2025_2,r)
        }
        if(TarYear[r,,] == "2025" & Type[r,,] %in% c("GHG")){
          r2025_3 = c(r2025_3,r)
        }
        if(TarYear[r,,] == "2025" & Type[r,,] %in% c("GHG/CAP")){
          r2025_4 = c(r2025_4,r)
        }
        if(TarYear[r,,] == "2035" & Type[r,,] %in% c("CO2/GDP","GHG/GDP")){
          r2035_1 = c(r2035_1,r)
        }
        if(TarYear[r,,] == "2035" & Type[r,,] %in% c("GHG-Absolute")){
          r2035_2 = c(r2035_2,r)
        }
        if(TarYear[r,,] == "2035" & Type[r,,] %in% c("GHG")){
          r2035_3 = c(r2035_3,r)
        }
        if(TarYear[r,,] == "2035" & Type[r,,] %in% c("GHG/CAP")){
          r2035_4 = c(r2035_4,r)
        }
      }

      

      #type 1: CO2/GDP or GHG/GDP: use gdp projections with ssp dependence
      for(regi in r2030_1){
         factor[regi,2030,]  <- (1+reduction[regi,,])*gdp[regi,2030,]/setYears(gdp[regi,round(as.numeric(RefYear[regi,,])/5)*5,],NULL)*setYears(ghg[regi,as.numeric(RefYear[regi,,]),],NULL)/setYears(ghg[regi,2005,],NULL)
      }
      for(regi in r2025_1){
        factor[regi,2025,]  <- (1+reduction[regi,,])*gdp[regi,2025,]/setYears(gdp[regi,round(as.numeric(RefYear[regi,,])/5)*5,],NULL)*setYears(ghg[regi,as.numeric(RefYear[regi,,]),],NULL)/setYears(ghg[regi,2005,],NULL)
      }
      for(regi in r2035_1){
        factor[regi,2035,]  <- (1+reduction[regi,,])*gdp[regi,2035,]/setYears(gdp[regi,as.numeric(RefYear[regi,,]),],NULL)*setYears(ghg[regi,as.numeric(RefYear[regi,,]),],NULL)/setYears(ghg[regi,2005,],NULL)
      }
      
      
      #type 2: GHG-Absolute: calculate with BAU/Reference level
      for(regi in r2030_2){
        if(RefYear[regi,,]=="BAU"){
          #if relative to BAU: BAU emission - absolute reduction target is target year emission
          factor[regi,2030,]  <- (BauRefEmi[regi,,]+reduction[regi,,])/setYears(ghg[regi,2005,],NULL)
        } else {
          if(is.na(BauRefEmi[regi,,])){
            #if relative to reference year, but reference emission level is not specified, use historic data
            factor[regi,2030,]  <- (setYears(ghg[regi,as.numeric(RefYear[regi,,]),],NULL)+reduction[regi,,])/setYears(ghg[regi,2005,],NULL)  
          } else {
            #if relative to reference year with specified reference year emissions, use this data
            factor[regi,2030,]  <- (setYears(ghg[regi,as.numeric(RefYear[regi,,]),],NULL)+reduction[regi,,])/setYears(ghg[regi,2005,],NULL)  
          }
          
        }
      }
      for(regi in r2025_2){
        if(RefYear[regi,,]=="BAU"){
          #if relative to BAU: BAU emission - absolute reduction target is target year emission
          factor[regi,2025,]  <- (BauRefEmi[regi,,]+reduction[regi,,])/setYears(ghg[regi,2005,],NULL)
        } else {
          if(is.na(BauRefEmi[regi,,])){
            #if relative to reference year, but reference emission level is not specified, use historic data
            factor[regi,2025,]  <- (setYears(ghg[regi,as.numeric(RefYear[regi,,]),],NULL)+reduction[regi,,])/setYears(ghg[regi,2005,],NULL)  
          } else {
            #if relative to reference year with specified reference year emissions, use this data
            factor[regi,2025,]  <- (setYears(ghg[regi,as.numeric(RefYear[regi,,]),],NULL)+reduction[regi,,])/setYears(ghg[regi,2005,],NULL)  
          }
          
        }
      }
      for(regi in r2035_2){
        if(RefYear[regi,,]=="BAU"){
          #if relative to BAU: BAU emission - absolute reduction target is target year emission
          factor[regi,2035,]  <- (BauRefEmi[regi,,]+reduction[regi,,])/setYears(ghg[regi,2005,],NULL)
        } else {
          if(is.na(BauRefEmi[regi,,])){
            #if relative to reference year, but reference emission level is not specified, use historic data
            factor[regi,2035,]  <- (setYears(ghg[regi,as.numeric(RefYear[regi,,]),],NULL)+reduction[regi,,])/setYears(ghg[regi,2005,],NULL)  
          } else {
            #if relative to reference year with specified reference year emissions, use this data
            factor[regi,2035,]  <- (setYears(ghg[regi,as.numeric(RefYear[regi,,]),],NULL)+reduction[regi,,])/setYears(ghg[regi,2005,],NULL)  
          }
          
        }
      }
       
      #type 3: GHG-relative: only if BAU/Reference level given, otherwise not use relative targets vs. BAU
      for(regi in r2030_3){
        if(RefYear[regi,,]=="BAU"){
          #if relative to BAU: BAU emission - absolute reduction target is target year emission
          factor[regi,2030,]  <- (1+reduction[regi,,])*BauRefEmi[regi,,]/setYears(ghg[regi,2005,],NULL)
        } else {
          if(is.na(BauRefEmi[regi,,])){
            #if relative to reference year, but reference emission level is not specified, use historic data
            factor[regi,2030,]  <- (1+reduction[regi,,])*setYears(ghg[regi,as.numeric(RefYear[regi,,]),],NULL)/setYears(ghg[regi,2005,],NULL)  
          } else {
            #if relative to reference year with specified reference year emissions, use this data
            factor[regi,2030,]  <- (1+reduction[regi,,])*setYears(ghg[regi,as.numeric(RefYear[regi,,]),],NULL)/setYears(ghg[regi,2005,],NULL)  
          }
          
        }
      }
      for(regi in r2025_3){
        if(RefYear[regi,,]=="BAU"){
          #if relative to BAU: BAU emission - absolute reduction target is target year emission
          factor[regi,2025,]  <- (1+reduction[regi,,])*BauRefEmi[regi,,]/setYears(ghg[regi,2005,],NULL)
        } else {
          if(is.na(BauRefEmi[regi,,])){
            #if relative to reference year, but reference emission level is not specified, use historic data
            factor[regi,2025,]  <- (1+reduction[regi,,])*setYears(ghg[regi,as.numeric(RefYear[regi,,]),],NULL)/setYears(ghg[regi,2005,],NULL)  
          } else {
            #if relative to reference year with specified reference year emissions, use this data
            factor[regi,2025,]  <- (1+reduction[regi,,])*setYears(ghg[regi,as.numeric(RefYear[regi,,]),],NULL)/setYears(ghg[regi,2005,],NULL)  
          }
          
        }
      }
      for(regi in r2035_3){
        if(RefYear[regi,,]=="BAU"){
          #if relative to BAU: BAU emission - absolute reduction target is target year emission
          factor[regi,2035,]  <- (1+reduction[regi,,])*BauRefEmi[regi,,]/setYears(ghg[regi,2005,],NULL)
        } else {
          if(is.na(BauRefEmi[regi,,])){
            #if relative to reference year, but reference emission level is not specified, use historic data
            factor[regi,2035,]  <- (1+reduction[regi,,])*setYears(ghg[regi,as.numeric(RefYear[regi,,]),],NULL)/setYears(ghg[regi,2005,],NULL)  
          } else {
            #if relative to reference year with specified reference year emissions, use this data
            factor[regi,2035,]  <- (1+reduction[regi,,])*setYears(ghg[regi,as.numeric(RefYear[regi,,]),],NULL)/setYears(ghg[regi,2005,],NULL)  
          }
        }
      }
      
      #type 4: GHG/CAP skip for the moment, only applies to Simbabwe, less than 0.1% of total GHG
      # factor  <-
    
      # special case EU countries: until official 2030 burden sharing is decided, apply EU target proportionally across countries (problematic for REMIND-EU runs...)
      factor[eu28_countries,2030,] <- (1-0.4)*setYears(ghg[eu28_countries,1990,],NULL)/setYears(ghg[eu28_countries,2005,],NULL)
    

    #default setting: exclude countries with factor higher than the highest region average BAU growth rate (excp. China and India)
    targetCountries <- NULL
    for (r in getRegions(factor)) {
      if((max(factor[r,,])<2.5)){
        targetCountries = c(targetCountries,r)
      }
    }
    setdiff(getRegions(factor),targetCountries)
    if(!"IND" %in% targetCountries){
      targetCountries <- c(targetCountries,"IND")
    }
    if(!"CHN" %in% targetCountries){
      targetCountries <- c(targetCountries,"CHN")
    }
    #only consider rest of countries for quantitative target calculation
    factor <- factor[targetCountries,,]
      
      
    #create 1/0 dummy for calculation of regional share covered by quantitative target, per TarYear
    dummy <- factor[,,2]
    for(y in getYears(dummy)){
      for(r in getRegions(dummy)){
        if(dummy[r,y,] != 0){
          dummy[r,y,] <- 1
        }
      }
    }
    
    #fill up with zeros for other countries
    dummy <- toolCountryFill(dummy,fill=0)
    factor <- toolCountryFill(factor,fill=0)
    ghg <- toolCountryFill(ghg,fill=0)
    
    #in order to calculate the share of regional emissions coming from countries with quantitative target
    ghg_target <- setYears(ghg[,2005,],NULL) * setNames(dummy,NULL)
    #check share of total emissions by countries with quantitative target
    dimSums(ghg_target,dim=c(1,2))/glob_ghg[,2005,]
    
    #make ghg_target only represent countries with 2030 data
    dummy2 <- 0*ghg
    for (y in getYears(dummy2)){
      dummy2[,y,] <- setYears(dummy[,"y2030",],y)
    }
    getNames(dummy2)<- NULL
    
    #------------------ weight alternative 1 -----------------------------------------
    # calulate weight for GHG emission share - assuming constant relative emission intensities accros countries of one region
    weight_share1 <- new.magpie(getRegions(dummy),getYears(dummy),getNames(factor))
    for(t in getYears(dummy)) {
      # calculate growth
      weight_share1[,t,] <-  setYears(ghg[,2005,] / gdp[,2005,],NULL) * gdp[,t,]
    }
    #---------------------------------------------------------------------------------
    
    #------------------ weight alternative 2 -----------------------------------------
    ## calulate alternative weight for GHG emission share - assuming converging emission intensities accros countries of one region
    #map <- read.csv(toolMappingFile("regional",getConfig("regionmapping")),sep=";")  # get current mapping
    #weight_share2 <- new.magpie(getRegions(dummy),getYears(dummy),getNames(factor))
    #for(t in getYears(dummy)) {
    #  for(r in getRegions(dummy)){
    #     # get list of countries that belong to the same region as r
    #     regi   <- map$RegionCode[map$CountryCode==r]
    #     c_regi <- map$CountryCode[map$RegionCode==regi] 
    #     # calculate growth
    #     weight_share2[r,t,] <- ( setYears(  ghg[r,2005,]/gdp[r,2005,]  
    #                                       + dimSums(ghg[c_regi,2005,],dim=1)/dimSums(gdp[c_regi,2005,],dim=1) ,NULL) 
    #                             ) / 2 * gdp[r,t,]
    #  }
    #}
    #---------------------------------------------------------------------------------
    
    # define dummy for all SSPs
    dummy3 <- new.magpie(getRegions(dummy),getYears(dummy),getNames(factor))
    for (s in getNames(factor)) {
       dummy3[,,s] <- dummy
    }
    
    if(length(grep("share",subtype))==0 && length(grep("ghg",subtype))==0){
      description <- "Multiplier for target year emissions vs 2005 emissions, as weighted average for all countries with quantifyable emissions under NDC in particular region, using compilation from SI of Rogelj et al 2017 Nat Comm paper, per target year"
      return(list(x=factor, weight=ghg_target[,,], unit="1",description = description))  #p45_factor_targetyear
      } else if(length(grep("ghg",subtype))==0){
      description <- "2005 GHG emission share of countries with quantifyable emissions under NDC in particular region, using compilation from SI of Rogelj et al 2017 Nat Comm paper, per target year"
      return(list(x=dummy3, weight=weight_share1, unit="1",description = description))  # p45_2005share_target - share of 2005 emissions in countries within region covered by quantitative target
      } else {
      description <- "GHG emissions share of countries with quantifyable 2030 target in particular region"
      return(list(x=dummy2,weight=ghg, unit="1",description = description))
    }
}


