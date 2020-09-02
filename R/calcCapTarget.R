#' Capacity targets from two sources
#' @description The capacity targets (GW)  at regional level are produced from two different databases-
#' Rogelj 2017 paper (see readme in inputdata), and REN21 Global Renewables report
#' The Rogelj2017 capacity targets are further broken down to conditional and unconditional targets.
#' @author Aman Malik
#' @param sources Database source
#' @param condition if applicable, conditional or unconditional
#' @importFrom dplyr %>% filter


calcCapTarget <- function(sources,condition){ 
  
  if(sources == "Rogelj2017"){
    if(condition=="Conditional"){
      x <- readSource("Rogelj2017","COCapacity")
      description <- "Conditional Capacity targets from Nationally Determined Contributions (NDC)"  
    }
    else if(condition=="Unconditional"){
      x <- readSource("Rogelj2017","UNCapacity")
      description <- "Unconditional Capacity targets from Nationally Determined Contributions (NDC)"  
    }
      
  }
  
  if(sources == "REN21"){
    x <- readSource("REN21","Capacity")
    description <- "Capacity targets from REN 21(2017) database"
  }
  
  if (sources =="NDC+REN21+CHN_NUC"){
    # Additional CHn nuclear target and EV target
    
    NDC <- readSource("Rogelj2017","COCapacity")
    REN21 <- readSource("REN21",subtype = "Capacity")
    
    t_names <- unique(c(getNames(NDC),getNames(REN21))) # names of all technologies in REN21 and NDC database
    t_names <- c(t_names,"apCarElT")
    x <- new.magpie(getRegions(REN21),getYears(REN21),t_names)
    # China's nuclear target
    common_tech <- intersect(getNames(REN21),getNames(NDC))
   # for common technologies, take bigger value
    x[,getYears(NDC),common_tech] <- pmax(REN21[,getYears(NDC),common_tech],NDC[,,common_tech])
    # for tech. in REN21 but not in NDC, take REN21 values
    x[,,setdiff(getNames(REN21),common_tech)] <- REN21[,,setdiff(getNames(REN21),common_tech)]
    # for tech. in NDC but not in REN21, take NDC values
    x[,getYears(NDC),setdiff(getNames(NDC),common_tech)] <- NDC[,,setdiff(getNames(NDC),common_tech)]
    # additional nuclear policy for CHN. The target is actually 2020 in 58 GW in 2020, but setting this leads to an
    # unfeasible solution in REMIND, therefore setting from 2025 onwards
    x["CHN",seq(2025,2040,5),"tnrs"] <- 58 # in GW 
    p40_conv_cap_2_MioLDV <-  650 # Conversion factor from capacity of ApCarxxx to Mio Light duty vehicles
    x["CHN",2020,"apCarElT"] <- 5/p40_conv_cap_2_MioLDV # China's EV target of 5 Million EVs by 2020. 
    x["CHN",seq(2025,2040,5),"apCarElT"] <- 15/p40_conv_cap_2_MioLDV
    
    # making 2040 targets as good as 2035 targets.
    for (t in 1:length(as.vector(x[,2040,])))  {
      if(is.na(as.vector(x[,2040,])[t])){
        x[,2040,][t] <- as.vector(x[,2035,])[t]
      }
    }
    
    
    ### Hydrogen Target Start
    # hydrogen capacity targets from national/EU hydrogen strategies
    
    # Region targets
    reg.map <- toolGetMapping("regionmappingH12.csv", type = "regional") # get H12 regionmapping
    H2Target.reg <- new.magpie(unique(reg.map$RegionCode), 
                           getYears(x), 
                           "elh2", 
                           fill = 0)
    # Electrolyzer capacity target from the EU Hydrogen Strategy
    # https://ec.europa.eu/energy/sites/ener/files/hydrogen_strategy.pdf
    H2Target.reg["EUR", "y2025",] <- 6
    H2Target.reg["EUR", "y2030",] <- 40
    
    
    
    # Country Targets 
    H2Target.country <- new.magpie(getRegions(x), 
                                   getYears(x), 
                                   "elh2", fill = 0)
    # iso countries with a country target that belong to the EU
    country.target.regs <- c("DEU")
    # Germany Target: https://www.bmbf.de/files/die-nationale-wasserstoffstrategie.pdf
    H2Target.country["DEU","y2030","elh2"] <- 5
    

    
    # aggregate Country Targets to EU
    H2Target.CountryAgg <- toolAggregate(H2Target.country, reg.map, dim=1)
    # reduce EU target by aggregated country targets
    H2Target <- new.magpie(unique(reg.map$RegionCode), 
                               getYears(x), 
                               "elh2", 
                               fill = 0)
    H2Target["EUR",,"elh2"] <- H2Target.reg["EUR",,"elh2"] - H2Target.CountryAgg["EUR",,"elh2"]
    
    # # SE VRE Production in 2015 to be used as weight for disaggregation EU target to iso countries
    # SEHistVRE <- dimSums(calcOutput("FE", aggregate = F)[,"y2015",c("SE|Electricity|Solar (EJ/yr)",
    #                                                                 "SE|Electricity|Wind (EJ/yr)")],
    #                      dim = 3)
    
    # GDP 2015 to be used as weight for disaggregation of EU target to iso coutries
    GDP2015 <- calcOutput("GDPpppPast", aggregate = F)[,"y2015",]
    
    

    # regionmapping without countries that already have a country target
    CountryCode <- NULL
    reg.map.reduced <- reg.map %>% 
                        filter(! CountryCode %in% country.target.regs)
    # disaggregate EU target to iso-countries
    H2Target.disagg <- toolAggregate(H2Target, reg.map.reduced, 
                                  from = "RegionCode", to = "CountryCode" , dim = 1,
                                  weight = GDP2015[country.target.regs,,,invert=TRUE])
    # bind country target together with disaggregation of EU targets to other countries
    H2Target.out <- magpiesort(mbind(H2Target.country[country.target.regs,,],H2Target.disagg))
    x <- mbind(x,H2Target.out)
    ### Hydrogen Target End
    
    x[is.na(x)] <- 0
    description <- "Capacity targets combined from REN 21(2017), NDC database, special case for China nuclear target, and EV target"
  }
  
  return(list(x=x, weight=NULL, unit="GW",description = description))
  
}
  
 