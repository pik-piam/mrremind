#' Capacity targets from two sources
#' @description The capacity targets (GW)  at regional level are produced from two different databases.
#' The Rogelj2017 capacity targets are further broken down to conditional and unconditional targets.
#' @author Aman Malik
#' @param sources Database source
#' @param condition if applicable, conditional or unconditional


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
    x[is.na(x)] <- 0
    description <- "Capacity targets combined from REN 21(2017), NDC database, special case for China nuclear target, and EV target"
  }
  return(list(x=x, weight=NULL, unit="GW",description = description))
  
}
  
 