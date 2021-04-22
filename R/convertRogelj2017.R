#' Policy targets for NDCs from Rogelj2017
#' @description Converts conditional and unconditional capacity targets into total capacity (GW) in target year
#' the Generation targets are similar to the capacity targets but include the capacity factors,
#' the Emissions targets are the total (except land CO2) emissions in the target year 
#' @param x MAgPIE object to be converted
#' @param subtype UNCapacity, COCapacity
#' @return Magpie object with Total Installed Capacity (GW) targets. The target years differ depending upon the database.
#' @author Aman Malik and Christoph Bertram
#' @importFrom R.utils isZero

convertRogelj2017 <- function(x,subtype){
  if(subtype == "UNCapacity"){  # UNconditional policies
    # x <- readSource("Rogelj2017","UNCapacity",convert=F)
    x <- x[,,"conditional",invert=TRUE] # keep only unconditional policies
    x[is.na(x)] <- 0 # Converting all NAs to zero

    # reading historical data
    hist_cap <- readSource(type="IRENA",subtype="Capacity")/1000 # converting from MW to GW
    hist_gen <- readSource("IRENA", subtype = "Generation")# Units are GWh
    
    # Real world capacity factor for hydro = Generation in last year/Capacity in last year
    cf_realworld <- hist_gen[,2015,"Hydropower"]/(8760*hist_cap[,2015,"Hydropower"]) 
    cf_realworld[is.na(cf_realworld)] <- 0
    getNames(cf_realworld) <- "Hydro"
    
    
    tech <- c("Wind","Solar","Hydro","Nuclear","Biomass")
    target_years <- c(2020,2025,2030,2035)
    
    x_tmp <- x 
    # 1. Cases when targets are given in non-model year. e.g., 2022, 2032
    input_yr <- getYears(x,as.integer = TRUE)
    for ( i in input_yr){
      if (i>2020 & i<2025)
        x_tmp[,2025,] <-  as.vector(x[,i,])*(1+(2025-i)*0.05) 
      if (i>2025 & i<2030)
        x_tmp[,2030,] <-  as.vector(x[,i,])*(1+(2030-i)*0.05) 
      if (i>2030 & i<2035)
        x_tmp[,2035,] <-  as.vector(x[,i,])*(1+(2035-i)*0.05)
    }
    
    # Creating magpie object which at the end will only contain capacity targets
    x_new <- new.magpie(getRegions(x_tmp),target_years,tech)
    x_new[is.na(x_new)] <- 0 
    
    
    # Capacity factors in REMIND. From : calcOutput("Capacityfactor"...)
    cf_biomass <- 0.75 
    cf_nuclear <- 0.85
    cf_hydro <- cf_realworld
   
    
    # Initialising all capacities for all model years to current capacities and converting generation to capacity
    x_new[,,c("Wind","Solar")]  <- setYears(hist_cap[getRegions(x_tmp),2015,c("Wind","Solar")]) 
    x_new[,,"Biomass"] <- setYears(hist_cap[getRegions(x_tmp),2015,"Bioenergy"])
    
    # special case for hydro. 
    x_new[,,"Hydro"] <- setYears(hist_gen[getRegions(x_new),2015,"Hydropower"])
    # Special case for nuclear
    hist_gen_nuclear <- readSource(type = "BP",subtype = "Generation")*1000 # TWh to GWh
    for (i in target_years){
      for (j in getNames(x_tmp[,,"Nuclear"])){
        for (k in getRegions(x_tmp)){
          if(x_tmp[k,i,j]!=0)
            x_new[k,,"Nuclear"] <- setYears(hist_gen_nuclear[k,2015,"Nuclear"])/(8760*cf_nuclear)
        }
      }
    }
    x_tmp <- x_tmp[,target_years,]
    x_current <- x_new # x_current contains current capacities except for hydro where current generation values are taken
    x_new_abs <- x_new
    x_new_abs[] <- 0
    x_new_prod_nb <- x_new
    x_new_prod_nb[] <- 0
    x_new_tic <- x_new
    x_new_tic[] <- 0
    x_new_prod_swh <- x_new
    x_new_prod_swh[] <- 0
    
    # Converting additional capacity targets to absolute capacity targets 
    x_new_abs[,,c("Nuclear","Biomass","Wind","Solar")] <- x_current[,,c("Nuclear","Biomass","Wind","Solar")] +
      x_tmp[,,c("AC-Absolute.Nuclear","AC-Absolute.Biomass","AC-Absolute.Wind","AC-Absolute.Solar"),drop=TRUE]
    x_new_abs[,,"Hydro"] <- x_current[,,"Hydro"] + x_tmp[,,"AC-Absolute.Hydro",drop=TRUE]*setYears(cf_hydro[getRegions(x_tmp),,]*8760)

    # Converting Production targets (GWh) to Capacity targets (TIC-Absolute) (GW) for nuclear and biomass
    # pmax used to always take the higher value from existing capacity and new capacity (from production)
    x_new_prod_nb[,,"Nuclear"] <- pmax( x_current[,,"Nuclear"], x_tmp[,,c("Production-Absolute.Nuclear")]/(8760*cf_nuclear))
    x_new_prod_nb[,,"Biomass"] <- pmax(x_current[,,"Biomass"],x_tmp[,,c("Production-Absolute.Biomass")]/(8760*cf_biomass))
    
    
    # Total installed capacity Targets
    # target in target year should be the maximum from the target year and the current capacity
    x_new_tic[,,c("Nuclear","Biomass","Wind","Solar")] <- pmax(x_current[,,c("Nuclear","Biomass","Wind","Solar")],x_tmp[,,c("Nuclear","Biomass","Wind","Solar")][,,"TIC-Absolute",drop = TRUE])
    x_new_tic[,,"Hydro"] <- pmax(x_current[,,"Hydro"],x_tmp[,,"TIC-Absolute.Hydro",drop = TRUE]*setYears(cf_hydro[getRegions(x_tmp),,]))
    
    # Converting Production targets to capacity targets for solar (pv and csp), hydro, and wind
    # Obtaining the capacity factors (nur) values and associated maxproduction (maxprod) for Hydro, Wind, and Solar
    data_wind <- calcOutput("PotentialWind", aggregate = FALSE)
    # Reordering dim=3 for data_wind so that 1st position corresponds to maxprod.nur.1 and not maxprod.nur.9
    data_wind_sorted <- mbind(data_wind[,,"1"],data_wind[,,"2"],data_wind[,,"3"],data_wind[,,"4"],
                              data_wind[,,"5"],data_wind[,,"6"],data_wind[,,"7"],data_wind[,,"8"],data_wind[,,"9"])
    data_hydro <- calcOutput("PotentialHydro", aggregate = FALSE)
    ## save the regional mapping for this mrremind iteration
    infoconfig <- getConfig()
    regionmapping2use <- infoconfig$regionmapping
    setConfig(regionmapping = "regionmappingTCD.csv")
    data_solar <- calcOutput("Solar")
    ## re-include the official regional mapping for this iteration
    setConfig(regionmapping = regionmapping2use)
    names_solar <- paste0("Solar.",getNames(collapseNames((mselect(data_solar,type=c("nur","maxprod"),technology="spv")),collapsedim = 2)))
    names_hydro <- paste0("Hydro.",getNames(data_hydro))
    names_wind <- paste0("Wind.",getNames(data_wind_sorted))
    data_combined <- new.magpie(getRegions(data_hydro), NULL, c(names_solar,names_hydro,names_wind))
    data_combined[,,"Hydro"] <- data_hydro
    data_combined[,,"Wind"] <- data_wind_sorted
    data_combined[c("TCD","JPN"),,"Solar"][,,"maxprod"]  <- as.vector(data_solar[c("TCD","JPN"),,"maxprod"][,,"spv"])
    data_combined[c("TCD","JPN"),,"Solar"][,,"nur"]  <- as.vector(data_solar[c("TCD","JPN"),,"nur"][,,"spv"])
    data_combined <- data_combined[getRegions(x_tmp),,]
    for (n in getNames(data_combined,dim=1)){
      name=paste0(n,".maxprod")
      # Conversion from EJ/a to GWh
      data_combined[,,name,pmatch=TRUE] <- data_combined[,,name,pmatch=TRUE]*277777.778
    }
    
    data_combined[is.na(data_combined)] <- 0
    # Production/Generation targets are converted into capacity targets by alloting production to certain capacity factors based on maxprod.
    final <- numeric(length(getRegions(x_tmp)))
    names(final) <- getRegions(x_tmp)
    tmp_target <- numeric(10)
 
    #x_tmp[,,"Production-Absolute.Hydro"] <- pmax(x_tmp[,,"Production-Absolute.Hydro"],x_new[,,"Hydro"])
    x_tmp[,,"Production-Absolute.Hydro"] <- pmax(x_tmp[,,"Production-Absolute.Hydro"],x_new_tic[,,"Hydro"],x_new_abs[,,"Hydro"])
    x_tmp[is.na(x_tmp)] <- 0
    # For all countries which have non-zero generation values but zero or negative maxprod(),
    #  replace x_tmp[,,"Production-Absolute.Hydro]==0
    #  Even if there is one +ve production absolute value for Hydro but all maxprod are zero
    for (r in names(final)){
      if(any(x_tmp[r,,"Production-Absolute.Hydro"]!=0) & 
         all(data_combined[r,,"Hydro.maxprod"]==0)|any(data_combined[r,,"Hydro.maxprod"]<0) )
        x_tmp[r,,"Production-Absolute.Hydro"] <- 0
    }
    
    
    
    for (t in c("Solar","Wind","Hydro")){
      data_sel <- data_combined[,,t]
      data_in_use <-  data_sel[,,"maxprod"]/data_sel[,,"nur"]
      for (y in target_years){
        final[] <-0
        for (r in names(final)){
          tmp_target <- numeric(10)
          name <- paste0(t,".maxprod")
          name2 <- paste0("Production-Absolute.",t)
          if (!isZero(x_tmp[,,"Production-Absolute"][,,t])[r,y,] & 
              dimSums(data_combined[r,,name],na.rm=T) > max(x_tmp[r,,name2])){
            # extracting the first non-zero location of maxprod
            name <- paste0(t,".maxprod")
            loc <- min(which(!R.utils::isZero(data_combined[r,,name,pmatch=TRUE]))) 
            tmp_target[1] <- x_tmp[r,y,"Production-Absolute"][,,t]
            if (data_sel[r,,"maxprod"][,,loc] > tmp_target[1]){
              final[r] <- tmp_target[1]/(8760*data_sel[r,,"nur"][,,loc])
            } else {tmp_target[2] <- tmp_target[1] - data_sel[r,,"maxprod"][,,loc]
            if(data_sel[r,,"maxprod"][,,loc+1] > tmp_target[2]){
              final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + tmp_target[1]/data_sel[r,,"nur"][,,loc+1])
            } else {tmp_target[3] <- tmp_target[2] - data_sel[r,,"maxprod"][,,loc+1]
            if(data_sel[r,,"maxprod"][,,loc+2] > tmp_target[3]){
              final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][,,loc+1]
                                    + tmp_target[2]/data_sel[r,,"nur"][,,loc+2])
            } else {tmp_target[4] <- tmp_target[3] - data_sel[r,,"maxprod"][,,loc+2]
            if(data_sel[r,,"maxprod"][,,loc+3] > tmp_target[4]){
              final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][,,loc+1] + data_in_use[r,,][,,loc+2] +
                                      tmp_target[3]/data_sel[r,,"nur"][,,loc+3])
              final[r] <- tmp_target[1]
            } else {tmp_target[5] <- tmp_target[4] - data_sel[r,,"maxprod"][,,loc+3]
            if(data_sel[r,,"maxprod"][loc+4] > tmp_target[5]){
              final[r] <-(1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][,,loc+1] + data_in_use[r,,][,,loc+2] +
                                     data_in_use[r,,][,,loc+3] + tmp_target[4]/data_sel[r,,"nur"][,,loc+4])
            } else {tmp_target[6] <- tmp_target[5] - data_sel[r,,"maxprod"][,,loc+4]
            if(data_sel[r,,"maxprod"][loc+5] > tmp_target[6]){
              final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][,,loc+1] + data_in_use[r,,][,,loc+2] +
                                      data_in_use[r,,][,,loc+3] + data_in_use[r,,][,,loc+4] + 
                                      tmp_target[5]/data_sel[r,,"nur"][,,loc+5])
            }
            }
            }
            }
            }
            }
          }
        }
        x_new_prod_swh[,y,t] <-final 
      } 
    }
    
    x_new_gen <- mbind(x_new_prod_swh[,,c("Solar","Wind","Hydro")],x_new_prod_nb[,,c("Biomass","Nuclear")])
    x_new[,,c("Solar","Wind","Hydro","Biomass","Nuclear")] <- pmax(x_new_abs[,,c("Solar","Wind","Hydro","Biomass","Nuclear")],
                                                                   x_new_gen[,,c("Solar","Wind","Hydro","Biomass","Nuclear")],
                                                                   x_new_tic[,,c("Solar","Wind","Hydro","Biomass","Nuclear")])
    # x_new[,,c("Solar","Wind")] <- pmax(x_new_copy[,,c("Solar","Wind")],x_new[,,c("Solar","Wind")])
    x_new[,,"Hydro"] <- x_new_gen[,,"Hydro"]
    
    #  # Making sure that targets in subsequent years are always same or greater than the proceeding year
    for (r in getRegions(x_tmp)){
      for (t in tech){
        for(i in c(2020,2025,2030)){
          if(x_new[r,i+5,t] < setYears(x_new[r,i,t])){
            x_new[r,i+5,t] <- setYears(x_new[r,i,t])
          }else{
            x_new[r,i,t] <- x_new[r,i,t]
          }
        }
      }
    }
    # countries not in the database
    rest_regions <- getRegions(hist_cap)[!(getRegions(hist_cap) %in% getRegions(x_new))]
    x_other <- new.magpie(rest_regions,target_years,tech)
    x_other[,,c("Wind","Solar")]  <- setYears(hist_cap[rest_regions,2015,c("Solar","Wind")])
    x_other[,,"Nuclear"] <- 0
    x_other[,,"Biomass"] <- setYears(hist_cap[rest_regions,2015,"Bioenergy"])
    x_other[,,"Hydro"] <- setYears(hist_cap[rest_regions,2015,"Hydropower"])*setYears(cf_hydro[rest_regions,,])
    
    x_final <- magpiesort(mbind(x_new,x_other))
    x_final[is.na(x_final)] <- 0
    x <- x_final
    getNames(x) <- c("wind","spv","hydro","tnrs","bioigcc")  
    
  }
  if(subtype == "COCapacity"){
    # x <- readSource("Rogelj2017",subtype = "COCapacity",convert = F)
    # loop to make conditional targets at least as good as unconditional targets
     for (r in getRegions(x)){
      for (t in getYears(x)){
        for (tech in getNames(x[,,"conditional",drop=TRUE]) )
        if(is.na(x[r,t,paste0("conditional",".",tech)]))
          x[r,t,paste0("conditional",".",tech)] <- x[r,t,paste0("unconditional",".",tech)]
      }
    }
    
    x <- x[,,"unconditional",invert=TRUE]# drop unconditional targets
    x[is.na(x)] <- 0 # Converting all NAs to zero
    # reading historical data
    hist_cap <- readSource(type="IRENA",subtype="Capacity")/1000 # converting from MW to GW
    hist_gen <- readSource("IRENA", subtype = "Generation")# Units are GWh
    
    # Real world capacity factor for hydro = Generation in last year/Capacity in last year
    cf_realworld <- hist_gen[,2015,"Hydropower"]/(8760*hist_cap[,2015,"Hydropower"]) 
    cf_realworld <- cf_realworld<1
    getNames(cf_realworld) <- "Hydro"
    
    tech <- c("Wind","Solar","Hydro","Nuclear","Biomass")
    target_years <- c(2020,2025,2030,2035)
    
    x_tmp <- x 
    # 1. Cases when targets are given in non-model year. e.g., 2022, 2032
    input_yr <- getYears(x,as.integer = TRUE)
    for ( i in input_yr){
      if (i>2020 & i<2025)
        x_tmp[,2025,] <-  as.vector(x[,i,])*(1+(2025-i)*0.05) 
      if (i>2025 & i<2030)
        x_tmp[,2030,] <-  as.vector(x[,i,])*(1+(2030-i)*0.05) 
      if (i>2030 & i<2035)
        x_tmp[,2035,] <-  as.vector(x[,i,])*(1+(2035-i)*0.05)
    }
    
    # Creating magpie object which at the end will only contain capacity targets
    x_new <- new.magpie(getRegions(x_tmp),target_years,tech)
    x_new[is.na(x_new)] <- 0 
    
    # Capacity factors in REMIND. From : calcOutput("Capacityfactor"...)
    cf_biomass <- 0.75 
    cf_nuclear <- 0.85
    cf_hydro <- cf_realworld
  
    
    # Initialising all capacities for all model years to current capacities and converting generation to capacity
    x_new[,,c("Wind","Solar")]  <- setYears(hist_cap[getRegions(x_tmp),2015,c("Wind","Solar")]) 
    x_new[,,"Biomass"] <- setYears(hist_cap[getRegions(x_tmp),2015,"Bioenergy"])
    
    # special case for hydro. 
    x_new[,,"Hydro"] <- setYears(hist_gen[getRegions(x_new),2015,"Hydropower"])
    # Special case for nuclear 
    hist_gen_nuclear <- readSource(type = "BP",subtype = "Generation")*1000 # TWh to GWh
    for (i in target_years){
      for (j in getNames(x_tmp[,,"Nuclear"])){
        for (k in getRegions(x_tmp)){
          if(x_tmp[k,i,j]!=0)
            x_new[k,,"Nuclear"] <- setYears(hist_gen_nuclear[k,2015,"Nuclear"])/(8760*cf_nuclear)
        }
      }
    }
    
    x_current <- x_new # x_current contains current capacity values. x_new contains capacity targets
    x_tmp <- x_tmp[,target_years,]
    x_current <- x_new # x_current contains current capacities except for hydro where current generation values are taken
    x_new_abs <- x_new
    x_new_abs[] <- 0
    x_new_prod_nb <- x_new
    x_new_prod_nb[] <- 0
    x_new_tic <- x_new
    x_new_tic[] <- 0
    x_new_prod_swh <- x_new
    x_new_prod_swh[] <- 0
    
    
    # Converting additional capacity targets to absolute capacity targets 
    x_new_abs[,,c("Nuclear","Biomass","Wind","Solar")] <- x_current[,,c("Nuclear","Biomass","Wind","Solar")] +
      x_tmp[,,c("AC-Absolute.Nuclear","AC-Absolute.Biomass","AC-Absolute.Wind","AC-Absolute.Solar"),drop=TRUE]
    x_new_abs[,,"Hydro"] <- x_current[,,"Hydro"] + x_tmp[,,"AC-Absolute.Hydro",drop=TRUE]*setYears(cf_hydro[getRegions(x_tmp),,]*8760)
    
    # Converting Production targets (GWh) to Capacity targets (TIC-Absolute) (GW) for nuclear and biomass
    # pmax used to always take the higher value from existing capacity and new capacity (from production)
    x_new_prod_nb[,,"Nuclear"] <- pmax( x_current[,,"Nuclear"], x_tmp[,,c("Production-Absolute.Nuclear")]/(8760*cf_nuclear))
    x_new_prod_nb[,,"Biomass"] <- pmax(x_current[,,"Biomass"],x_tmp[,,c("Production-Absolute.Biomass")]/(8760*cf_biomass))
    
    
    # Total installed capacity Targets
    # target in target year should be the maximum from the target year and the current capacity
    x_new_tic[,,c("Nuclear","Biomass","Wind","Solar")] <- pmax(x_current[,,c("Nuclear","Biomass","Wind","Solar")],x_tmp[,,c("Nuclear","Biomass","Wind","Solar")][,,"TIC-Absolute",drop = TRUE])
    x_new_tic[,,"Hydro"] <- pmax(x_current[,,"Hydro"],x_tmp[,,"TIC-Absolute.Hydro",drop = TRUE]*setYears(cf_hydro[getRegions(x_tmp),,]))
    
    # Converting Production targets to capacity targets for solar (pv and csp), hydro, and wind
    # Obtaining the capacity factors (nur) values and associated maxproduction (maxprod) for Hydro, Wind, and Solar
    data_wind <- calcOutput("PotentialWind", aggregate = FALSE)
    # Reordering dim=3 for data_wind so that 1st position corresponds to maxprod.nur.1 and not maxprod.nur.9
    data_wind_sorted <- mbind(data_wind[,,"1"],data_wind[,,"2"],data_wind[,,"3"],data_wind[,,"4"],
                              data_wind[,,"5"],data_wind[,,"6"],data_wind[,,"7"],data_wind[,,"8"],data_wind[,,"9"])
    data_hydro <- calcOutput("PotentialHydro", aggregate = FALSE)
    ## change regional mapping to load this specific dataset
    infoconfig <- getConfig()
    regionmapping2use <- infoconfig$regionmapping
    setConfig(regionmapping = "regionmappingTCD.csv")
    data_solar <- calcOutput("Solar")
    names_solar <- paste0("Solar.",getNames(collapseNames((mselect(data_solar,type=c("nur","maxprod"),technology="spv")),collapsedim = 2)))
    ## go back to the oficial database
    setConfig(regionmapping = regionmapping2use)
    names_hydro <- paste0("Hydro.",getNames(data_hydro))
    names_wind <- paste0("Wind.",getNames(data_wind_sorted))
    data_combined <- new.magpie(getRegions(data_hydro), NULL, c(names_solar,names_hydro,names_wind))
    data_combined[,,"Hydro"] <- data_hydro
    data_combined[,,"Wind"] <- data_wind_sorted
    data_combined[c("TCD","JPN"),,"Solar"][,,"maxprod"]  <- as.vector(data_solar[c("TCD","JPN"),,"maxprod"][,,"spv"])
    data_combined[c("TCD","JPN"),,"Solar"][,,"nur"]  <- as.vector(data_solar[c("TCD","JPN"),,"nur"][,,"spv"])
    data_combined <- data_combined[getRegions(x_tmp),,]
    for (n in getNames(data_combined,dim=1)){
      name=paste0(n,".maxprod")
      # Conversion from EJ/a to GWh
      data_combined[,,name,pmatch=TRUE] <- data_combined[,,name,pmatch=TRUE]*277777.778
    }
    data_combined[is.na(data_combined)] <- 0
    
    # Production/Generation targets are converted into capacity targets by alloting production to certain capacity factors based on maxprod.
    final <- numeric(length(getRegions(x_tmp)))
    names(final) <- getRegions(x_tmp)
    tmp_target <- numeric(10)
    x_new_copy <- x_new
   # x_tmp[,,"Production-Absolute.Hydro"] <- pmax(x_tmp[,,"Production-Absolute.Hydro"],x_new[,,"Hydro"])
    x_tmp[,,"Production-Absolute.Hydro"] <- pmax(x_tmp[,,"Production-Absolute.Hydro"],x_new_tic[,,"Hydro"],x_new_abs[,,"Hydro"])
    x_tmp[is.na(x_tmp)] <- 0
    # For all countries which have non-zero generation values but zero or negative maxprod(),
    #  replace x_tmp[,,"Production-Absolute.Hydro]==0
    #  
    for (r in names(final)){
      if(any(x_tmp[r,,"Production-Absolute.Hydro"]!=0) & 
         all(data_combined[r,,"Hydro.maxprod"]==0)|any(data_combined[r,,"Hydro.maxprod"]<0) )
        x_tmp[r,,"Production-Absolute.Hydro"] <- 0
    }
    
    for (t in c("Solar","Wind","Hydro")){
      data_sel <- data_combined[,,t]
      data_in_use <-  data_sel[,,"maxprod"]/data_sel[,,"nur"]
      for (y in target_years){
        final[] <-0
        for (r in names(final)){
          tmp_target <- numeric(10)
          name <- paste0(t,".maxprod")
          name2 <- paste0("Production-Absolute.",t)
          if (!isZero(x_tmp[,,"Production-Absolute"][,,t])[r,y,] & 
              dimSums(data_combined[r,,name],na.rm=T) > max(x_tmp[r,,name2])){
            # extracting the first non-zero location of maxprod
            name <- paste0(t,".maxprod")
            loc <- min(which(!R.utils::isZero(data_combined[r,,name,pmatch=TRUE]))) 
            tmp_target[1] <- x_tmp[r,y,"Production-Absolute"][,,t]
            if (data_sel[r,,"maxprod"][,,loc] > tmp_target[1]){
              final[r] <- tmp_target[1]/(8760*data_sel[r,,"nur"][,,loc])
            } else {tmp_target[2] <- tmp_target[1] - data_sel[r,,"maxprod"][,,loc]
            if(data_sel[r,,"maxprod"][,,loc+1] > tmp_target[2]){
              final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + tmp_target[1]/data_sel[r,,"nur"][,,loc+1])
            } else {tmp_target[3] <- tmp_target[2] - data_sel[r,,"maxprod"][,,loc+1]
            if(data_sel[r,,"maxprod"][,,loc+2] > tmp_target[3]){
              final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][,,loc+1]
                                    + tmp_target[2]/data_sel[r,,"nur"][,,loc+2])
            } else {tmp_target[4] <- tmp_target[3] - data_sel[r,,"maxprod"][,,loc+2]
            if(data_sel[r,,"maxprod"][,,loc+3] > tmp_target[4]){
              final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][,,loc+1] + data_in_use[r,,][,,loc+2] +
                                      tmp_target[3]/data_sel[r,,"nur"][,,loc+3])
              final[r] <- tmp_target[1]
            } else {tmp_target[5] <- tmp_target[4] - data_sel[r,,"maxprod"][,,loc+3]
            if(data_sel[r,,"maxprod"][loc+4] > tmp_target[5]){
              final[r] <-(1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][,,loc+1] + data_in_use[r,,][,,loc+2] +
                                     data_in_use[r,,][,,loc+3] + tmp_target[4]/data_sel[r,,"nur"][,,loc+4])
            } else {tmp_target[6] <- tmp_target[5] - data_sel[r,,"maxprod"][,,loc+4]
            if(data_sel[r,,"maxprod"][loc+5] > tmp_target[6]){
              final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][,,loc+1] + data_in_use[r,,][,,loc+2] +
                                      data_in_use[r,,][,,loc+3] + data_in_use[r,,][,,loc+4] + 
                                      tmp_target[5]/data_sel[r,,"nur"][,,loc+5])
            }
            }
            }
            }
            }
            }
          }
        }
        x_new_prod_swh[,y,t] <-final 
      } 
    }
    x_new_gen <- mbind(x_new_prod_swh[,,c("Solar","Wind","Hydro")],x_new_prod_nb[,,c("Biomass","Nuclear")])
    x_new[,,c("Solar","Wind","Hydro","Biomass","Nuclear")] <- pmax(x_new_abs[,,c("Solar","Wind","Hydro","Biomass","Nuclear")],
                                                                   x_new_gen[,,c("Solar","Wind","Hydro","Biomass","Nuclear")],
                                                                   x_new_tic[,,c("Solar","Wind","Hydro","Biomass","Nuclear")])
    
    # x_new[,,c("Solar","Wind")] <- pmax(x_new_copy[,,c("Solar","Wind")],x_new[,,c("Solar","Wind")])
    x_new[,,"Hydro"] <- x_new_gen[,,"Hydro"]
    
    #x_new[,,c("Solar","Wind")] <- pmax(x_new_copy[,,c("Solar","Wind")],x_new[,,c("Solar","Wind")])
    #x_new[,,"Hydro"] <- x_new_copy[,,"Hydro"]
    
    #  # Making sure that targets in subsequent years are always same or greater than the proceeding year
    for (r in getRegions(x_tmp)){
      for (t in tech){
        for(i in c(2020,2025,2030)){
          if(x_new[r,i+5,t] < setYears(x_new[r,i,t])){
            x_new[r,i+5,t] <- setYears(x_new[r,i,t])
          }else{
            x_new[r,i,t] <- x_new[r,i,t]
          }
        }
      }
    }
    # countries not in the database
    rest_regions <- getRegions(hist_cap)[!(getRegions(hist_cap) %in% getRegions(x_new))]
    x_other <- new.magpie(rest_regions,target_years,tech)
    x_other[,,c("Wind","Solar")]  <- setYears(hist_cap[rest_regions,2015,c("Solar","Wind")])
    x_other[,,"Nuclear"] <- 0
    x_other[,,"Biomass"] <- setYears(hist_cap[rest_regions,2015,"Bioenergy"])
    x_other[,,"Hydro"] <- setYears(hist_cap[rest_regions,2015,"Hydropower"])*setYears(cf_hydro[rest_regions,,])
    
    x_final <- magpiesort(mbind(x_new,x_other))
    x_final[is.na(x_final)] <- 0
    x <- x_final
    getNames(x) <- c("wind","spv","hydro","tnrs","bioigcc")
   }
  
  x <- toolCountryFill(x,fill = 0)
  return(x)
}
