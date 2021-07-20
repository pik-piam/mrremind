#' Policy targets for REN21
#' @description This code aggregates and homogenises different types of renewable energy targets into total installed capacity targets (in GW).
#' @details Policy database accessible in "inputdata/sources/REN21/README"
#' @param x MAgPIE object to be converted
#' @param subtype Only "Capacity" asof now
#' @return Magpie object with Total Installed Capacity targets. The target years differ depending upon the database.
#' @importFrom R.utils isZero
#' @importFrom magclass magpiesort
#' @author Aman Malik


convertREN21 <- function(x,subtype){
  if(subtype == "Capacity"){
    x <- readSource("REN21",subtype="Capacity",convert = F)
    x <- magpiesort(x) # sorting years chronologically and region names alphabetically
    x[is.na(x)] <- 0 # Converting all NAs to zero
    getRegions(x) <- toolCountry2isocode(getRegions(x))# Country names to ISO3 code
    
    # reading historical data
    hist_cap <- readSource(type="IRENA",subtype="Capacity")/1000 # converting from MW to GW
    hist_gen <- readSource("IRENA", subtype = "Generation")# Units are GWh
    
    # Real world capacity factor for hydro = Generation in last year/Capacity in 2015
    cf_realworld <- hist_gen[,2015,"Hydropower"]/(8760*hist_cap[,2015,"Hydropower"]) 
    cf_realworld[is.na(cf_realworld)] <- 0
    getNames(cf_realworld) <- "Hydro"
    
    # renaming variable names from IRENA historical database   
    getNames(hist_cap)[c(2,7,8,10,11,12)] <- c("Hydro","Wind_ON","Wind_OFF","SolarPV",
                                               "SolarCSP","Biomass")
    
    x_tmp <- x # contains original targets + targets extrapolated to model years
    
    # Combining wind (on and offshore) as no distinction made in REMIND
    x_tmp <- add_columns(x_tmp ,addnm = "Wind", dim = 3.2)
    x_tmp[,,"Wind"] <- x_tmp[,,"Wind_ON"] + x_tmp[,,"Wind_OFF"]
    
     # Selecting relevant technologies i.e., all except "Base Year"
    techs <- c("SolarPV","SolarCSP","Wind","Hydro","Biomass","Geothermal")
    
    # 1. If targets are given in non-model year. e.g., 2022, 2032, then targets are extrapolated linearly.
    input_yr <- getYears(x,as.integer = TRUE)
    input_yr <- input_yr[input_yr%%5!=0] # years not multiple of 5/non-model years
    # Selecting relevant model years 
    target_years <- c(2020,2025,2030,2035,2040)
    regions <- getRegions(x_tmp)
    tech_n <- getNames(x_tmp[,,techs])
    for (r in regions){
      for (t in tech_n){
      for (i in input_yr){
        if(x_tmp[r,i,t]!=0){
      if (i>2015 & i<2020)
      x_tmp[r,2020,tech_n] <- setYears(x_tmp[r,i,tech_n])*(1+(2020-i)*0.05)
      if (i>2020 & i<2025)
      x_tmp[r,2025,tech_n] <- setYears(x_tmp[r,i,tech_n])*(1+(2025-i)*0.05)
      if (i>2025 & i<2030)
      x_tmp[r,2030,tech_n] <- setYears(x_tmp[r,i,tech_n])*(1+(2030-i)*0.05)
      if (i>2030 & i<2035 )
      x_tmp[r,2035,tech_n] <- setYears(x_tmp[r,i,tech_n])*(1+(2035-i)*0.05)
      if (i>2035 & i<2040 )
      x_tmp[r,2040,tech_n] <- setYears(x_tmp[r,i,tech_n])*(1+(2040-i)*0.05)
      # else
      # x_tmp[,i,tech_n] <- x_tmp[,i,tech_n]
    }
      }
      }
    }
    
    #for now# x_tmp <- x_tmp[,target_years,] #  only take model years
    
    # Creating new magpie object containing historical or base year targets
    x_cap <- new.magpie(getRegions(x_tmp),target_years,techs)
    x_cap[is.na(x_cap)] <- 0 # replacing NAs with zero
    # Capacity factors in REMIND. From : calcOutput("Capacityfactor"...)
    cf_biomass <- 0.75 
    cf_geothermal <- 0.8
    cf_hydro <- cf_realworld

    
    # Initialising all capacities for all model years to historical 2015 capacities.
    # except when Base year is mentioned (then initialise with base year capacities) and except for Hydro which will get historical generation.
  
    year <- getYears(x_tmp)
    for (t in year){
      for (r in getRegions(x_tmp)){
        if (x_tmp[r,t,"AC-Absolute.Base year"]!=0)
          x_cap[r,,]  <- setYears(hist_cap[r,x_tmp[r,t,"AC-Absolute.Base year"],techs])
        else
          x_cap[r,,]  <- setYears(hist_cap[r,2015,techs])
      }
    } 
    
    # for hydro overwrite capacity  with generation
    x_cap[,,"Hydro"] <- setYears(hist_gen[getRegions(x_cap),2015,"Hydropower"])
    
  
    
    # for (t in target_years){
    #   for (r in getRegions(x_tmp)){
    #     if (x_tmp[r,t,"AC-Absolute.Base year"]!=0)
    #       x_cap[r,t,techs]  <- setYears(hist_cap[r,x_tmp[r,t,"AC-Absolute.Base year"],techs])
    #     else
    #       x_cap[r,t,techs]  <- setYears(hist_cap[r,2015,techs])
    #   }
    # }
    
  
    #x_current <- x_cap
    x_cap_ac <- x_cap # object will contain additional capacity targets as absolute capacity targets
    x_cap_ac[] <- 0
    x_cap_pt <- x_cap  # object will contain generation targets as absolute capacity targets
    x_cap_pt[] <- 0
    x_cap_tic <- x_cap # object will contain total installed capacity targets 
    x_cap_tic[] <- 0
    

    
    # Converting additional capacity targets to absolute capacity targets.
    # x_new_abs[,,"Biomass"] <- x_current[,,"Biomass"] + x_tmp[,,"AC-Absolute.Biomass",drop=TRUE]
    # 
    x_cap_ac[,,c("Biomass","Wind","SolarPV","SolarCSP")] <- x_cap[,,c("Biomass","Wind","SolarPV","SolarCSP")] + 
      x_tmp[,target_years,c("AC-Absolute.Biomass","AC-Absolute.Wind","AC-Absolute.SolarPV","AC-Absolute.SolarCSP"),drop=TRUE]
    
    # For hydro converting to additional generation targets
    x_cap_ac[,,"Hydro"] <- x_cap[,,"Hydro"] + x_tmp[,target_years,"AC-Absolute.Hydro",drop=TRUE]*setYears(cf_hydro[getRegions(x_tmp),,]*8760)
    
    # x_new_abs[,,c("Biomass","Wind","SolarPV","SolarCSP")] <- x_current[,,c("Biomass","Wind","SolarPV","SolarCSP")] + 
    #   x_tmp[,,c("AC-Absolute.Biomass","AC-Absolute.Wind","AC-Absolute.SolarPV","AC-Absolute.SolarCSP"),drop=TRUE]
    # # For hydro converting to additional generation targets
    # x_new_abs[,,"Hydro"] <- x_current[,,"Hydro"] + x_tmp[,,"AC-Absolute.Hydro",drop=TRUE]*setYears(cf_hydro[getRegions(x_tmp),,]*8760)
    
    # Total installed capacity Targets for all technologies 
    
    x_cap_tic[,,c("Wind","SolarPV","SolarCSP","Biomass")] <- pmax(x_cap[,,c("Wind","SolarPV","SolarCSP","Biomass")],x_tmp[,target_years,c("Wind","SolarPV","SolarCSP","Biomass")][,,"TIC-Absolute",drop = TRUE])
    x_cap_tic[,,"Hydro"] <- pmax(x_cap[,,"Hydro"],x_tmp[,target_years,"TIC-Absolute.Hydro",drop = TRUE]*setYears(cf_hydro[getRegions(x_tmp),,]*8760))

    # Converting Production targets (GWh) to Capacity targets (TIC-Absolute) (GW) for geothermal and biomass
    # pmax takes the higher value from existing capacity and new capacity derived (from production)
    x_cap_pt[,,"Biomass"] <- pmax(x_cap[,,"Biomass"],x_tmp[,target_years,"Production-Absolute.Biomass"]/(8760*cf_biomass))
    
    x_cap_pt[,,"Geothermal"] <- pmax(x_cap[,,"Geothermal"],x_tmp[,target_years,c("Production-Absolute.Geothermal")]/(8760*cf_geothermal))    
  # for solar, wind, and hydro conversion will be done using another method below
  x_cap_pt[,target_years,c("SolarPV","SolarCSP","Wind")] <- x_tmp[,target_years,c("SolarPV","SolarCSP","Wind")][,,"Production-Absolute",drop=TRUE]
  
  
# Converting Production targets to capacity targets for solar and wind
# Fpr Hydro, all targets (which have already been converted into generation targets) will be converted into capacity targets based on maxprod and capacity factors.
#

  # x_tmp[,target_years,"Production-Absolute.Hydro"] <- pmax(x_tmp[,target_years,"Production-Absolute.Hydro"],x_cap_tic[,,"Hydro"],x_cap_ac[,,"Hydro"])
  # Special case for hydro as mentioned above
  x_cap_pt[,target_years,"Hydro"] <- pmax(x_tmp[,target_years,"Production-Absolute.Hydro"],x_cap_tic[,,"Hydro"],x_cap_ac[,,"Hydro"])

# Obtaining the capacity factors (nur) values and associated maxproduction (maxprod) for Hydro, Wind, and Solar

    data_wind <- calcOutput("PotentialWindOn", aggregate = FALSE)
    # Reordering dim=3 for data_wind so that 1st position corresponds to maxprod.nur.1 and not maxprod.nur.9
    data_wind_sorted <- mbind(data_wind[,,"1"],data_wind[,,"2"],data_wind[,,"3"],data_wind[,,"4"],
                              data_wind[,,"5"],data_wind[,,"6"],data_wind[,,"7"],data_wind[,,"8"],data_wind[,,"9"])
    data_hydro <- calcOutput("PotentialHydro", aggregate = FALSE)
    #data_solar <- calcOutput("Solar", aggregate = FALSE)
    data_solar <- calcOutput("Solar")
    names_solarPV <- paste0("SolarPV.",getNames(collapseNames((mselect(data_solar,type=c("nur","maxprod"),technology="spv")),collapsedim = 2)))
    names_solarCSP <- paste0("SolarCSP.",getNames(collapseNames((mselect(data_solar,type=c("nur","maxprod"),technology="csp")),collapsedim = 2)))
    names_hydro <- paste0("Hydro.",getNames(data_hydro))
    names_wind <- paste0("Wind.",getNames(data_wind_sorted))
    data_combined <- new.magpie(getRegions(data_hydro), NULL, c(names_solarPV,names_solarCSP,names_hydro,names_wind))
    data_combined[,,"Hydro"] <- data_hydro
    data_combined[,,"Wind"] <- data_wind_sorted
    
    # hard-coding values for countries with solar pv and csp generation data (only KOT and MKD) as country level maxprod
    # and nur value for solar (in data_solar) are  not yet available. 
    data_combined[c("KOR","MKD"),,"SolarPV"][,,"maxprod"]  <- as.vector(data_solar[c("JPN","EUR"),,"maxprod"][,,"spv"])
    data_combined[c("KOR","MKD"),,"SolarPV"][,,"nur"]  <- as.vector(data_solar[c("JPN","EUR"),,"nur"][,,"spv"])
    data_combined[c("KOR","MKD"),,"SolarCSP"][,,"maxprod"]  <- as.vector(data_solar[c("JPN","EUR"),,"maxprod"][,,"csp"])
    data_combined[c("KOR","MKD"),,"SolarCSP"][,,"nur"]  <- as.vector(data_solar[c("JPN","EUR"),,"nur"][,,"csp"])
    data_combined <- data_combined[getRegions(x_tmp),,]# only interested in limited dataset
    
    for (n in getNames(data_combined,dim=1)){
      name=paste0(n,".maxprod")
      # Conversion from EJ/a to GWh
      data_combined[,,name,pmatch=TRUE] <- data_combined[,,name,pmatch=TRUE]*277777.778
    }
    
    final <- numeric(nregions(x_tmp))
    names(final) <- getRegions(x_tmp)
    tmp_target <- numeric(10)
    x_cap_pt_2_tic <- x_cap
    x_cap_pt_2_tic[] <- 0
    # # For all countries which have non-zero generation values but zero or negative maxprod() for hydro (i.e., for some read), replace x_tmp[,,"Production-Absolute.Hydro]==0,  Even if there is one +ve production absolute value for Hydro but all maxprod are zero
    # for (r in names(final)){
    #   if(any(x_tmp[r,,"Production-Absolute.Hydro"]!=0) & 
    #      all(data_combined[r,,"Hydro.maxprod"]==0)|any(data_combined[r,,"Hydro.maxprod"]<0) )
    #     x_tmp[r,,"Production-Absolute.Hydro"] <- 0
    # }
    # 
    for (t in c("SolarPV","SolarCSP","Wind","Hydro")){
      data_sel <- data_combined[,,t]
      data_in_use <-  data_sel[,,"maxprod"]/data_sel[,,"nur"]
      for (y in target_years){
        final[] <-0
        for (r in names(final)){
          #Only start loop if Production targets are non-zero and if the maxprod for that country can absorb the targets set.
          name <- paste0(t,".maxprod")
          #name2 <- paste0("Production-Absolute.",t)
          if (!isZero(x_cap_pt[r,y,t] & 
              dimSums(data_combined[r,,name]) > max(x_cap_pt[r,,t]))){             # extracting the first non-zero location of maxprod
            # name <- paste0(t,".maxprod")
            loc <- min(which(!isZero(data_combined[r,,name,pmatch=TRUE])))
            tmp_target[1] <- x_cap_pt[r,y,t]
            if (data_sel[r,,"maxprod"][,,loc] > tmp_target[1]){
              final[r] <- tmp_target[1]/(8760*data_sel[r,,"nur"][,,loc])
            } else {tmp_target[2] <- tmp_target[1] - data_sel[r,,"maxprod"][,,loc]
            if(data_sel[r,,"maxprod"][,,loc+1] > tmp_target[2]){
              final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + tmp_target[2]/data_sel[r,,"nur"][,,loc+1])
            } else {tmp_target[3] <- tmp_target[2] - data_sel[r,,"maxprod"][loc+1]
            if(data_sel[r,,"maxprod"][,,loc+2] > tmp_target[3]){
              final[r] <- (1/8760)*(data_in_use[r,,][loc] + data_in_use[r,,][loc+1] +
                                      tmp_target[3]/data_sel[r,,"nur"][loc+2])
            } else {tmp_target[4] <- tmp_target[3] - data_sel[r,,"maxprod"][,,loc+2]
            if(data_sel[r,,"maxprod"][,,loc+3] > tmp_target[4]){
              final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][loc+1] + data_in_use[r,,][,,loc+2] +
                                      tmp_target[4]/data_sel[r,,"nur"][,,loc+3])
              
            } else {tmp_target[5] <- tmp_target[4] - data_sel[r,,"maxprod"][,,loc+3]
            if(data_sel[r,,"maxprod"][loc+4] > tmp_target[5]){
              final[r] <-(1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][loc+1] + data_in_use[r,,][,,loc+2] +
                                     data_in_use[r,,][,,loc+3] + tmp_target[5]/data_sel[r,,"nur"][,,loc+4])
            } else {tmp_target[6] <- tmp_target[5] - data_sel[r,,"maxprod"][,,loc+4]
            if(data_sel[r,,"maxprod"][loc+5] > tmp_target[6]){
              final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][loc+1] + data_in_use[r,,][,,loc+2] +
                                      data_in_use[r,,][,,loc+3] + data_in_use[r,,][,,loc+4] +
                                      tmp_target[6]/data_sel[r,,"nur"][,,loc+5])
            } else {tmp_target[7] <- tmp_target[6] - data_sel[r,,"maxprod"][,,loc+5]
            if(data_sel[r,,"maxprod"][loc+6] > tmp_target[7]){
              final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][loc+1] + data_in_use[r,,][,,loc+2] +
                                      data_in_use[r,,][,,loc+3] + data_in_use[r,,][,,loc+4] + data_in_use[r,,][,,loc+5]
                                    + tmp_target[7]/data_sel[r,,"nur"][,,loc+6])
            }  else {tmp_target[8] <- tmp_target[7] - data_sel[r,,"maxprod"][,,loc+6]
            if(data_sel[r,,"maxprod"][loc+7] > tmp_target[8]){
              final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][loc+1] + data_in_use[r,,][,,loc+2] +
                                      data_in_use[r,,][,,loc+3] + data_in_use[r,,][,,loc+4] + data_in_use[r,,][,,loc+5]
                                    +  data_in_use[r,,][,,loc+6]+ tmp_target[8]/data_sel[r,,"nur"][,,loc+7])
            }
            }
            }
            }
            }
            }
            }
            }
          }
          }
        x_cap_pt_2_tic[,y,t] <-final
      }
    }  
    
#################################################
    # x_new_prod_sswh <- x_cap
    # x_new_prod_sswh[] <- 0
    # for (t in c("SolarPV","SolarCSP","Wind","Hydro")){
    #   data_sel <- data_combined[,,t]
    #   data_in_use <-  data_sel[,,"maxprod"]/data_sel[,,"nur"]
    #   for (y in target_years){
    #     final[] <-0
    #     for (r in names(final)){
    #       #Only start loop if Production targets are non-zero and if the maxprod for that country can absorb the targets set.
    #       name <- paste0(t,".maxprod")
    #       name2 <- paste0("Production-Absolute.",t)
    #       if (!isZero(x_tmp[,,"Production-Absolute"][,,t])[r,y,] &
    #           dimSums(data_combined[r,,name]) > max(x_tmp[r,,name2])){
    #         # extracting the first non-zero location of maxprod
    #         # name <- paste0(t,".maxprod")
    #         loc <- min(which(!isZero(data_combined[r,,name,pmatch=TRUE])))
    #         tmp_target[1] <- x_tmp[r,y,"Production-Absolute"][,,t]
    #         if (data_sel[r,,"maxprod"][,,loc] > tmp_target[1]){
    #           final[r] <- tmp_target[1]/(8760*data_sel[r,,"nur"][,,loc])
    #         } else {tmp_target[2] <- tmp_target[1] - data_sel[r,,"maxprod"][,,loc]
    #         if(data_sel[r,,"maxprod"][,,loc+1] > tmp_target[2]){
    #           final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + tmp_target[1]/data_sel[r,,"nur"][,,loc+1])
    #         } else {tmp_target[3] <- tmp_target[2] - data_sel[r,,"maxprod"][loc+1]
    #         if(data_sel[r,,"maxprod"][,,loc+2] > tmp_target[3]){
    #           final[r] <- (1/8760)*(data_in_use[r,,][loc] + data_in_use[r,,][loc+1] +
    #                                   tmp_target[2]/data_sel[r,,"nur"][loc+2])
    #         } else {tmp_target[4] <- tmp_target[3] - data_sel[r,,"maxprod"][,,loc+2]
    #         if(data_sel[r,,"maxprod"][,,loc+3] > tmp_target[4]){
    #           final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][loc+1] + data_in_use[r,,][,,loc+2] +
    #                                   tmp_target[3]/data_sel[r,,"nur"][,,loc+3])
    #           final[r] <- tmp_target[1]
    #         } else {tmp_target[5] <- tmp_target[4] - data_sel[r,,"maxprod"][,,loc+3]
    #         if(data_sel[r,,"maxprod"][loc+4] > tmp_target[5]){
    #           final[r] <-(1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][loc+1] + data_in_use[r,,][,,loc+2] +
    #                                  data_in_use[r,,][,,loc+3] + tmp_target[4]/data_sel[r,,"nur"][,,loc+4])
    #         } else {tmp_target[6] <- tmp_target[5] - data_sel[r,,"maxprod"][,,loc+4]
    #         if(data_sel[r,,"maxprod"][loc+5] > tmp_target[6]){
    #           final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][loc+1] + data_in_use[r,,][,,loc+2] +
    #                                   data_in_use[r,,][,,loc+3] + data_in_use[r,,][,,loc+4] +
    #                                   tmp_target[5]/data_sel[r,,"nur"][,,loc+5])
    #         } else {tmp_target[7] <- tmp_target[6] - data_sel[r,,"maxprod"][,,loc+5]
    #         if(data_sel[r,,"maxprod"][loc+6] > tmp_target[7]){
    #           final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][loc+1] + data_in_use[r,,][,,loc+2] +
    #                                   data_in_use[r,,][,,loc+3] + data_in_use[r,,][,,loc+4] + data_in_use[r,,][,,loc+5]
    #                                 + tmp_target[6]/data_sel[r,,"nur"][,,loc+6])
    #         }  else {tmp_target[8] <- tmp_target[7] - data_sel[r,,"maxprod"][,,loc+6]
    #         if(data_sel[r,,"maxprod"][loc+7] > tmp_target[8]){
    #           final[r] <- (1/8760)*(data_in_use[r,,][,,loc] + data_in_use[r,,][loc+1] + data_in_use[r,,][,,loc+2] +
    #                                   data_in_use[r,,][,,loc+3] + data_in_use[r,,][,,loc+4] + data_in_use[r,,][,,loc+5]
    #                                 +  data_in_use[r,,][,,loc+6]+ tmp_target[7]/data_sel[r,,"nur"][,,loc+7])
    #         }
    #         }
    #         }
    #         }
    #         }
    #         }
    #         }
    #         }
    #       }
    # 
    #     }
    #     x_new_prod_sswh[,y,t] <-final
    #   }
    # }
    # 
  
  # combining generation to installed capacity targets of biomass and geothermal from before
  x_cap_pt_2_tic <- mbind(x_cap_pt_2_tic[,,c("SolarPV","SolarCSP","Wind","Hydro")],x_cap_pt[,,c("Biomass","Geothermal")])
  # choose the maximum of installed capacity targets
  techs_e_hydro <- setdiff(techs,"Hydro")  
  x_cap_pt_2_tic[,,techs_e_hydro] <- pmax(x_cap_ac[,,techs_e_hydro],x_cap_tic[,,techs_e_hydro],x_cap_pt_2_tic[,,techs_e_hydro])  
    
#     x_new_gen <- mbind(x_new_prod_sswh[,,c("SolarPV","SolarCSP","Wind","Hydro")],x_new_prod_bg[,,c("Biomass","Geothermal")])
# x_new[,,c("SolarPV","SolarCSP","Wind","Hydro","Biomass","Geothermal")] <- pmax(x_new_abs[,,c("SolarPV","SolarCSP","Wind","Hydro","Biomass","Geothermal")],                                                                   x_new_gen[,,c("SolarPV","SolarCSP","Wind","Hydro","Biomass","Geothermal")],
#                                                                                x_new_tic[,,c("SolarPV","SolarCSP","Wind","Hydro","Biomass","Geothermal")])


# for hydro revert to capacity targets from previous for loop    
#x_new[,,"Hydro"] <- x_new_gen[,,"Hydro"]
    # Making sure that targets in subsequent years are always same or greater than the proceeding year
    # for (r in regions){
    #   for (t in techs){
    #     for(i in c(2020,2025,2030,2035)){
    #       if(x_new[r,i+5,t] < setYears(x_new[r,i,t])){
    #         x_new[r,i+5,t] <- setYears(x_new[r,i,t])
    #       }
    #     }
    #   }
    # }
  
  for (r in regions){
    for (t in techs){
      for(i in c(2020,2025,2030,2035)){
        if(x_cap_pt_2_tic[r,i+5,t] < setYears(x_cap_pt_2_tic[r,i,t])){
          x_cap_pt_2_tic[r,i+5,t] <- setYears(x_cap_pt_2_tic[r,i,t])
        }
      }
    }
  }
    
    # countries not in the REN21 capacity targets database
  rest_regions <- setdiff(getRegions(hist_cap),getRegions(x_cap))
     x_other <- new.magpie(rest_regions,target_years,techs)
    # for all other countries not in database, targets for all model years are historical capacities
    x_other[,,c("Wind","SolarPV","SolarCSP","Biomass","Geothermal")] <- setYears(hist_cap[rest_regions,2019,techs_e_hydro])
    x_other[,,"Hydro"] <- setYears(hist_cap[rest_regions,2019,"Hydro"])*setYears(cf_hydro[rest_regions,,])
    x_final <- magpiesort(mbind(x_cap_pt_2_tic,x_other))
    x <- x_final
    x[is.na(x)] <- 0
    #renaming to REMIND convention
    getNames(x) <- c("spv","csp","wind","hydro","biochp","geohdr") 
  } else if (subtype == "investmentCosts") {
    
    # save data of specific countries
    x_country <- x[c("China","India","United States"),,]
    # translate country names into ISO-codes
    getRegions(x_country) <- toolCountry2isocode(getRegions(x_country))
    # delete those countries from x
    x <- x[c("China","India","United States"),,,invert=TRUE]
    
    # split up regional data into countries
    map <- read.csv("regionmappingREN2Country.csv",sep=";")
    x <- toolAggregate(x,map,weight=NULL)
    
    # overwrite country data
    x[getRegions(x_country),,] <- x_country

  }
  
  return (x)
}