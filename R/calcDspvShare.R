#' Calculates the share of distributed solar pv, wind-onshore/offshore, hydro-small/large from 2015 to 2050. For spv - Only includes grid-connected pv.
#' @details Known limitations - source for distributed spv (IEA Renewables 2019) is different than source for total spv (IRENA 2019)
#' @param subtype Either "current","expert", or "irena". Current are current shares extended until 2050. expert is based on Robert P.'s judgement, and irena are based on IRENA's 2050 global numbers. Hydro case remains same in all cases
#' @return magpie object with REMIND-aggregated regions

calcDspvShare <- function(subtype)
{
  # Note: Limitation - Different sources for absolute distributed solar pv (IEA), and absolute total solar pv (IRENA)
  
  IRENA_cap <- readSource("IRENA",subtype = "Capacity")
  
  #----------------------------------------
  # solar dpsv and solar utility
  
  dspv <- readSource("IEA_REN")[,2018,] # in GW
  total_spv <- IRENA_cap[,2018,"Solar photovoltaic"]/1000

  # to avoid getting NaN while dividing by total spv capacity, give countries with no spv, a very small value
  total_spv[which(total_spv==0),,] <- 0.0001 # in GW
  
  
  if (subtype=="expert"){
    share_spv <- new.magpie(getRegions(dspv),c(2015,2020,2030),names = "spv")
    share_spv[,,] <- dspv/total_spv # 2015 and 2020 get 2018 values
    # few countries have share greater than 1. This is because either all their solar pv is rooftop (but error exists because of different sources of the numerator and denominator)
    # or because of disaggregation methodology in readSource(IEA_REN). In any case, these countries don't have significant solar pv installations
    #  Force it to be 95%. 
    share_spv[which(share_spv[,2015,]>1),,] <- 0.95
    share_spv[which(share_spv[,2020,]>1),,] <- 0.95
    
    # data sources but is only for a small number of unimportant countries
    share_spv[,2030,] <- 0.3
    share_spv <- time_interpolate(share_spv,2025,integrate_interpolated_years = T)
  
    share_tmp <- share_spv[c("IND","JPN"),c(2015,2020,2030),]
    share_tmp[,2030,] <- 0.4
    share_tmp <- time_interpolate(share_tmp,2025,integrate_interpolated_years = T)
    
    share_spv[c("IND","JPN"),,] <- share_tmp
    # all shares after 2030 until 2050 have same value as 2030
    share_spv <- add_columns(share_spv,addnm = c(2035,2040,2045,2050),dim=2.1)
    share_spv[,seq(2035,2050,5),] <- setYears(share_spv[,2030,])
  }
  if (subtype=="current"){
    share_spv <- new.magpie(getRegions(dspv),seq(2015,2050,5),names = "spv")
    share_spv[,,] <- dspv/total_spv
  
  }
  
  if (subtype=="irena"){
    share_spv <- new.magpie(getRegions(dspv),c(2015,2020,2050),names = "spv")
    share_spv[,2015,] <- dspv/total_spv
    share_spv[,2020,] <- dspv/total_spv
    share_spv[,2050,] <- 0.4 # from IRENA https://irena.org/-/media/Files/IRENA/Agency/Publication/2019/Nov/IRENA_Future_of_Solar_PV_2019.pdf, however these numbers are global 2050. Disaggregate to regions
    share_spv <- time_interpolate(share_spv,seq(2020,2050,5),integrate_interpolated_years = T)
  }
  # share of dspv/utilty until 2030 converges to 0.3 linearly except for Japan and India where it is 0.4 
 
  
  #-----------------------------------------
  
  #----------------------------
  # Shares - Wind on and offshore
  mapping_remind <- toolGetMapping(getConfig()[1],where = "mappingfolder",type = "regional")

  wind_off <- IRENA_cap[,c(2015,2018),"Offshore wind energy"]/1000 # converting to GW
  wind_tot <- IRENA_cap[,c(2015,2018),"Wind"]/1000
  
  # to avoid getting NaN while dividing by total wind capacity, give countries with no wind, a very small value
  wind_tot[which(wind_tot[,2015,]==0),,] <- 0.00001 # in GW
  wind_tot[which(wind_tot[,2018,]==0),,] <- 0.00001 # in GW
  
    
  # top 20 countries by coastline in CIA's World factbook coastline 2020 - https://www.citypopulation.de/en/world/bymap/Coastlines.html
  top_20 <- c("Canada","Norway","Indonesia","Greenland","Russia","Philippines","Japan","Australia",
              "United States of America","Antarctica","New Zealand","China","Greece","United Kingdom",
              "Mexico","Italy","Brazil","Denmark","Turkey","India")
  
  top_20 <- toolCountry2isocode(top_20)
  # countries with no coastline/landlocked
  no_coast <- c("Monaco","Afghanistan","Andorra","Armenia","Austria","Azerbaijan","Belarus","Bhutan","Bolivia","Botswana",
               "Burkina Faso","Burundi","Central African Republic","Chad","Czech Republic","Eswatini","Ethiopia","Hungary",
               "Kazakhstan","Kyrgyzstan","Laos","Lesotho","Liechtenstein","Luxembourg","Malawi","Mali",
               "Moldova","Mongolia","Nepal","Niger","North Macedonia","Paraguay","Rwanda","San Marino","Serbia",
               "Slovakia","South Sudan","Switzerland","Tajikistan","Turkmenistan","Uganda","Uzbekistan","Zambia","Zimbabwe")
  no_coast <- toolCountry2isocode(no_coast)  
  # remaining countries
  rem_countries <- setdiff(mapping_remind$CountryCode,c(top_20,no_coast))
  # share in 2015 
  share_wind <- new.magpie(mapping_remind$CountryCode,seq(2015,2050,5),names = "wind")
  share_wind[,2015,] <- wind_off[,2015,]/wind_tot[,2015,]
  share_wind[,2020,] <- wind_off[,2018,]/wind_tot[,2018,] # 2020 gets 2018 values
  if (subtype=="expert"){
  # countries with longest coastline have 30% wind share in 2050. Interpolated linearly.
    share_top_20 <- share_wind[top_20,c(2020,2050),]
    share_top_20[,2050,] <- 0.3
    share_top_20 <- time_interpolate(share_top_20,seq(2020,2050,5),integrate_interpolated_years = T)
    share_wind[top_20,getYears(share_top_20),] <- share_top_20
    # all other countries with a shore line have 10% wind share in 2050. Interpolated linearly
    share_rem <- share_wind[rem_countries,c(2020,2050),]
    share_rem[,2050,] <- 0.1
    share_rem <- time_interpolate(share_rem,seq(2025,2045,5),integrate_interpolated_years = T)
    share_wind[rem_countries,getYears(share_rem),] <- share_rem
    
    share_wind[no_coast,,] <- 0 # landlocked countries
  #share_wind <- mbind(share_wind[no_coast,,],share_top_20,share_rem)
  }
  
  if (subtype=="current"){
  share_wind[,,] <-   wind_off[,2018,]/wind_tot[,2018,]
  share_wind[,2015,] <- wind_off[,2015,]/wind_tot[,2015,]
  }
  
  if (subtype=="irena"){
  share_tmp <- new.magpie(mapping_remind$CountryCode,years = c(2020,2050),names = "wind")
  share_tmp[,"y2020",] <- share_wind[,2020,]
  share_tmp[,"y2050",] <- 0.2
  share_tmp <- time_interpolate(share_tmp,seq(2025,2045,5),integrate_interpolated_years = T)
  share_wind[,getYears(share_tmp),] <- share_tmp 
  share_wind[no_coast,,] <- 0
  }
  
  #-----------------------------------------------------
  
  #---------------------------------
  # Share - hydro small in total hydro (small hydro <10 MW)
  ### From IRENA capacity statistics 2017, for WORLD
  # Total Hydropower capacity (excl. pumped storage and mixed plants) - 1083489 MW
  # Total Hydropower capacity (>10 MW) : 935425 MW
  # Total  Hydropower capacity (<10 MW) : 31277 + 116787 = 148064 MW
  # share of <10 MW (small) to total capacity:  148064/(935425+148064) = 0.136 
  # share is assumed to be the same world over and remain the same until 2050.
  if (subtype=="current"||subtype=="expert"||subtype=="irena"){
    share_hydro <- new.magpie(mapping_remind$CountryCode,seq(2015,2050,5),names = "hydro",fill=0.14) # all countries get world average
  }
  #----------------------------------------
  
  ## Combining all
  share_all <- mbind(share_spv,share_wind,share_hydro)
  
 
  ## Weights
  wt <- IRENA_cap[,"y2018","Total renewable energy"]

  return(list(x           = share_all,
              weight      = wt,
              unit        = "none",
              description = "share of distributed solar pv in total pv"))
}
