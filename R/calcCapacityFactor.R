#' @title calc Capacity Factor
#' @description provides capacity factor values
#' @return magpie object of the capacity factor data
#' @author Renato Rodrigues, Stephen Bi
#' @importFrom plyr round_any
#' @examples
#'
#' \dontrun{
#' calcOutput("CapacityFactor")
#' }
#'

calcCapacityFactor <- function(){




  ### calculation of coal power capacity factor
  GWh_2_EJ <- 3.6e-6

  # Read capacity factor inputs
  global <- readSource("REMIND_11Regi", subtype="capacityFactorGlobal", convert = FALSE)
  # Set coal plant capacity factor long-term assumption to 50% (down from 60%)
  global[,,"pc"] <- 0.5
  # Read capacity factor rules
  rules <- readSource("REMIND_11Regi", subtype="capacityFactorRules")

  #   Creating new MAgPIE object to store the final capacity values
  output <- new.magpie(getRegions(rules),seq(2005,2150,5),getNames(global))


  # Merging global and rules values
  # Filling MagPIE object with global values
  output[,,getNames(global)] <- global[,,getNames(global)]


  ### Global Coal Plant Tracker Calcs ###
  # Read coal capacity and generation data to derive historical capacity factor rules
  # Read generation data from Energy Balances
  coalgen_c <- calcOutput("IO",subtype="output",aggregate = F)[,,"pecoal.seel"]
  coalgen_c <- dimSums(coalgen_c,dim=3)
  map <- toolGetMapping(getConfig("regionmapping"),type="regional", where = "mappingfolder")
  coalgen_R <- toolAggregate(coalgen_c,map,weight=NULL)
  getNames(coalgen_c) <- "pc"
  getNames(coalgen_R) <- "pc"

  #Read coal capacity data from GCPT
    hist_cap_coal_c <- readSource("GCPT",subtype="historical",convert=F)
  hist_cap_coal_R <- toolAggregate(hist_cap_coal_c,rel=map,weight=NULL)

  #Calculate historical 5-year average capacity factors by country
  coal_factor_c <- new.magpie(getRegions(hist_cap_coal_c),years = seq(2005, round_any(max(getYears(coalgen_c, as.integer = TRUE)), 5, f = floor)),names="pc",fill=0)
  coal_factor_R <- new.magpie(getRegions(hist_cap_coal_R),years = getYears(coal_factor_c),names="pc",fill=0)
  for (i in getYears(coal_factor_c,as.integer=TRUE)) {
    if ((i+2) %in% getYears(coalgen_c,as.integer=TRUE)) {
      coal_factor_c[,i,] <- dimSums(coalgen_c[,(i-2):(i+2),],dim=2)/(dimSums(hist_cap_coal_c[,(i-2):(i+2),],dim=2)*365*24 * GWh_2_EJ)
      coal_factor_R[,i,] <- dimSums(coalgen_R[,(i-2):(i+2),],dim=2)/(dimSums(hist_cap_coal_R[,(i-2):(i+2),],dim=2)*365*24 * GWh_2_EJ)
      #coal_factor_c[,i,][which(dimSums(hist_cap_coal_c[,(i-2):(i+2),],dim=2)==0)] <- 0
    # If latest available data falls in year 4 of a 5-year timestep
    }else if ((i+1) %in% getYears(coalgen_c,as.integer=TRUE)) {
      # Special treatment of 2020 due to COVID-19: half-weight 2020
      if (i==2020) {
        coal_factor_c[,i,] <- (dimSums(coalgen_c[,(i-2):(i+1),],dim=2) - 0.5 * dimSums(coalgen_c[,(i),],dim=2)) /
          ((dimSums(hist_cap_coal_c[,(i-2):(i+1),],dim=2) + dimSums(hist_cap_coal_c[,(i+1),],dim=2))*365*24 * GWh_2_EJ)
        coal_factor_R[,i,] <- (dimSums(coalgen_R[,(i-2):(i+1),],dim=2) + dimSums(coalgen_R[,(i+1),],dim=2)) /
          ((dimSums(hist_cap_coal_R[,(i-2):(i+1),],dim=2) - 0.5 * dimSums(hist_cap_coal_R[,(i),],dim=2))*365*24 * GWh_2_EJ)
      # For other timesteps, just use a 4-year average
      }else {
        coal_factor_c[,i,] <- dimSums(coalgen_c[,(i-2):(i+1),],dim=2) / (dimSums(hist_cap_coal_c[,(i-2):(i+1),],dim=2)*365*24 * GWh_2_EJ)
        coal_factor_R[,i,] <- dimSums(coalgen_R[,(i-2):(i+1),],dim=2) / (dimSums(hist_cap_coal_R[,(i-2):(i+1),],dim=2)*365*24 * GWh_2_EJ)
      }
    # If latest available data falls in the middle of a timestep
    }else {
      # if only data until 2020 is available, assign a double-weight to 2018 and 2019
      if (i==2020) {
        coal_factor_c[,i,] <- (dimSums(coalgen_c[,(i-2):i,],dim=2) + dimSums(coalgen_c[,(i-2):(i-1),],dim=2)) /
          ((dimSums(hist_cap_coal_c[,(i-2):i,],dim=2) + dimSums(hist_cap_coal_c[,(i-2):(i-1),],dim=2))*365*24 * GWh_2_EJ)
        coal_factor_R[,i,] <- (dimSums(coalgen_R[,(i-2):i,],dim=2) + dimSums(coalgen_R[,(i-2):(i-1),],dim=2)) /
          ((dimSums(hist_cap_coal_R[,(i-2):i,],dim=2) + dimSums(hist_cap_coal_R[,(i-2):(i-1),],dim=2))*365*24 * GWh_2_EJ)
      # For other timesteps, assign triple-weight to final year
      # Valid assumption for 2015 capacity factors
      }else {
        coal_factor_c[,i,] <- (dimSums(coalgen_c[,(i-2):i,],dim=2) + 2*coalgen_c[,i,]) /
                                 ((dimSums(hist_cap_coal_c[,(i-2):i,],dim=2) + 2*hist_cap_coal_c[,i,])*365*24 * GWh_2_EJ)
        coal_factor_R[,i,] <- (dimSums(coalgen_R[,(i-2):i,],dim=2) + 2*coalgen_R[,i,]) /
          ((dimSums(hist_cap_coal_R[,(i-2):i,],dim=2) + 2*hist_cap_coal_R[,i,])*365*24 * GWh_2_EJ)
      }
      #coal_factor_c[,i,][which(hist_cap_coal_c[,i,]==0)] <- 0
    }
    #Replace countries without coal power with regional capacity factor.
    coal_factor_c[,i,][which(!is.finite(coal_factor_c[,i,]))] <-
      coal_factor_R[map$RegionCode[which(map$CountryCode %in% getRegions(coal_factor_c)[which(!is.finite(coal_factor_c[,i,]))])],i,]
    # The derived coal capacity factors for Bosnia, Ireland and Myanmar are > 1 in some years for as yet unclear reasons.
    coal_factor_c[,i,][which(coal_factor_c[,i,]>1)] <- max(coal_factor_c[,i,][which(coal_factor_c[,i,]<1)])
  }

  #Derive coal capacity factor rule assumptions for 2020 - 2035 (linear convergence of historical trends to global default)
  hist_yr <- "y2015"
  start_yr <- "y2020"
  end_yr <- "y2030"
  conv_yr <- "y2035"

  coal_factor_c_years <- intersect(getYears(coal_factor_c), getYears(output))
  coal_factor_c_years <- coal_factor_c_years[coal_factor_c_years < start_yr]
  output[,getYears(output)[getYears(output) < start_yr],"pc"] <-
    coal_factor_c[,coal_factor_c_years,]

  output[,getYears(output)[which(getYears(output)>=conv_yr)],"pc"] <- global[,,"pc"]
  slope <- (output[,conv_yr,'pc'] - output[,hist_yr,'pc']) / (as.numeric(gsub("y","",conv_yr)) - as.numeric(gsub("y","",hist_yr)))
  for (t in getYears(output)[which(getYears(output)>=start_yr & getYears(output)<=end_yr)]) {
    output[,t,"pc"] <- output[,hist_yr,"pc"] + slope * (as.numeric(gsub("y","",t)) - as.numeric(gsub("y","",hist_yr)))
  }

  # Overwriting MAgPie object with rules values
  output[getRegions(rules),getYears(rules),getNames(rules)] <- ifelse(rules[getRegions(rules),getYears(rules),getNames(rules)]!=0, rules[getRegions(rules),getYears(rules),getNames(rules)], output[getRegions(rules),getYears(rules),getNames(rules)])

  # Convergence and lin.convergence functions are not working...
  # output[,getYears(output)[which(getYears(output)>=hist_yr & getYears(output)<=conv_yr)],"pc"] <-
  #   convergence(origin=output[,getYears(output)[which(getYears(output)>=hist_yr & getYears(output)<=conv_yr)],"pc"],
  #               aim=as.numeric(global[,,"pc"]),start_year=start_yr,end_year=conv_yr)
  # Define weight aggregation for capacity factors
  # using final energy as a proxy for the existent capacity factor to weight the capacity factor aggregation (it should be changed if the information about the existent capacity factor become available in the future)


  # change coal capacity factor in Germany to reflect observed decrease in coal electricity in recent years, note: to be checked whether necessary for other regions as well
  # https://static.agora-energiewende.de/fileadmin/Projekte/2021/2020_01_Jahresauswertung_2020/200_A-EW_Jahresauswertung_2020_WEB.pdf
  output["DEU",c("y2020","y2025"),"pc"] = 0.43
  output["DEU",c("y2030"),"pc"] = 0.4

  weight <- calcOutput("FE",source="IEA",aggregate=FALSE)[,2015,"FE (EJ/yr)"]

  # Return regions aggregation weighted by final energy
  return(list(x=output, weight=weight,
               unit="% of capacity",
               description="Installed capacity availability - capacity factor (fraction of the year that a plant is running)"
  ))

}
