#' @importFrom dplyr mutate_ rename_ filter_ select_ 
#' @importFrom tidyr gather_
# NH3 only has one scenario

readECLIPSE <- function(subtype) {
  
  if (is.null(subtype) | ! subtype %in% c('activities.aggregated', 'activities.extended', 'emissions.aggregated', 'emissions.extended', 'shipping.emi', 'shipping.ef')) 
    stop("Please provide one of the following subtypes: 'activities.aggregated', 'activities.extended', 'emissions.aggregated', 'emissions.extended', 'shipping.emi' or 'shipping.ef'")
  
  # Some sectors do not have any values or are not needed (remove them)
  rm_sectors = c("")
    #"AACID","CHEM","CHEMBULK","CUSM",
    #"Transformation_Coal", "Transformation_HLF", "Transformation_NatGas")
    #"Losses_Coal", "Losses_Distribution_Use", 
  rm_unwanted = c("Sum","Total","Unattributed")
  
  if (subtype == "activities.aggregated") {
    ap_mfr <- read_excel("ACTIVITIES_MFR_EMF30_aggregated_2021.xlsx", sheet="Air pollutants")
    ap_mfr$'Scenario' <- 'MFR'
    
    ap_cle <- read_excel("ACTIVITIES_CLE_EMF30_aggregated_2021.xlsx", sheet="Air pollutants")
    ap_cle$'Scenario' <- 'CLE'
    
    ap_sle <- ap_mfr
    ap_sle$'Scenario' <- 'SLE'
    
    ap  <- rbind(ap_mfr,ap_cle, ap_sle)
    ap  <- ap[!is.na(ap[[1]]),]
    
    ap <- ap %>% 
      gather_("year", "value",  setdiff(colnames(ap),c("Region","Scenario","Sector","Unit"))) %>% 
      mutate_(year=~as.numeric(paste(year))) %>% 
      rename_(region=~Region,scenario=~Scenario, sector=~Sector, unit=~Unit) %>% 
      mutate_(region = ~gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", region))) %>% 
      filter_(~!sector %in% c(rm_sectors, rm_unwanted), ~region!="Global") %>% 
      select_(~-unit) %>% 
      as.data.frame()
    
    # Remove spurious data
    ap <- ap %>% 
      filter_(~!(region == "Japan"  & sector == "End_Use_Residential_Coal")) %>%  # No activity data available
      filter_(~!(region == "Turkey" & sector == "End_Use_Transport_Coal"))        # No activity data available
    
    x <- as.magpie(ap, spatial=1, temporal=4)
  }
  
  if (subtype == "activities.extended") {
    ap_mfr <- read_excel("ACTIVITIES_MFR_EMF30_extended_2021.xlsx", sheet="Air pollutants")
    ap_mfr$'Scenario' <- 'MFR'
    
    ap_cle <- read_excel("ACTIVITIES_CLE_EMF30_extended_2021.xlsx", sheet="Air pollutants")
    ap_cle$'Scenario' <- 'CLE'
    
    
    ap_sle <- ap_mfr
    ap_sle$'Scenario' <- 'SLE'
    
    ap  <- rbind(ap_mfr,ap_cle, ap_sle)

    ap  <- ap[!is.na(ap[[1]]),]
    
    ap <- ap %>% 
      gather_("year", "value",  setdiff(colnames(ap),c("Region","Scenario","Sector","Unit"))) %>% 
      mutate_(year=~as.numeric(paste(year))) %>% 
      rename_(region=~Region,scenario=~Scenario, sector=~Sector, unit=~Unit) %>% 
      mutate_(region = ~gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", region))) %>% 
      filter_(~!sector %in% c(rm_sectors, rm_unwanted), ~region!="Global") %>% 
      select_(~-unit) %>% 
      as.data.frame()
    
    x <- as.magpie(ap, spatial=1, temporal=4)
  }
  
  if (subtype == "emissions.aggregated") {
    species <- c("SO2", "NH3", "NOx", "VOC", "BC", "OC", "CO","PM25")
    
    cle <- do.call("bind_rows", 
                   lapply(species, 
                          function(s) {
                            out <- read_excel("EMISSIONS_CLE_EMF30_aggregated_2021.xlsx", sheet=s)
                            out  <- out[!is.na(out[[1]]),]
                            out <- out %>% 
                              gather_("year", "value",  setdiff(colnames(out),c("Region","Sector"))) %>% 
                              mutate_(year=~as.numeric(paste(year))) %>% 
                              rename_(region=~Region, sector=~Sector) %>% 
                              mutate_(region = ~gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", region))) %>% 
                              mutate_(variable = ~s) %>% 
                              filter_(~!sector %in% c(rm_sectors, rm_unwanted), ~region != "Global")
                            
                            return(out)
                          })) %>% 
      mutate_(scenario = ~"CLE")
    
    mfr <- do.call("bind_rows", 
                   lapply(species, 
                          function(s) {
                            out <- read_excel("EMISSIONS_MFR_EMF30_aggregated_2021.xlsx", sheet=s)
                            out  <- out[!is.na(out[[1]]),]
                            out <- out %>% 
                              gather_("year", "value",  setdiff(colnames(out),c("Region","Sector"))) %>% 
                              mutate_(year=~as.numeric(paste(year))) %>% 
                              rename_(region=~Region, sector=~Sector) %>% 
                              mutate_(region = ~gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", region))) %>% 
                              mutate_(variable = ~s) %>% 
                              filter_(~!sector %in% c(rm_sectors, rm_unwanted), ~region != "Global")
                            return(out)
                          })) %>% 
      mutate_(scenario = ~"MFR")
    
    # MFR only has 2030 and 2050, so add CLE values for other years
    x <- bind_rows(cle, 
                   bind_rows(cle %>% filter_(~year %in% c(2000,2005,2010,2020)) %>% mutate_(scenario = ~"MFR"), mfr)) %>% 
      select_(~region,~year,~sector,~variable,~scenario,~value)
    
    # Remove spurious data
    x <- x %>% 
      filter_(~!(region == "Japan"  & sector == "End_Use_Residential_Coal")) %>%  # No activity data available
      filter_(~!(region == "Turkey" & sector == "End_Use_Transport_Coal"))        # No activity data available
    
    x <- as.magpie(as.data.frame(x), spatial=1, temporal=2)
  }

  if (subtype == "emissions.extended") {
    species <- c("SO2", "NH3", "NOx", "VOC", "BC", "OC", "CO", "PM25")
    
    cle <- do.call("bind_rows", 
                   lapply(species, 
                          function(s) {
                            out <- read_excel("EMISSIONS_CLE_EMF30_extended_2021.xlsx", sheet=s)
                            out  <- out[!is.na(out[[1]]),]
                            out <- out %>% 
                              gather_("year", "value",  setdiff(colnames(out),c("Region","Sector"))) %>% 
                              rename_(region=~Region, sector=~Sector) %>% 
                              mutate_(region = ~gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", region))) %>% 
                              mutate_(variable = ~s) %>% 
                              filter_(~!sector %in% c(rm_sectors, rm_unwanted), ~region != "Global")
                            return(out)
                          })) %>% 
      mutate_(scenario = ~"CLE")
    
    mfr <- do.call("bind_rows", 
                   lapply(species, 
                          function(s) {
                            out <- read_excel("EMISSIONS_MFR_EMF30_extended_2021.xlsx", sheet=s)
                            out  <- out[!is.na(out[[1]]),]
                            out <- out %>% 
                              gather_("year", "value",  setdiff(colnames(out),c("Region","Sector"))) %>% 
                              rename_(region=~Region, sector=~Sector) %>% 
                              mutate_(region = ~gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", region))) %>% 
                              mutate_(variable = ~s) %>% 
                              filter_(~!sector %in% c(rm_sectors, rm_unwanted), ~region != "Global")
                            return(out)
                          })) %>% 
      mutate_(scenario = ~"MFR")
    
    # MFR only has 2030 and 2050, so add CLE values for other years
    x <- bind_rows(cle, 
                   bind_rows(cle %>% filter_(~year %in% c(2000,2005,2010,2020)) %>% mutate_(scenario = ~"MFR"), mfr)) %>% 
      select_(~region,~year,~sector,~variable,~scenario,~value)
    
    x <- as.magpie(as.data.frame(x), spatial=1, temporal=2)
  }
  
  if (subtype %in% c("shipping.emi", "shipping.ef")) {
    
    ship <- read_excel("Internattional_Shipping_EMF30_Ev5a_CLE-MFR.xlsx", sheet="output_to_R")
    
    tmp_CLE <- as.magpie(ship[ship$scenario=="CLE", !names(ship) == "scenario"])
    tmp_CLE <- add_dimension(tmp_CLE,dim=3.2,add="scenario",nm="CLE")
    
    tmp_MFR <- as.magpie(ship[ship$scenario=="MFR", !names(ship) == "scenario"])
    tmp_MFR <- add_dimension(tmp_MFR,dim=3.2,add="scenario",nm="MFR")
    
    tmp_ship <- mbind(tmp_CLE,tmp_MFR)
    
    # add 2100 and set to 2050 values
    tmp_ship <- add_columns(tmp_ship,"y2100",dim=2.1)
    tmp_ship[,2100,] <- setYears(tmp_ship[,2050,])
    
    # Add sector name
    getNames(tmp_ship) <- paste0("InternationalShipping.",getNames(tmp_ship))
    
    ship_act <- collapseNames(tmp_ship[,,"activity.CLE"]) # activity is scenario independent -> the same for CLE and MFR
    ship_emi <- tmp_ship[,,"activity",invert=TRUE]
    
    # add empty SSP dimensions
    ship_emi <- add_columns(ship_emi, addnm=c("SSP1","SSP2","SSP5"),dim=3.3)
    
    # fill SSPs with preliminary data and define scenarios below
    ship_emi[,,"SSP1"] <- ship_emi[,,"CLE"]
    ship_emi[,,"SSP5"] <- ship_emi[,,"SSP1"]
    ship_emi[,,"SSP2"] <- ship_emi[,,"CLE"] # higher than SSP1 and 5
    ship_ef  <- ship_emi/ship_act
    
    # remove 2040 (will be calcualted below by time_interpolate)
    ship_ef <- ship_ef[,2040,,invert=TRUE]
    
    # in some cases (CH4 and VOC) MFR is higher than CLE -> set MFR to CLE
    ship_ef[,,"MFR"] <- pmin(ship_ef[,,"MFR"],ship_ef[,,"CLE"])
    
    # define SSP5
    ship_ef[,2005,"SSP5"] <- ship_ef[,2005,"CLE"]
    ship_ef[,2010,"SSP5"] <- ship_ef[,2010,"CLE"]
    ship_ef[,2020,"SSP5"] <- ship_ef[,2020,"CLE"]
    ship_ef[,2030,"SSP5"] <- 0.5 * ship_ef[,2030,"CLE"] + 0.5 * ship_ef[,2030,"MFR"]
    ship_ef[,2050,"SSP5"] <- 0.25 * ship_ef[,2050,"CLE"] + 0.75 * ship_ef[,2050,"MFR"]
    ship_ef[,2100,"SSP5"] <- ship_ef[,2100,"MFR"]
    
    ship_ef  <- time_interpolate(ship_ef,  interpolated_year=seq(2005,2100,5), integrate_interpolated_years=TRUE, extrapolation_type="constant")
    ship_emi <- time_interpolate(ship_emi, interpolated_year=seq(2005,2100,5), integrate_interpolated_years=TRUE, extrapolation_type="constant")
    
    getSets(ship_ef) <- c("region","year","sector","emi","scenario")
    getSets(ship_emi) <- c("region","year","sector","emi","scenario")
    
  }
  
  if (subtype == "shipping.emi") {
    x <- ship_emi 
  }
  
  if (subtype == "shipping.ef") {
    x <- ship_ef 
  }
  
  return(x)
}
