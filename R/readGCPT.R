#' Data from the Global Coal Plant Tracker January 2021 release by Global Energy Monitor (formerly EndCoal/CoalSwarm)
#' @description  Historical data of operating, under-construction, planned and announced Coal Plants by country (in MW) 
#' from the Global Energy Monitor's Global Coal Plant Tracker, and extrapolations for 2025 capacity scenarios
#' @param subtype Options are status, historical, future, lifespans, comp_rates and emissions
#' @author Stephen Bi
#' @importFrom readxl read_excel
#' @importFrom dplyr %>% filter select mutate summarize group_by left_join across everything
#' @importFrom magclass replace_non_finite
#' @aliases readEndCoal


readGCPT <- function(subtype) {
  map <- toolGetMapping(getConfig("regionmapping"), type="regional")
  
  if (!(subtype %in% c("historical","status","future","lifespans","emissions","comp_rates","reg_comp_rates","meanAge","ppca_emi",
                       "historical2020","status2020","future2020","lifespans2020","emissions2020","comp_rates2020","2030"))) {
    stop("Invalid subtype!")
  }
  
  # Names of data files to be read in 
  if (grepl("2020",subtype) | grepl("2030",subtype)) {
    summary_data <- "GCPT_data_Jul2020.xlsx"
    status_changes <- "GCPT Status Changes H1 2015 to H1 2020.xlsx"
    plant_data <- "July 2020 Global Coal Plant Tracker.xlsx"
  }else {
    summary_data <- "GCPT_data_Jan2021.xlsx"
    status_changes <- "GCPT Status Changes_1Feb2021.csv"
    plant_data <- "January 2021 Global Coal Plant Tracker.xlsx"
  }
  
  `Announced + Pre-permit + Permitted` <- NULL
  H2.2014 <- NULL
  H2.2015 <- NULL
  H2.2016 <- NULL
  H2.2017 <- NULL
  H2.2018 <- NULL
  H2.2019 <- NULL
  H2.2020 <- NULL
  Jan15 <- NULL
  Jan16 <- NULL
  Jan17 <- NULL
  Jan18 <- NULL
  Jan19 <- NULL
  Jan20 <- NULL
  Jul20 <- NULL
  Country <- NULL
  CountryCode <- NULL
  MW <- NULL
  `2015` <- NULL
  `2016` <- NULL
  `2017` <- NULL
  `2018` <- NULL
  `2019` <- NULL
  `2020` <- NULL
  `Plant age` <- NULL
  `Capacity (MW)` <- NULL
  Status <- NULL
  RETIRED <- NULL
  `Planned Retire` <- NULL
  Avg_Ret_Age <- NULL
  `Heat rate` <- NULL
  `Emission factor` <- NULL
  `Combustion technology` <- NULL
  Year <- NULL
  Period <- NULL
  Cap_Factor <- NULL
  green_5yr_emi <- NULL
  bau_5yr_emi <- NULL
  brown_5yr_emi <- NULL
  norm_5yr_emi <- NULL
  BAU_emi <- NULL
  All_total_emi <- NULL
  Value <- NULL
  BAU_total_emi <- NULL
  green_total_emi <- NULL
  brown_total_emi <- NULL
  norm_total_emi <- NULL
  brown_emi <- NULL
  green_emi <- NULL
  norm_emi <- NULL
  
  # Capacity by country as of January 2021, with select manual updates according to the Global Coal Finance Tracker and news media
  cap_sum <- read_excel(summary_data,sheet="Summary",range="A4:K112")
  cap_sum <- cap_sum %>% select(-`Announced + Pre-permit + Permitted`)
  cap_sum <- as.magpie(cap_sum,spatial=1)
  getRegions(cap_sum) <- toolCountry2isocode(getRegions(cap_sum))
  cap_sum <- toolCountryFill(cap_sum,fill=0,no_remove_warning = "KOS",verbosity=2)
  
  cap_sum_oper <- cap_sum[,,"Operating"]
  
  additions <- read_excel(summary_data,sheet="Additions",range = c("A4:V112"))
  # colnames(additions)[length(colnames(additions))]="2020"
  retirements <- read_excel(summary_data,sheet="Retirements",range = c("A4:V112"))
  add_startyr <- as.numeric(colnames(additions)[2])
  add_endyr <- as.numeric(colnames(additions)[length(additions)])
  
  # Read file with time series data of plant status changes
  if (grepl("20",subtype)) {
    plant_status <- read_excel("GCPT Status Changes H1 2015 to H1 2020.xlsx",sheet=2)
    plant_status <- plant_status %>% select(Country,MW,Jul20,Jan20,Jan19,Jan18,Jan17,Jan16,Jan15)
    colnames(plant_status)[3:ncol(plant_status)] <- c(2020:2014)
  }
  else {
    plant_status <- read.csv(status_changes,sep = ",")
    plant_status <- plant_status %>% select(Country,MW,H2.2020,H2.2019,H2.2018,H2.2017,H2.2016,H2.2015,H2.2014)
    colnames(plant_status)[3:ncol(plant_status)] <- as.numeric(gsub("H2.","",colnames(plant_status)[3:ncol(plant_status)]))
  }
  status_startyr <- as.numeric(colnames(plant_status)[ncol(plant_status)])
  status_endyr <- as.numeric(colnames(plant_status)[3])
  
  # Filter for all plants that were mothballed during this period
  mothballed <- plant_status %>% 
    filter(rowSums(across(everything(),~grepl("Moth",.)))>0) %>% 
    filter(rowSums(across(everything(),~grepl("Oper",.)))>0) 

  # Calculate capacity that was mothballed or restarted each year since 2014 (to be fed into back-calculation of annual capacity below)
  cap_moth <- new.magpie(getRegions(cap_sum),years=as.numeric(colnames(additions)[2:length(additions)]),names=c("Operating","pre_status"),fill=0)
  for (j in 4:ncol(mothballed)) {
    # Mothballed plants in 2020 which were operating earlier
    oper_moth <- mothballed %>% 
      filter(mothballed[,j]=="Operating" & mothballed[,3]=="Mothballed") %>%
      group_by(Country) %>%
      summarize(sum=sum(MW))
    oper_moth$Country <- toolCountry2isocode(oper_moth$Country)
    oper_moth <- suppressWarnings(
      toolCountryFill(as.magpie(oper_moth,spatial=1,temporal=as.numeric(colnames(mothballed)[j]),datacol=2),verbosity=2,fill=0,no_remove_warning = "KOS")
    )
    
    # Retired plants in 2020 which were mothballed earlier
    moth_ret <- mothballed %>%
      filter(mothballed[,j]=="Mothballed" & mothballed[,3]=="Retired") %>%
      group_by(Country) %>%
      summarize(sum=sum(MW))
    moth_ret$Country <- toolCountry2isocode(moth_ret$Country)
    moth_ret <- suppressWarnings(
      toolCountryFill(as.magpie(moth_ret,spatial=1,temporal=as.numeric(colnames(mothballed)[j]),datacol=2),verbosity=2,fill=0,no_remove_warning = "KOS")
    )
    # Operating plants in 2020 which were mothballed earlier
    moth_oper <- mothballed %>% 
      filter(mothballed[,j]=="Mothballed" & mothballed[,3]=="Operating") %>%
      group_by(Country) %>%
      summarize(sum=sum(MW))
    moth_oper$Country <- toolCountry2isocode(moth_oper$Country)
    moth_oper <- suppressWarnings(
      toolCountryFill(as.magpie(moth_oper,spatial=1,temporal=as.numeric(colnames(mothballed)[j]),datacol=2),verbosity=2,fill=0,no_remove_warning = "KOS")
    )
    
    constr_moth <- mothballed %>% 
      filter(mothballed[,3]=="Mothballed" & mothballed[,j]!="Mothballed" & mothballed[,j]!="Operating") %>%
      group_by(Country) %>%
      summarize(sum=sum(MW))
    constr_moth$Country <- toolCountry2isocode(constr_moth$Country)
    constr_moth <- suppressWarnings(
      toolCountryFill(as.magpie(constr_moth,spatial=1,temporal=as.numeric(colnames(mothballed)[j]),datacol=2),verbosity=2,fill=0,no_remove_warning = "KOS")
    )
    
    # Annual flux of mothballing/restarting
    cap_moth[,as.numeric(colnames(mothballed)[j]),"Operating"] <- oper_moth + constr_moth - moth_oper - moth_ret
  }
  
  # Capacity with mothballed status in the first and last year shall be added as operating capacity in years prior to status tracking 
  moth_moth <- mothballed %>% 
    filter(mothballed[,3]=="Mothballed" & mothballed[,j]=="Mothballed") %>%
    group_by(Country) %>%
    summarize(sum=sum(MW))
  moth_moth$Country <- toolCountry2isocode(moth_moth$Country)
  moth_moth <- suppressWarnings(
    toolCountryFill(as.magpie(moth_moth,spatial=1,temporal=as.numeric(colnames(mothballed)[j]),datacol=2),verbosity=2,fill=0,no_remove_warning = "KOS")
  )
  cap_moth[,add_startyr:(as.numeric(colnames(mothballed)[j])-1),"pre_status"] <- oper_moth + constr_moth + moth_moth
  
  # Back-calculate annual capacity by adding retired and mothballed plants and subtracting added plants each year
  cap <- new.magpie(getRegions(cap_sum),years=as.numeric(colnames(additions)[2:length(additions)]),fill = 0)
  # j <- ncol(mothballed)
  for (i in add_startyr:(add_endyr-1)) {
    cap_add <- additions %>% select(Country,as.character(i+1):as.character(add_endyr))
    cap_add <- mutate(cap_add,Operating=rowSums(cap_add[,2:ncol(cap_add)]))
    cap_add$Country <- toolCountry2isocode(cap_add$Country)
    cap_add <- as.magpie(cap_add,spatial=1,temporal=i)
    cap_add <- toolCountryFill(cap_add,verbosity=2,fill=0,no_remove_warning = "KOS")
    
    cap_ret <- retirements %>% select(Country,as.character(i+1):as.character(add_endyr))
    cap_ret <- mutate(cap_ret,Operating=rowSums(cap_ret[,2:ncol(cap_ret)]))
    cap_ret$Country <- toolCountry2isocode(cap_ret$Country)
    cap_ret <- as.magpie(cap_ret,spatial=1,temporal=i)
    cap_ret <- toolCountryFill(cap_ret,verbosity=2,fill=0,no_remove_warning = "KOS")
    
    cap[,i,] <- cap_sum[,,"Operating"] - cap_add[,,"Operating"] + cap_ret[,,"Operating"] + cap_moth[,i,"Operating"] + cap_moth[,i,"pre_status"]
  }
  cap[,2020,] <- cap_sum_oper
  
  if (subtype=="historical") {
    return(cap/1000)
  }else if (subtype=="status") {
    return(cap_sum/1000)
  }

  #Plant-level data detailing capacities, commissioning and retirement dates, location and more.
  retire <- read_excel(plant_data,sheet=2)
  retire$`Capacity (MW)` <- as.numeric(retire$`Capacity (MW)`)
  retire$Country <- toolCountry2isocode(retire$Country)
  retire <- retire[-which(retire$Country=="KOS"),]
  
  retAge <- retire %>% select(Country,`Plant age`,`Capacity (MW)`,Status) %>% 
    filter(Status=="Retired") %>% filter(!is.na(`Plant age`)) %>% filter(`Plant age` > 0) %>% filter(!is.na(`Capacity (MW)`))
  avgRetAge <- retAge %>% group_by(Country) %>% summarise(Avg_Ret_Age=weighted.mean(`Plant age`,`Capacity (MW)`))
  retCap <- retAge %>% group_by(Country) %>% summarise(Retired_Cap=sum(`Capacity (MW)`))
  avgRetAge <- avgRetAge[do.call(order,avgRetAge),]
  retCap <- retCap[do.call(order,retCap),]
  
  mavgRetAge <- as.magpie(avgRetAge,spatial=1)
  mavgRetAge <- toolCountryFill(mavgRetAge,fill=0,no_remove_warning = "KOS",verbosity=2)
  retCap <- as.magpie(retCap,spatial=1)
  retCap <- toolCountryFill(retCap,fill=0,no_remove_warning = "KOS",verbosity=2)
  regAvgRetAge <- toolAggregate(mavgRetAge,map,weight=retCap)
  regAvgRetAge[which(regAvgRetAge==0)] <- mean(regAvgRetAge)
  mavgRetAge[getRegions(mavgRetAge)[mavgRetAge == 0],,] <- as.vector(regAvgRetAge[map$RegionCode[which(map$CountryCode %in% getRegions(mavgRetAge)[mavgRetAge == 0])],,])

  if (subtype=="lifespans") {
    return(mavgRetAge)
  }else if (subtype=="meanAge") {
    meanAge <- retire %>% select(Country,`Plant age`,`Capacity (MW)`,Status) %>% 
      filter(Status=="Operating" & !is.na(`Capacity (MW)`) & !is.na(`Plant age`)) 
    meanAge_c <- meanAge %>% group_by(Country) %>% summarise(meanAge=weighted.mean(`Plant age`,`Capacity (MW)`))
    meanAge_c <- toolCountryFill(as.magpie(meanAge_c,spatial=1),fill=0)
    capWeight <- meanAge %>% group_by(Country) %>% summarise(Capacity=sum(`Capacity (MW)`))
    capWeight <- toolCountryFill(as.magpie(capWeight,spatial=1),fill=0)
    return(toolAggregate(meanAge_c,map,capWeight))
  }
  
  #Return national average lifespans to calculate early retirement adjustment factors 
  # if (subtype=="early_retire") {
  #   retired <- retire %>% select(Country,`Plant age`,`Capacity (MW)`,RETIRED)
  #   #Sum up capacity retired in each country in each year 2000-2020
  #   early_ret <- retired %>% filter(!is.na(RETIRED)) %>% group_by(Country,.add=TRUE) %>% group_by(RETIRED,.add=TRUE) %>%
  #     summarise(Retired_cap=sum(`Capacity (MW)`))
  #   early_ret <- as.magpie(early_ret,spatial=1,temporal=2)
  #   early_ret <- toolCountryFill(early_ret,fill=0,no_remove_warning = "KOS",verbosity = 0)
  #   ret_rate <- new.magpie(getRegions(cap),getYears(early_ret),names="Early_Retirement",fill=0)
  #   for (t in 2001:2020) {
  #     ret_rate[,t,] <- dimSums(early_ret[,(t-1):t,],dim=2)/cap[,t-1,]
  #   }
  #   ret_rate <- replace_non_finite(ret_rate)
  #   max_ret <- new.magpie(getRegions(ret_rate),NULL,names="Max_Early_Retirement",fill=0)
  #   for (reg in getRegions(ret_rate)) {
  #     max_ret[reg,,] <- max(ret_rate[reg,,])
  #   }
  #   return(max_ret)
  # }
  
  
  #Use the national lifespans to derive scenarios of 2025 coal capacity
  avgRetAge <- data.frame(Country=getRegions(mavgRetAge),Avg_Ret_Age=array(mavgRetAge))
  retire <- left_join(retire,avgRetAge,by="Country")
  
  #Brown COVID recovery scenario: Countries extend their average lifespans by 5 years
  retireBrown <- retire %>% select(Country,`Planned Retire`,`Plant age`,`Capacity (MW)`,Status,Avg_Ret_Age) %>% 
    filter(Status %in% c("Operating")) %>% 
    filter(`Planned Retire`<=2028 | `Plant age` >= (Avg_Ret_Age)) %>%
    filter(!is.na(`Capacity (MW)`)) %>%
    group_by(Country) %>% summarise(Retiring_Cap=sum(`Capacity (MW)`))
  
  retireBrown$Retiring_Cap[which(is.na(retireBrown$Retiring_Cap))] <- 0
  retireBrown <- as.magpie(retireBrown,spatial=1)
  retireBrown <- toolCountryFill(retireBrown,fill=0,no_remove_warning = "KOS",verbosity=2)
  
  #Green COVID recovery scenario: Countries maintain their historical average lifespans
  retireGreen <- retire %>% select(Country,`Planned Retire`,`Plant age`,`Capacity (MW)`,Status,Avg_Ret_Age) %>% 
    filter(Status %in% c("Operating")) %>% 
    filter(`Planned Retire`<=2028 | `Plant age` >= (Avg_Ret_Age-5)) %>%
    filter(!is.na(`Capacity (MW)`)) %>%
    group_by(Country) %>% summarise(Retiring_Cap=sum(`Capacity (MW)`))
  
  retireGreen$Retiring_Cap[which(is.na(retireGreen$Retiring_Cap))] <- 0
  retireGreen <- as.magpie(retireGreen,spatial=1)
  retireGreen <- toolCountryFill(retireGreen,fill=0,no_remove_warning = "KOS",verbosity=2)
  
  #Literature norm scenario: Popular choice in literature has been to use a globally uniform 40-yr lifespan
  retireNorm <- retire %>% select(Country,`Planned Retire`,`Plant age`,`Capacity (MW)`,Status,Avg_Ret_Age) %>%
    filter(Status %in% c("Operating")) %>% 
    filter(`Plant age` >= 35) %>%
    filter(!is.na(`Capacity (MW)`)) %>%
    group_by(Country) %>% summarise(Retiring_Cap=sum(`Capacity (MW)`))
  retireNorm$Retiring_Cap[which(is.na(retireNorm$Retiring_Cap))] <- 0
  retireNorm <- as.magpie(retireNorm,spatial=1)
  retireNorm <- toolCountryFill(retireNorm,fill=0,no_remove_warning = "KOS",verbosity=2)
  
  #Relevant regional mappings and country classifications
  #Current members of PPCA
  PPCAmap <- toolGetMapping("regionmappingPPCA.csv",type = "regional")
  #Current OECD members
  OECDmap <- toolGetMapping("regionmappingOECD.csv",type = "regional")
  ppca <- PPCAmap$CountryCode[which(PPCAmap$RegionCode=="PPCA")]
  oecd <- OECDmap$CountryCode[which(OECDmap$RegionCode=="OECD")]
  EU27 <- map$CountryCode[which(map$RegionCode=="EUR")]
  nonoecd <- OECDmap$CountryCode[which(OECDmap$RegionCode=="NON")]
  #The PPCA stipulates that OECD and EU members must phase out coal power by 2030
  oecdPPCA <- ppca[which(ppca %in% oecd | ppca %in% EU27)]
  #While non-OECD members must phase out coal power by 2050
  nonOECDppca <- ppca[which(!(ppca %in% oecdPPCA))]
  #There are several countries which have explicit plans to abandon coal power in their COVID recovery packages
  greenNations <- c(EU27[-which(EU27=="POL")][-which(EU27 %in% ppca)],"PHL","BGD","EGY","PAK","AUS","KOR","VNM","CHL")
  
  # Remove any plants with unknown (NA) capacity from plant status data frame
  plant_status <- plant_status %>% filter(!is.na(MW))
  tran <- data.frame(Country=unique(plant_status$Country),shelved=0,she2she=0,she2can=0,she2oper=0,she2ann=0,she2pre=0,pre2she=0,pre2con=0,
                     she2perm=0,she2con=0,newcon=0,con2she=0,con2she2oper=0,con2she2con=0,con2she2can=0,con2can=0,perm2she=0,perm2con=0,
                     con2oper=0,con2she2she=0,plan2she=0,ann=0,ann2oper=0,ann2can=0,ann2she=0,ann2con=0,pre=0,pre2oper=0,pre2can=0,perm=0,
                     perm2oper=0,perm2can=0,moth=0,moth2ret=0,moth2oper=0)
  
  for (i in 1:nrow(plant_status)) {
    j <- ncol(plant_status)
    if (plant_status[i,j]=="XXX") {
      plant_status[i,which(plant_status[i,]=="XXX")] <- "Announced"
    }
    while (j > 4) {
      if (plant_status[i,j]=="Shelved") {
        if (j==ncol(plant_status) || !("Shelved" %in% plant_status[i,j:ncol(plant_status)])) {
          tran$shelved[which(tran$Country==plant_status$Country[i])] <- tran$shelved[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          if (plant_status[i,4]=="Cancelled") {
            tran$she2can[which(tran$Country==plant_status$Country[i])]<- tran$she2can[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Shelved") {
            tran$she2she[which(tran$Country==plant_status$Country[i])]<- tran$she2she[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Operating") {
            tran$she2oper[which(tran$Country==plant_status$Country[i])]<- tran$she2oper[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Announced") {
            tran$she2ann[which(tran$Country==plant_status$Country[i])]<- tran$she2ann[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Pre-permit") {
            tran$she2pre[which(tran$Country==plant_status$Country[i])]<- tran$she2pre[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Permitted") {
            tran$she2perm[which(tran$Country==plant_status$Country[i])]<- tran$she2perm[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Construction") {
            tran$she2con[which(tran$Country==plant_status$Country[i])]<- tran$she2con[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }
        }
      }else if (plant_status[i,j]=="Construction") {
        if (j==ncol(plant_status)  || !("Construction" %in% plant_status[i,(j+1):ncol(plant_status)])) {
          tran$newcon[which(tran$Country==plant_status$Country[i])] <- tran$newcon[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          if (plant_status[i,4]=="Cancelled") {
            tran$con2can[which(tran$Country==plant_status$Country[i])] <- tran$con2can[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Shelved") {
            tran$con2she2she[which(tran$Country==plant_status$Country[i])]<- tran$con2she2she[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Operating") {
            tran$con2oper[which(tran$Country==plant_status$Country[i])]<- tran$con2oper[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Construction") {
            tran$con2con[which(tran$Country==plant_status$Country[i])]<- tran$con2con[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }
        }
      }else if (plant_status[i,j]=="Announced") {
        if (j==ncol(plant_status)  || !("Announced" %in% plant_status[i,(j+1):ncol(plant_status)])) {
          tran$ann[which(tran$Country==plant_status$Country[i])] <- tran$ann[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          if (plant_status[i,4]=="Cancelled") {
            tran$ann2can[which(tran$Country==plant_status$Country[i])] <- tran$ann2can[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Shelved") {
            tran$ann2she[which(tran$Country==plant_status$Country[i])]<- tran$ann2she[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Operating") {
            tran$ann2oper[which(tran$Country==plant_status$Country[i])]<- tran$ann2oper[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Construction") {
            tran$ann2con[which(tran$Country==plant_status$Country[i])]<- tran$ann2con[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }
        }
      }else if (plant_status[i,j]=="Pre-permit") {
        if (j==ncol(plant_status)  || !("Pre-permit" %in% plant_status[i,(j+1):ncol(plant_status)])) {
          tran$pre[which(tran$Country==plant_status$Country[i])] <- tran$pre[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          if (plant_status[i,4]=="Cancelled") {
            tran$pre2can[which(tran$Country==plant_status$Country[i])] <- tran$pre2can[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Shelved") {
            tran$pre2she[which(tran$Country==plant_status$Country[i])]<- tran$pre2she[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Operating") {
            tran$pre2oper[which(tran$Country==plant_status$Country[i])]<- tran$pre2oper[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Construction") {
            tran$pre2con[which(tran$Country==plant_status$Country[i])]<- tran$pre2con[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }
        }
      }else if (plant_status[i,j]=="Permitted") {
        if (j==ncol(plant_status)  || !("Permitted" %in% plant_status[i,(j+1):ncol(plant_status)])) {
          tran$perm[which(tran$Country==plant_status$Country[i])] <- tran$perm[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          if (plant_status[i,4]=="Cancelled") {
            tran$perm2can[which(tran$Country==plant_status$Country[i])] <- tran$perm2can[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Shelved") {
            tran$perm2she[which(tran$Country==plant_status$Country[i])]<- tran$perm2she[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Operating") {
            tran$perm2oper[which(tran$Country==plant_status$Country[i])]<- tran$perm2oper[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Construction") {
            tran$perm2con[which(tran$Country==plant_status$Country[i])]<- tran$perm2con[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }
        }
      }else if (plant_status[i,j]=="Mothballed") {
        if (j==ncol(plant_status)  || !("Mothballed" %in% plant_status[i,(j+1):ncol(plant_status)])) {
          tran$moth[which(tran$Country==plant_status$Country[i])] <- tran$moth[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          if (plant_status[i,4]=="Ret") {
            tran$moth2ret[which(tran$Country==plant_status$Country[i])] <- tran$moth2ret[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (any(plant_status[i,4:(j-1)]=="Operating")) {
            tran$moth2oper[which(tran$Country==plant_status$Country[i])] <- tran$moth2oper[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }
        }
      }
      
      j <- j-1
      if (plant_status[i,j]=="Shelved") {
        if (plant_status[i,j+1]=="Construction") {
          tran$con2she[which(tran$Country==plant_status$Country[i])]<- tran$con2she[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          if (plant_status[i,4]=="Operating") {
            tran$con2she2oper[which(tran$Country==plant_status$Country[i])] <- tran$con2she2oper[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Construction") {
            tran$con2she2con[which(tran$Country==plant_status$Country[i])] <- tran$con2she2con[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (plant_status[i,4]=="Cancelled") {
            tran$con2she2can[which(tran$Country==plant_status$Country[i])] <- tran$con2she2can[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }
        }else if (plant_status[i,j+1] %in% c("Announced","Pre-permit","Permitted")) {
          tran$plan2she[which(tran$Country==plant_status$Country[i])] <- tran$plan2she[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
        }
      }
    }
  }
  tran <- tran[-which(tran$Country=="Kosovo"),]
  
  
  mtran <- as.magpie(tran,spatial=1)
  getYears(mtran) <- "y2020"
  getRegions(mtran) <- toolCountry2isocode(getRegions(mtran))
  mtran <- toolCountryFill(mtran,fill=0,verbosity=2)
  # Combined cancellation + shelving rates of each project phase, to be used for completion rate calculation
  can_she_rate <- new.magpie(getRegions(mtran),getYears(mtran),getNames(mtran)[which(!grepl("2",getNames(mtran)) & getNames(mtran)!="moth")],fill=NA)
  # Shelving rate of projects in each phase
  she_rate <- new.magpie(getRegions(mtran),getYears(mtran),getNames(can_she_rate),fill=NA)
  # Cancellation rate of projects in the shelving phase
  can_rate <- new.magpie(getRegions(mtran),getYears(mtran),"shelved",fill=NA)
  
  reg_can_rate <- new.magpie(getRegions(regAvgRetAge),getYears(mtran),getNames(can_rate))
  reg_can_she_rate <- new.magpie(getRegions(regAvgRetAge),getYears(mtran),getNames(can_she_rate))
  reg_she_rate <- new.magpie(getRegions(regAvgRetAge),getYears(mtran),getNames(she_rate))
  for (phase in getNames(can_she_rate)) {
    nonzero_countries <- getRegions(mtran)[which(mtran[,,phase]!=0)]
    zero_countries <- getRegions(mtran)[which(mtran[,,phase]==0)]
    if (phase=="newcon") {
      can_she_rate[,,phase] <- (mtran[,,"con2can"] + mtran[,,"con2she2she"])/mtran[,,"newcon"]
      she_rate[,,phase] <- mtran[,,"con2she2she"]/mtran[,,"newcon"]
    }else if (phase=="shelved") {
      can_she_rate[,,phase] <- (mtran[,,"she2can"] + mtran[,,"she2she"])/mtran[,,phase]
      she_rate[,,phase] <- mtran[,,"she2she"]/mtran[,,"shelved"]
      can_rate[nonzero_countries,,phase] <- mtran[nonzero_countries,,"she2can"]/mtran[nonzero_countries,,"shelved"]
      #Calculate global mean rates
      glo_can_rate <- weighted.mean(can_rate[nonzero_countries,,phase],mtran[nonzero_countries,,phase])
      #Calculate regional rates
      tmp <- toolAggregate(can_rate[nonzero_countries,,phase],rel=map[which(map$CountryCode %in% nonzero_countries),],mtran[nonzero_countries,,phase])
      #For regions which had zero projects in a certain phase, assign the global weighted mean
      reg_can_rate <- mbind(tmp,new.magpie(getRegions(reg_can_rate)[which(!(getRegions(reg_can_rate)) %in% getRegions(tmp))],getYears(can_rate),phase,glo_can_rate))
      #Assign regional rates to countries without projects in the present phase
      can_rate[zero_countries,,phase] <- as.numeric(reg_can_rate[map$RegionCode[which(map$CountryCode %in% zero_countries)],,phase])
      
    }else {
      can_she_rate[,,phase] <- (mtran[,,paste0(phase,"2can")] + mtran[,,paste0(phase,"2she")])/mtran[,,phase]
      she_rate[,,phase] <- mtran[,,paste0(phase,"2she")]/mtran[,,phase]
    }
    #Calculate global mean rates
    glo_can_she_rate <- weighted.mean(can_she_rate[nonzero_countries,,phase],mtran[nonzero_countries,,phase])
    glo_she_rate <- weighted.mean(she_rate[nonzero_countries,,phase],mtran[nonzero_countries,,phase])
    
    #Calculate regional rates
    tmp_can_she <- toolAggregate(can_she_rate[nonzero_countries,,phase],rel=map[which(map$CountryCode %in% nonzero_countries),],mtran[nonzero_countries,,phase])
    tmp_she <- toolAggregate(she_rate[nonzero_countries,,phase],rel=map[which(map$CountryCode %in% nonzero_countries),],mtran[nonzero_countries,,phase])
    
    #For regions which had zero projects in a certain phase, assign the global weighted mean
    reg_can_she_rate[,,phase] <- mbind(tmp_can_she,
                                       new.magpie(getRegions(reg_can_she_rate)[which(!(getRegions(reg_can_she_rate)) %in% getRegions(tmp_can_she))],getYears(mtran),phase,glo_can_she_rate))
    reg_she_rate[,,phase] <- mbind(tmp_she,
                                   new.magpie(getRegions(reg_she_rate)[which(!(getRegions(reg_she_rate)) %in% getRegions(tmp_she))],getYears(mtran),phase,glo_she_rate))
    
    #Assign regional rates to countries without projects in the present phase
    can_she_rate[zero_countries,,phase] <- as.numeric(reg_can_she_rate[map$RegionCode[which(map$CountryCode %in% zero_countries)],,phase])
    she_rate[zero_countries,,phase] <- as.numeric(reg_she_rate[map$RegionCode[which(map$CountryCode %in% zero_countries)],,phase])
  }
  
  
  # 
  # #Regional average cancellation and shelving rates, weighted by each country's total projects from 2015-2020
  # reg_can_she_rate <- toolAggregate(can_she_rate,rel=map,weight=dimSums(mtran[,,getNames(can_she_rate)],dim=3))
  # reg_she_rate <- toolAggregate(she_rate,rel=map,weight=dimSums(mtran[,,getNames(she_rate)],dim=3))
  # reg_can_rate <- toolAggregate(can_rate,rel=map,weight=dimSums(mtran[,,getNames(can_rate)],dim=3))
  # 
  
  #Immaturity index formula to determine nascent coal consuming nations, where >50% of all coal plants are in the current pipeline 
  immaturity <- (cap_sum[,,"Announced"] + cap_sum[,,"Pre-permit"] + cap_sum[,,"Permitted"] + cap_sum[,,"Construction"] + cap_sum[,,"Shelved"]) /
    (dimSums(cap_sum,dim=3))
  
  #Nascent coal consumers AND countries without any projects in the past 6 years are assigned their regional cancellation/shelving rates
  can_rate[which(immaturity>0.5)] <- reg_can_rate[map$RegionCode[which(map$CountryCode %in% getRegions(immaturity)[which(immaturity>0.5)])],,]
  can_rate[which(can_rate==0.5)] <- reg_can_rate[map$RegionCode[which(map$CountryCode %in% getRegions(immaturity)[which(immaturity>0.5)])],,]
  
  comp_rate <- new.magpie(getRegions(can_she_rate),getYears(mtran),getNames(can_she_rate),fill=NA)
  comp_rate_brown <- new.magpie(getRegions(can_she_rate),getYears(mtran),getNames(can_she_rate),fill=NA)
  reg_comp_rate <- new.magpie(getRegions(reg_can_rate),getYears(mtran),getNames(can_she_rate),fill=NA)
  reg_comp_rate_brown <- new.magpie(getRegions(reg_can_rate),getYears(mtran),getNames(can_she_rate),fill=NA)
  glo_comp_rate <- new.magpie("GLO",getYears(mtran),getNames(can_she_rate),fill=NA)
  glo_comp_rate_brown <- new.magpie("GLO",getYears(mtran),getNames(can_she_rate),fill=NA)
  for (status in getNames(can_she_rate)) {
    nonzero_countries <- getRegions(mtran)[which(mtran[,,status]!=0)]
    zero_countries <- getRegions(mtran)[which(mtran[,,status]==0)]
    can_she_rate[,,status][which(immaturity>0.5)] <- reg_can_she_rate[map$RegionCode[which(map$CountryCode %in% getRegions(immaturity)[which(immaturity>0.5)])],,status]
    she_rate[,,status][which(immaturity>0.5)] <- reg_she_rate[map$RegionCode[which(map$CountryCode %in% getRegions(immaturity)[which(immaturity>0.5)])],,status]
    # Derive completion rates to be used in the 2025 capacity estimation
    if (status=="shelved") {
      comp_rate[,,status] <- 1 - can_rate[,,status]
      comp_rate_brown[,,status] <- 1 - 0.5*can_rate[,,status]
    }else {
      comp_rate[,,status] <- 1 - can_she_rate[,,status]
      comp_rate_brown[,,status] <- 1 - 0.5*can_she_rate[,,status]
    }
    glo_comp_rate[,,status] <- weighted.mean(comp_rate[nonzero_countries,,status],mtran[nonzero_countries,,status])
    glo_comp_rate_brown[,,status] <- weighted.mean(comp_rate_brown[nonzero_countries,,status],mtran[nonzero_countries,,status])
  
    if (length(nonzero_countries)) {
      #Calculate regional rates
      tmp <- toolAggregate(comp_rate[nonzero_countries,,status],filter(map,CountryCode %in% nonzero_countries),mtran[nonzero_countries,,status])
      tmp_brown <- toolAggregate(comp_rate_brown[nonzero_countries,,status],filter(map,CountryCode %in% nonzero_countries),mtran[nonzero_countries,,status])
      #For regions which had zero projects in a certain status, assign the global weighted mean
      reg_comp_rate[,,status] <- mbind(tmp,new.magpie(getRegions(reg_comp_rate)[which(!(getRegions(reg_comp_rate)) %in% getRegions(tmp))],getYears(comp_rate),status,glo_comp_rate))
      reg_comp_rate_brown[,,status] <- mbind(tmp_brown,new.magpie(getRegions(reg_comp_rate_brown)[which(!(getRegions(reg_comp_rate_brown)) %in% getRegions(tmp_brown))],getYears(comp_rate_brown),status,glo_comp_rate_brown))
    }else {
      reg_comp_rate[,,status] <- 0
      reg_comp_rate_brown[,,status] <- 0
    }
  }
  
  # Countries which have indicated an anti-coal recovery plan are assumed to halve their BAU completion rates in the brown and BAU recovery scenarios
  # In the green recovery scenario, their completion rates will be 0 (except for under-construction plants which will be built at BAU rates)
  comp_rate_brown[c(nonOECDppca,greenNations),,] <- 0.5 * comp_rate_brown[c(nonOECDppca,greenNations),,]
  comp_rate[c(nonOECDppca,greenNations),,] <- 0.5 * comp_rate[c(nonOECDppca,greenNations),,]
  # Under-construction plants in anti-coal nations will all be completed in both scenarios
  comp_rate[c(nonOECDppca,greenNations),,"newcon"] <- 2 * comp_rate[c(nonOECDppca,greenNations),,"newcon"]
  comp_rate_brown[c(nonOECDppca,greenNations),,"newcon"] <- 2 * comp_rate_brown[c(nonOECDppca,greenNations),,"newcon"]
  
  #OECD PPCA members cannot complete any pipeline projects to meet their 2030 phase-out target
  comp_rate[oecdPPCA,,] <- 0
  comp_rate_brown[oecdPPCA,,] <- 0
  
  #OECD PPCA members and green recovery nations are assumed to cancel all shelved projects and retire mothballed ones.
  comp_rate[c(greenNations,oecdPPCA,nonOECDppca),,"shelved"] <- 0
  comp_rate_brown[c(greenNations,oecdPPCA,nonOECDppca),,"shelved"] <- 0
  cap_sum[c(oecdPPCA,greenNations,nonOECDppca),,"Mothballed"] <- 0
  
  # Return completion rate magpies 
  if (grepl("comp_rates",subtype)) {
    getNames(comp_rate) <- c("Shelved","Construction","Announced","Pre-permit","Permitted")
    getNames(comp_rate_brown) <- c("Shelved_brown","Construction_brown","Announced_brown","Pre-permit_brown","Permitted_brown")
    getNames(glo_comp_rate) <- c("Shelved","Construction","Announced","Pre-permit","Permitted")
    getNames(glo_comp_rate_brown) <- c("Shelved_brown","Construction_brown","Announced_brown","Pre-permit_brown","Permitted_brown")
    getNames(reg_comp_rate) <- c("Shelved","Construction","Announced","Pre-permit","Permitted")
    getNames(reg_comp_rate_brown) <- c("Shelved_brown","Construction_brown","Announced_brown","Pre-permit_brown","Permitted_brown")
    reg_mtran <- toolAggregate(
      setNames(mtran[,,c("shelved","newcon","ann","pre","perm")],
               c("Shelved","Construction","Announced","Pre-permit","Permitted")),map,NULL)
    if (grepl("reg",subtype)) {
      reg_all_comp_rate <- toolAggregate(reg_comp_rate,weight=reg_mtran,dim=3,rel=data.frame(from=getNames(reg_mtran),to=rep("all",5)))
      out <- mbind(reg_comp_rate,reg_all_comp_rate)
    }else  out <- mbind(comp_rate,comp_rate_brown)
    return(out)
  }
  
  # *********************************Derive Coal capacity in 2025******************************
  
  cap2025_brown <- cap_sum[,,"Operating"] + 
    cap_sum[,,"Mothballed"]*0.5 + 
    cap_sum[,,"Announced"] * comp_rate_brown[,,"ann"] + 
    cap_sum[,,"Pre-permit"] * comp_rate_brown[,,"pre"] + 
    cap_sum[,,"Permitted"] * comp_rate_brown[,,"perm"] + 
    cap_sum[,,"Construction"] * comp_rate_brown[,,"newcon"] +
    comp_rate_brown[,,"shelved"] * (cap_sum[,,"Shelved"] + 
                                      cap_sum[,,"Announced"] * (1-she_rate[,,"ann"]) + 
                                      cap_sum[,,"Pre-permit"] * (1-she_rate[,,"pre"]) + 
                                      cap_sum[,,"Permitted"] * (1-she_rate[,,"perm"]) + 
                                      cap_sum[,,"Construction"] * (1-she_rate[,,"newcon"])) - 
    retireBrown[,,"Retiring_Cap"]
  
  cap2025_BAU <- cap_sum[,,"Operating"] + 
    cap_sum[,,"Mothballed"]*0.5 + 
    cap_sum[,,"Announced"] * comp_rate[,,"ann"] + 
    cap_sum[,,"Pre-permit"] * comp_rate[,,"pre"] + 
    cap_sum[,,"Permitted"] * comp_rate[,,"perm"] + 
    cap_sum[,,"Construction"] * comp_rate[,,"newcon"] +
    comp_rate[,,"shelved"] * (cap_sum[,,"Shelved"] + 
                                cap_sum[,,"Announced"] * (1-she_rate[,,"ann"]) + 
                                cap_sum[,,"Pre-permit"] * (1-she_rate[,,"pre"]) + 
                                cap_sum[,,"Permitted"] * (1-she_rate[,,"perm"]) + 
                                cap_sum[,,"Construction"] * (1-she_rate[,,"newcon"])) - 
    retireGreen[,,"Retiring_Cap"]
  
  cap2025_green <- cap_sum[,,"Operating"] + 
    cap_sum[,,"Announced"] * comp_rate[,,"ann"]/2 + 
    cap_sum[,,"Pre-permit"] * comp_rate[,,"pre"]/2 + 
    cap_sum[,,"Permitted"] * comp_rate[,,"perm"]/2 + 
    0.5 * comp_rate[,,"newcon"] * cap_sum[,,"Construction"] +
    0.5 * comp_rate[,,"shelved"] * cap_sum[,,"Construction"] * (1-she_rate[,,"newcon"]) -
    retireGreen[,,"Retiring_Cap"]
  
  cap2025_norm <- cap_sum[,,"Operating"] + 
    cap_sum[,,"Mothballed"]*0.5 + 
    cap_sum[,,"Announced"] + 
    cap_sum[,,"Pre-permit"] + 
    cap_sum[,,"Permitted"] + 
    cap_sum[,,"Construction"]  - 
    retireNorm[,,"Retiring_Cap"]
  
  
  cap2025_brown[which(cap2025_brown<0)] <- 0
  cap2025_green[which(cap2025_green<0)] <- 0
  cap2025_norm[which(cap2025_norm<0)] <- 0
  cap2025_BAU[which(cap2025_norm<0)] <- 0
  
  getNames(cap2025_brown) <- "Brown"
  getNames(cap2025_green) <- "Green"
  getNames(cap2025_norm) <- "Norm"
  getNames(cap2025_BAU) <- "BAU"
  
  
  getYears(cap2025_brown) <- "y2025"
  getYears(cap2025_green) <- "y2025"
  getYears(cap2025_norm) <- "y2025"
  getYears(cap2025_BAU) <- "y2025"
  
  mcap2025 <- mbind(cap2025_brown/1000,cap2025_green/1000,cap2025_BAU/1000,cap2025_norm/1000)
  
  # Return Capacity scenarios
  if (subtype=="future") {
    return(mcap2025)
  }
  
  # Prepare completion rate data frames
  getNames(comp_rate) <- c("Shelved","Construction","Announced","Pre-permit","Permitted")
  df_comp <- as.data.frame(comp_rate)
  colnames(df_comp)[2] <- "Country"
  df_comp <- df_comp[,-3]
  df_comp <- df_comp[,-1]
  colnames(df_comp)[2] <- "Status"
  
  getNames(comp_rate_brown) <- c("Shelved","Construction","Announced","Pre-permit","Permitted")
  df_comp_brown <- as.data.frame(comp_rate_brown)
  colnames(df_comp_brown)[2] <- "Country"
  df_comp_brown <- df_comp_brown[,-3]
  df_comp_brown <- df_comp_brown[,-1]
  colnames(df_comp_brown)[2] <- "Status"
  
  #Calculate committed emissions
  # Select necessary columns from plant-level database
  emi_data <- retire %>% select(Country,`Plant age`,`Capacity (MW)`,Status,`Heat rate`,`Emission factor`,`Combustion technology`,RETIRED,Year) %>% 
    filter(Status!="Retired" & Status!="Cancelled" & !is.na(`Capacity (MW)`) & is.na(RETIRED) & !is.na(`Emission factor`)) %>% 
    left_join(avgRetAge,by="Country") %>% mutate(Brown_Ret_Age=Avg_Ret_Age+5) %>% mutate(Norm_Ret_Age=40)
  
  #Convert Year column to numeric, first handling the special non-numeric cases
  emi_data$Year[which(grepl("after",emi_data$Year,ignore.case=TRUE))] <- 
    as.numeric(gsub("after ","",emi_data$Year[which(grepl("after",emi_data$Year,ignore.case=TRUE))],ignore.case=TRUE))+1
  emi_data$Year[which(grepl("/",emi_data$Year,fixed=TRUE) | grepl("-",emi_data$Year,fixed=TRUE))] <- 
    substr(emi_data$Year[which(grepl("/",emi_data$Year,fixed=TRUE) | grepl("-",emi_data$Year,fixed=TRUE))],1,4)
  emi_data$Year <- as.numeric(emi_data$Year)
  
  # Set operating plants with an unknown age to 50% of the national average lifespan
  emi_data <- emi_data %>% 
    mutate(`Plant age`=
             ifelse(is.na(Year) & Status=="Operating",
                    0.5 * Avg_Ret_Age,
                    `Plant age`))

  # emi_data[which(is.na(emi_data$`Plant age`)),] <- emi_data[which(is.na(emi_data$`Plant age`)),] %>% mutate(`Plant age`=Avg_Ret_Age/2)
  
  # Set pipeline plants to appropriate ages
  for (Phase in getNames(comp_rate)) {
    
    emi_data <- emi_data %>% 
      mutate(`Plant age` = 
               ifelse(Status==Phase & is.finite(Year) & Year >=2020,
                      2020 - Year,
                      `Plant age`))
    
    meanAge <- emi_data %>% 
      filter(Status==Phase & is.finite(Year) & Year >=2020) %>% 
      summarise(age=mean(`Plant age`))
    
    # emi_data$`Plant age`[which(emi_data$Status==Phase & is.finite(emi_data$Year) & emi_data$Year>=2020)] <- 
    #   2020 - emi_data$Year[which(emi_data$Status==Phase & is.finite(emi_data$Year) & emi_data$Year>=2020)]
    
    # meanAge <- mean(emi_data$`Plant age`[which(emi_data$Status==Phase & is.finite(emi_data$Year) & emi_data$Year>=2020)])
    
    emi_data$`Plant age`[which(emi_data$Status==Phase & is.na(emi_data$Year))] <- meanAge$age
  }
  # Read in national average capacity factor assumption for each 5-year time-step
  capFac <- calcOutput("CapacityFactor",aggregate=F)[,seq(2020,2100,5),"pc"]
  capFac <- removeColNa(as.data.frame(capFac))[,-3]
  colnames(capFac) <- c("Country","Period","Cap_Factor")
  capFac$Period <- as.numeric(as.character(capFac$Period))
  
  # Assign each coal plant its country's average capacity factor per time-step
  emi_data <- emi_data %>% left_join(capFac,by="Country") %>% mutate(`Plant age`=`Plant age`+(Period-2020))
  
  # Calculate emissions from each plant for each 5-yr timestep in each scenario using GCPT methodology
  # 5 * MW * cf * Btu/kWh * kgCO2/TJ * 9.2427e-12
  emi_data <- emi_data %>% mutate(green_5yr_emi=ifelse(emi_data$`Plant age`>=0 & (emi_data$`Plant age` < emi_data$Avg_Ret_Age),
                                                       5 * `Capacity (MW)` * Cap_Factor * `Heat rate` * `Emission factor`*9.2427e-12, 0)) %>% 
    mutate(bau_5yr_emi=ifelse(emi_data$`Plant age`>=0 & (emi_data$`Plant age` < emi_data$Avg_Ret_Age),
                              5 * `Capacity (MW)` * Cap_Factor * `Heat rate` * `Emission factor`*9.2427e-12, 0)) %>%
    mutate(brown_5yr_emi=ifelse(emi_data$`Plant age`>=0 & (emi_data$`Plant age` < emi_data$Brown_Ret_Age),
                                5 * `Capacity (MW)` * Cap_Factor * `Heat rate` * `Emission factor`*9.2427e-12, 0)) %>% 
    mutate(norm_5yr_emi=ifelse(emi_data$`Plant age`>=0 & (emi_data$`Plant age` < emi_data$Norm_Ret_Age),
                               5 * `Capacity (MW)` * Cap_Factor * `Heat rate` * `Emission factor`*9.2427e-12, 0))
  
  # Calculate total emissions (MtCO2) over the lifespan of plants in operation and development for each scenario
  green_emi <- emi_data %>% group_by(Country,.add=TRUE) %>% group_by(Status,.add=TRUE) %>% summarise(green_emi = sum(green_5yr_emi))
  
  bau_emi <- emi_data %>% group_by(Country,.add=TRUE) %>% group_by(Status,.add=TRUE) %>% summarise(BAU_emi = sum(bau_5yr_emi))
  
  brown_emi <- emi_data %>% group_by(Country,.add=TRUE) %>% group_by(Status,.add=TRUE) %>% summarise(brown_emi = sum(brown_5yr_emi))
  
  norm_emi <- emi_data %>% group_by(Country,.add=TRUE) %>% group_by(Status,.add=TRUE) %>% summarise(norm_emi = sum(norm_5yr_emi))
  
  # Calculate total emissions from all operating & pipeline plants with 40-yr lifespans
  norm_emi <- norm_emi %>% group_by(Country) %>% summarise(norm_total_emi=sum(norm_emi)/1000)
  glo_norm_emi <- norm_emi %>% summarise(norm_global_emi=sum(norm_total_emi))
  
  # Calculate committed emissions reduced by PPCA members based on norm assumptions
  ppca_emi <- emi_data %>% 
    filter(Period>=2030 & Country %in% ppca) %>% 
    mutate(norm_5yr_emi=ifelse(Period==2030,norm_5yr_emi/2,norm_5yr_emi)) %>%
    summarise(avoided_emissions=sum(norm_5yr_emi)/1000)
  
  # Calculate total emissions from all operating & pipeline plants
  all_emi <- bau_emi %>% group_by(Country) %>% summarise(All_total_emi=sum(BAU_emi)/1000)
  glo_all_emi <- all_emi %>% summarise(All_global_emi=sum(All_total_emi))
  
  # Calculate total emissions from operating & pipeline plants in each recovery scenario
  bau_emi <- left_join(bau_emi,df_comp,by=c("Country","Status"))
  bau_emi$Value[which(bau_emi$Status=="Operating")] <- 1
  bau_emi$Value[which(bau_emi$Status=="Mothballed")] <- 0.5
  bau_emi <- bau_emi %>% group_by(Country) %>% summarize(BAU_total_emi=sum(BAU_emi*Value)/1000)
  glo_bau_emi <- bau_emi %>% summarise(Global_BAU_emi=sum(BAU_total_emi))
  
  green_emi <- left_join(green_emi,df_comp,by=c("Country","Status"))
  green_emi$Value <- 0.5 * green_emi$Value 
  green_emi$Value[which(green_emi$Status=="Operating")] <- 1
  green_emi$Value[which(green_emi$Status=="Mothballed")] <- 0
  green_emi <- green_emi %>% group_by(Country) %>% summarize(green_total_emi=sum(green_emi*Value)/1000)
  glo_green_emi <- green_emi %>% summarise(Global_green_emi=sum(green_total_emi))
  
  brown_emi <- left_join(brown_emi,df_comp_brown,by=c("Country","Status"))
  brown_emi$Value[which(brown_emi$Status=="Operating")] <- 1
  brown_emi$Value[which(brown_emi$Status=="Mothballed")] <- 0.5
  brown_emi <- brown_emi %>% group_by(Country) %>% summarize(brown_total_emi=sum(brown_emi*Value)/1000)
  glo_brown_emi <- brown_emi %>% summarise(Global_brown_emi=sum(brown_total_emi))
  
  # Calculate capacity-weighted mean emission factor by country and plant status
  # emifac <- emi_data %>% group_by(Country,.add=TRUE) %>% group_by(Status,.add=TRUE) %>%
  #   summarise(EF_w_mean = weighted.mean(`Emission factor`,`Capacity (MW)`))
  
  bau_emi <- toolCountryFill(as.magpie(bau_emi),fill=0,verbosity=2)
  green_emi <- toolCountryFill(as.magpie(green_emi),fill=0,verbosity=2)
  brown_emi <- toolCountryFill(as.magpie(brown_emi),fill=0,verbosity=2)
  norm_emi <- toolCountryFill(as.magpie(norm_emi),fill=0,verbosity=2)
  
  if (subtype=="emissions") {
    return(mbind(bau_emi,green_emi,brown_emi,norm_emi))
  }else if (subtype=="ppca_emi") {
    return(as.magpie(ppca_emi))
  }
}
