#' Data from the Global Coal Plant Tracker January 2021 release by Global Energy Monitor (formerly EndCoal/CoalSwarm)
#' @description  Historical data of operating, under-construction, planned and announced Coal Plants by country (in MW)
#' from the Global Energy Monitor's Global Coal Plant Tracker, and extrapolations for 2025 capacity scenarios
#' @param subtype Options are status, historical, future, lifespans, comp_rates and emissions
#' @author Stephen Bi
#' @importFrom readxl read_excel
#' @importFrom dplyr %>% filter select mutate summarize group_by left_join across everything starts_with
#' @importFrom magclass replace_non_finite
#' @aliases readEndCoal


readGCPT <- function(subtype) {
  map <- toolGetMapping(getConfig("regionmapping"), type="regional", where = "mappingfolder")
  year <- NULL

  if (!(subtype %in% c("early_retire","historical","status","future","lifespans","emissions","comp_rates","reg_comp_rates","glo_comp_rates","meanAge","ppca_emi","G20_FinEx",
                       "PEGASOS","PEGASOS_pubfinex","PEGASOS_prefinex","REM_PEGASOS_pubfinex","REM_PEGASOS_pubfinex_REsub"))) {
    stop("Invalid subtype!")
  }

  # Data files to be read in
  status_changes_encoding <- 'UTF-8'
  if (is.null(year)) {
    summary_data <- "GCPT_data_Jan2023.xlsx"
    status_changes <-
      "Jan 2023 GCPT Status Changes - 2014 - 2022 (final).csv"
    status_changes_encoding <- 'ANSI_X3.4-1986'
    plant_data <- "Global-Coal-Plant-Tracker-January-2023.xlsx"
    lastCol <- "X"
    sep = ";"
    latest_col <- 3
  } else if (grepl('2021', year)) {
    summary_data <- "GCPT_data_Jan2021.xlsx"
    status_changes <- "GCPT Status Changes_1Feb2021.csv"
    plant_data <- "January 2021 Global Coal Plant Tracker.xlsx"
    lastCol <- "V"
    sep = ","
    latest_col <- 3
  } else if (grepl('2022', year)) {
    summary_data <- "GCPT_data_Jan2022.xlsx"
    status_changes <-
      "Jan 2022 GCPT Status Changes - 2014 - 2021 (b).csv"
    status_changes_encoding <- 'ANSI_X3.4-1986'
    plant_data <- "Global-Coal-Plant-Tracker-Jan-2022.xlsx"
    lastCol <- "W"
    sep = ";"
    latest_col <- 3
  } else if (grepl('2020', year)) {
    summary_data <- "GCPT_data_Jul2020.xlsx"
    status_changes <- "GCPT Status Changes H1 2015 to H1 2020.xlsx"
    plant_data <- "July 2020 Global Coal Plant Tracker.xlsx"
    lastCol <- "V"
    sep = ","
    plant_status <-
      read_excel("GCPT Status Changes H1 2015 to H1 2020.xlsx", sheet = 2)
    plant_status <-
      plant_status %>% select(Country, MW, Jul20, Jan20, Jan19, Jan18, Jan17, Jan16, Jan15)
    colnames(plant_status)[3:ncol(plant_status)] <- c(2020:2014)
    latest_col <- 3
  }

  plant_status <- read.csv(status_changes, sep = sep,
                           stringsAsFactors = FALSE)

  plant_status <- plant_status %>%
    select(Country, MW, starts_with("H2")) %>%
    # mutate(across(where(is.character), ~iconv(., from = status_changes_encoding, to = "UTF-8"))) %>%
    mutate(Country = ifelse(grepl("rkiye", Country), "Turkey", Country),
           Country = ifelse(grepl("Ivoire", Country), "Cote d'Ivoire", Country))

  colnames(plant_status)[3:ncol(plant_status)] <- as.numeric(gsub("H2.","",colnames(plant_status)[3:ncol(plant_status)]))

  present_year <- as.numeric(substr(summary_data,nchar(summary_data)-8,nchar(summary_data)-5))

  `Announced + Pre-permit + Permitted` <- NULL
  . <- NULL
  H2.2014 <- NULL
  H2.2015 <- NULL
  H2.2016 <- NULL
  H2.2017 <- NULL
  H2.2018 <- NULL
  H2.2019 <- NULL
  H2.2020 <- NULL
  H2.2021 <- NULL
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
  `Plant Age` <- NULL
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

  # Capacity by country as of January 2022, with select manual updates according to the Global Coal Finance Tracker and news media
  cap_sum <- read_excel(summary_data,sheet="Summary",range="A4:K112") %>%
    filter(!grepl("Total",Country) & !grepl("World",Country))

  # cap_sum <- ifelse(grepl("2020",subtype) | grepl("2021",subtype),
  #                   read_excel(summary_data,sheet="Summary",range="A4:K112"),
  #                   read_excel(summary_data,sheet="Summary"))

  cap_sum <- cap_sum %>% select(-`Announced + Pre-permit + Permitted`)
  cap_sum <- as.magpie(cap_sum,spatial=1)
  cap_sum <- toolCountryFill(setItems(dim = 1, x = cap_sum,
                                      value = toolCountry2isocode(mapping = c("DR Congo" = "COD"),
                                                                  country = getItems(dim = 1, x = cap_sum))),
                             fill=0,
                             no_remove_warning = "KOS",
                             verbosity=2)

  cap_sum_oper <- cap_sum[,,"Operating"]


  ###################################################
  ### Historical capacity additions & retirements ###
  ###################################################

  additions <- read_excel(summary_data,sheet="Additions",range = c(paste0("A4:",lastCol,"112"))) %>%
    filter(!grepl("Total",Country) & !grepl("World",Country))
  # colnames(additions)[length(colnames(additions))]="2020"
  retirements <- read_excel(summary_data,sheet="Retirements",range = c(paste0("A4:",lastCol,"112"))) %>%
    filter(!grepl("Total",Country) & !grepl("World",Country))
  add_startyr <- as.numeric(colnames(additions)[2])
  add_endyr <- as.numeric(colnames(additions)[length(additions)])


  status_startyr <- as.numeric(colnames(plant_status)[ncol(plant_status)])
  status_endyr <- as.numeric(colnames(plant_status)[3])

  # Filter for all plants that were mothballed during this period
  mothballed <- plant_status %>%
    filter(rowSums(across(everything(),~grepl("Moth",.)))>0) %>%
    filter(rowSums(across(everything(),~grepl("Oper",.)))>0)

  # Calculate capacity that was mothballed or restarted each year since 2014 (to be fed into back-calculation of annual capacity below)
  cap_moth <- new.magpie(getItems(dim = 1, x = cap_sum),years=as.numeric(colnames(additions)[2:length(additions)]),names=c("Operating","pre_status"),fill=0)
  for (j in 4:ncol(mothballed)) {
    # Mothballed plants in 2020 which were operating earlier
    oper_moth <- mothballed %>%
      filter(mothballed[,j]=="Operating" & mothballed[,3]=="Mothballed") %>%
      group_by(Country) %>%
      summarize(sum=sum(MW))
    oper_moth$Country <- toolCountry2isocode(mapping = c("DR Congo" = "COD"), country = oper_moth$Country)
    oper_moth <- suppressWarnings(
      toolCountryFill(as.magpie(oper_moth,spatial=1,temporal=as.numeric(colnames(mothballed)[j]),datacol=2),verbosity=2,fill=0,no_remove_warning = "KOS")
    )

    # Retired plants in 2020 which were mothballed earlier
    moth_ret <- mothballed %>%
      filter(mothballed[,j]=="Mothballed" & mothballed[,3]=="Retired") %>%
      group_by(Country) %>%
      summarize(sum=sum(MW))
    moth_ret$Country <- toolCountry2isocode(mapping = c("DR Congo" = "COD"), country = moth_ret$Country)
    moth_ret <- suppressWarnings(
      toolCountryFill(as.magpie(moth_ret,spatial=1,temporal=as.numeric(colnames(mothballed)[j]),datacol=2),verbosity=2,fill=0,no_remove_warning = "KOS")
    )
    # Operating plants in 2020 which were mothballed earlier
    moth_oper <- mothballed %>%
      filter(mothballed[,j]=="Mothballed" & mothballed[,3]=="Operating") %>%
      group_by(Country) %>%
      summarize(sum=sum(MW))
    moth_oper$Country <- toolCountry2isocode(mapping = c("DR Congo" = "COD"), country = moth_oper$Country)
    moth_oper <- suppressWarnings(
      toolCountryFill(as.magpie(moth_oper,spatial=1,temporal=as.numeric(colnames(mothballed)[j]),datacol=2),verbosity=2,fill=0,no_remove_warning = "KOS")
    )

    constr_moth <- mothballed %>%
      filter(mothballed[,3]=="Mothballed" & mothballed[,j]!="Mothballed" & mothballed[,j]!="Operating") %>%
      group_by(Country) %>%
      summarize(sum=sum(MW))
    constr_moth$Country <- toolCountry2isocode(mapping = c("DR Congo" = "COD"), country = constr_moth$Country)
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
  moth_moth$Country <- toolCountry2isocode(mapping = c("DR Congo" = "COD"), country = moth_moth$Country)
  moth_moth <- suppressWarnings(
    toolCountryFill(as.magpie(moth_moth,spatial=1,temporal=as.numeric(colnames(mothballed)[j]),datacol=2),verbosity=2,fill=0,no_remove_warning = "KOS")
  )
  cap_moth[,add_startyr:(as.numeric(colnames(mothballed)[j])-1),"pre_status"] <- oper_moth + constr_moth + moth_moth

  # Back-calculate annual capacity by adding retired and mothballed plants and subtracting added plants each year
  cap <- new.magpie(getItems(dim = 1, x = cap_sum),years=as.numeric(colnames(additions)[2:length(additions)]),fill = 0)
  # j <- ncol(mothballed)
  for (i in add_startyr:(add_endyr-1)) {
    cap_add <- additions %>% select(Country,as.character(i+1):as.character(add_endyr)) %>%
      mutate(Operating=rowSums(.[-1])) %>%
      mutate(Country=toolCountry2isocode(mapping = c("DR Congo" = "COD"), country = Country))
    cap_add <- as.magpie(cap_add[,c("Country","Operating")])
    cap_add <- toolCountryFill(cap_add,verbosity=2,fill=0,no_remove_warning = "KOS")
    getItems(cap_add,dim=2) <- i

    cap_ret <- retirements %>% select(Country,as.character(i+1):as.character(add_endyr)) %>%
      mutate(Operating=rowSums(.[-1])) %>%
      mutate(Country=toolCountry2isocode(mapping = c("DR Congo" = "COD"), country = Country))
    cap_ret <- as.magpie(cap_ret[,c("Country","Operating")])
    cap_ret <- toolCountryFill(cap_ret,verbosity=2,fill=0,no_remove_warning = "KOS")
    getItems(cap_ret,dim=2) <- i

    cap[,i,] <- cap_sum[,,"Operating"] - cap_add[,,"Operating"] + cap_ret[,,"Operating"] + cap_moth[,i,"Operating"] + cap_moth[,i,"pre_status"]
  }
  cap[,add_endyr,] <- cap_sum_oper

  if (grepl("historical",subtype)) {
    return(cap/1000)
  }else if (grepl("status",subtype)) {
    return(cap_sum/1000)
  }

  #Plant-level data detailing capacities, commissioning and retirement dates, location and more.
  retire <- suppressWarnings(read_excel(plant_data,sheet=2))
  retire$`Capacity (MW)` <- as.numeric(retire$`Capacity (MW)`)
  retire$`Planned Retire` <- suppressWarnings(as.numeric(retire$`Planned Retire`))
  retire$Country <- toolCountry2isocode(mapping = c("DR Congo" = "COD"), country = retire$Country)
  retire <- retire[-which(retire$Country=="KOS"),]
  if (present_year!=2020) {
    retire <- retire %>% mutate(`Plant Age` =
                                  ifelse(is.na(Year),
                                         NA,
                                         ifelse(grepl("retire",Status,ignore.case=T),
                                                ifelse(is.na(RETIRED),
                                                       NA,
                                                       RETIRED - Year),
                                                present_year - Year)))
  }

  retAge <- retire %>% select(Country,`Plant Age`,`Capacity (MW)`,Status) %>%
    filter(Status %in% c("Retired","retired")) %>% filter(!is.na(`Plant Age`)) %>% filter(`Plant Age` > 0) %>% filter(!is.na(`Capacity (MW)`))
  avgRetAge <- retAge %>% group_by(Country) %>% summarise(Avg_Ret_Age=weighted.mean(`Plant Age`,`Capacity (MW)`))
  retCap <- retAge %>% group_by(Country) %>% summarise(Retired_Cap=sum(`Capacity (MW)`))
  avgRetAge <- avgRetAge[do.call(order,avgRetAge),]
  retCap <- retCap[do.call(order,retCap),]

  mavgRetAge <- as.magpie(avgRetAge,spatial=1)
  mavgRetAge <- toolCountryFill(mavgRetAge,fill=0,no_remove_warning = "KOS",verbosity=2)
  retCap <- as.magpie(retCap,spatial=1)
  retCap <- toolCountryFill(retCap,fill=0,no_remove_warning = "KOS",verbosity=2)
  regAvgRetAge <- toolAggregate(mavgRetAge,map,weight=retCap)
  regAvgRetAge[which(regAvgRetAge==0)] <- mean(regAvgRetAge)
  mavgRetAge[getItems(dim = 1, x = mavgRetAge)[mavgRetAge == 0],,] <- as.vector(regAvgRetAge[map$RegionCode[which(map$CountryCode %in% getItems(dim = 1, x = mavgRetAge)[mavgRetAge == 0])],,])

  if (grepl("lifespans",subtype)) {
    return(mavgRetAge)
  }else if (grepl("meanAge",subtype)) {
    meanAge <- retire %>% select(Country,`Plant Age`,`Capacity (MW)`,Status) %>%
      filter(Status %in% c("Operating","operating") & !is.na(`Capacity (MW)`) & !is.na(`Plant Age`))
    meanAge_c <- meanAge %>% group_by(Country) %>% summarise(meanAge=weighted.mean(`Plant Age`,`Capacity (MW)`))
    meanAge_c <- toolCountryFill(as.magpie(meanAge_c,spatial=1),fill=0)
    capWeight <- meanAge %>% group_by(Country) %>% summarise(Capacity=sum(`Capacity (MW)`))
    capWeight <- toolCountryFill(as.magpie(capWeight,spatial=1),fill=0)
    return(toolAggregate(meanAge_c,map,capWeight))
  }

 # Return national average lifespans to calculate early retirement adjustment factors
  if (grepl("early_retire",subtype)) {
    retired <- retire %>%
      select(Country,`Plant Age`,`Capacity (MW)`,RETIRED) %>%
      left_join(avgRetAge,by="Country")
    #Sum up capacity retired in each country in each year 2000-2020
    early_ret <- retired %>%
      filter(!is.na(RETIRED) & !is.na(`Plant Age`)) %>%
      filter(`Plant Age` < Avg_Ret_Age) %>%
      group_by(Country,.add=TRUE) %>%
      group_by(RETIRED,.add=TRUE) %>%
      summarise(Retired_cap=sum(`Capacity (MW)`))
    early_ret <- as.magpie(early_ret,spatial=1,temporal=2)
    early_ret <- toolCountryFill(early_ret,fill=0,no_remove_warning = "KOS",verbosity = 0)
    ret_rate <- new.magpie(getItems(dim = 1, x = mavgRetAge),seq(2000,2015,5),names="Early_Retirement",fill=0)
    for (t in seq(2000,2015,5)) {
      ret_rate[,t,] <- dimSums(early_ret[,t:(t+5),],dim=2)/cap[,t,]
    }
    ret_rate <- replace_non_finite(ret_rate)
    max_ret <- new.magpie(getItems(dim = 1, x = ret_rate),NULL,names="Max_Early_Retirement",fill=0)
    for (reg in getItems(dim = 1, x = ret_rate)) {
      max_ret[reg,,] <- max(ret_rate[reg,,])
    }
    return(max_ret)
  }


  #Use the national lifespans to derive scenarios of 2025 coal capacity
  avgRetAge <- data.frame(Country=getItems(dim = 1, x = mavgRetAge),Avg_Ret_Age=array(mavgRetAge))
  retire <- left_join(retire,avgRetAge,by="Country")

  #Neutral COVID recovery scenario: Countries maintain their historical average lifespans
  retireBAU <- retire %>% select(Country,`Planned Retire`,`Plant Age`,`Capacity (MW)`,Status,Avg_Ret_Age) %>%
    filter(Status %in% c("Operating","operating")) %>%
    filter(`Planned Retire`<=2028 | `Plant Age` >= (Avg_Ret_Age-5)) %>%
    filter(!is.na(`Capacity (MW)`)) %>%
    group_by(Country) %>% summarise(Retiring_Cap=sum(`Capacity (MW)`))

  retireBAU$Retiring_Cap[which(is.na(retireBAU$Retiring_Cap))] <- 0
  retireBAU <- as.magpie(retireBAU,spatial=1)
  retireBAU <- toolCountryFill(retireBAU,fill=0,no_remove_warning = "KOS",verbosity=2)

  #Brown COVID recovery scenario: Countries extend their average lifespans by 5 years
  retireBrown <- retire %>% select(Country,`Planned Retire`,`Plant Age`,`Capacity (MW)`,Status,Avg_Ret_Age) %>%
    filter(Status %in% c("Operating","operating")) %>%
    filter(`Planned Retire`<=2028 | `Plant Age` >= (Avg_Ret_Age)) %>%
    filter(!is.na(`Capacity (MW)`)) %>%
    group_by(Country) %>% summarise(Retiring_Cap=sum(`Capacity (MW)`))

  retireBrown$Retiring_Cap[which(is.na(retireBrown$Retiring_Cap))] <- 0
  retireBrown <- as.magpie(retireBrown,spatial=1)
  retireBrown <- toolCountryFill(retireBrown,fill=0,no_remove_warning = "KOS",verbosity=2)

  #Green COVID recovery scenario: Countries reduce historical average lifespans by 5 years
  retireGreen <- retire %>% select(Country,`Planned Retire`,`Plant Age`,`Capacity (MW)`,Status,Avg_Ret_Age) %>%
    filter(Status %in% c("Operating","operating")) %>%
    filter(`Planned Retire`<=2028 | `Plant Age` >= (Avg_Ret_Age-10)) %>%
    filter(!is.na(`Capacity (MW)`)) %>%
    group_by(Country) %>% summarise(Retiring_Cap=sum(`Capacity (MW)`))

  retireGreen$Retiring_Cap[which(is.na(retireGreen$Retiring_Cap))] <- 0
  retireGreen <- as.magpie(retireGreen,spatial=1)
  retireGreen <- toolCountryFill(retireGreen,fill=0,no_remove_warning = "KOS",verbosity=2)

  #Literature norm scenario: Popular choice in literature has been to use a globally uniform 40-yr lifespan
  retireNorm <- retire %>% select(Country,`Planned Retire`,`Plant Age`,`Capacity (MW)`,Status,Avg_Ret_Age) %>%
    filter(Status %in% c("Operating","operating")) %>%
    filter(`Plant Age` >= 35) %>%
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
  greenNations <- c(EU27[-which(EU27=="POL")][-which(EU27 %in% ppca)],"EGY","AUS","KOR","CHL")
  if (subtype!="PEGASOS")    greenNations <- c(greenNations,"BGD","PAK","PHL","VNM")

  ###########################
  ### ANNUAL PLANT STATUS ###
  ###########################
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
    while (j > latest_col) {
      if (grepl("She",plant_status[i,j])) {
        if (j==ncol(plant_status) || !any(grepl("She",plant_status[i,j:ncol(plant_status)]))) {
          tran$shelved[which(tran$Country==plant_status$Country[i])] <- tran$shelved[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          if (grepl("Can",plant_status[i,latest_col])) {
            tran$she2can[which(tran$Country==plant_status$Country[i])]<- tran$she2can[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("She",plant_status[i,latest_col])) {
            tran$she2she[which(tran$Country==plant_status$Country[i])]<- tran$she2she[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("Oper",plant_status[i,latest_col])) {
            tran$she2oper[which(tran$Country==plant_status$Country[i])]<- tran$she2oper[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("Ann",plant_status[i,latest_col])) {
            tran$she2ann[which(tran$Country==plant_status$Country[i])]<- tran$she2ann[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("Pre",plant_status[i,latest_col])) {
            tran$she2pre[which(tran$Country==plant_status$Country[i])]<- tran$she2pre[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("Perm",plant_status[i,latest_col])) {
            tran$she2perm[which(tran$Country==plant_status$Country[i])]<- tran$she2perm[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("Con",plant_status[i,latest_col])) {
            tran$she2con[which(tran$Country==plant_status$Country[i])]<- tran$she2con[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }
        }
      }else if (grepl("Con",plant_status[i,j])) {
        if (j==ncol(plant_status)  || !any(grepl("Con",plant_status[i,(j+1):ncol(plant_status)]))) {
          tran$newcon[which(tran$Country==plant_status$Country[i])] <- tran$newcon[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          if (grepl("Can",plant_status[i,latest_col])) {
            tran$con2can[which(tran$Country==plant_status$Country[i])] <- tran$con2can[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("She",plant_status[i,latest_col])) {
            tran$con2she2she[which(tran$Country==plant_status$Country[i])]<- tran$con2she2she[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("Oper",plant_status[i,latest_col])) {
            tran$con2oper[which(tran$Country==plant_status$Country[i])]<- tran$con2oper[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("Con",plant_status[i,latest_col])) {
            tran$con2con[which(tran$Country==plant_status$Country[i])]<- tran$con2con[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }
        }
      }else if (grepl("Ann",plant_status[i,j])) {
        if (j==ncol(plant_status)  || !any(grepl("Ann",plant_status[i,(j+1):ncol(plant_status)]))) {
          tran$ann[which(tran$Country==plant_status$Country[i])] <- tran$ann[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          if (grepl("Can",plant_status[i,latest_col])) {
            tran$ann2can[which(tran$Country==plant_status$Country[i])] <- tran$ann2can[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("She",plant_status[i,latest_col])) {
            tran$ann2she[which(tran$Country==plant_status$Country[i])]<- tran$ann2she[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("Oper",plant_status[i,latest_col])) {
            tran$ann2oper[which(tran$Country==plant_status$Country[i])]<- tran$ann2oper[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("Con",plant_status[i,latest_col])) {
            tran$ann2con[which(tran$Country==plant_status$Country[i])]<- tran$ann2con[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }
        }
      }else if (grepl("Pre",plant_status[i,j])) {
        if (j==ncol(plant_status)  || !any(grepl("Pre",plant_status[i,(j+1):ncol(plant_status)]))) {
          tran$pre[which(tran$Country==plant_status$Country[i])] <- tran$pre[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          if (grepl("Can",plant_status[i,latest_col])) {
            tran$pre2can[which(tran$Country==plant_status$Country[i])] <- tran$pre2can[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("She",plant_status[i,latest_col])) {
            tran$pre2she[which(tran$Country==plant_status$Country[i])]<- tran$pre2she[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("Oper",plant_status[i,latest_col])) {
            tran$pre2oper[which(tran$Country==plant_status$Country[i])]<- tran$pre2oper[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("Con",plant_status[i,latest_col])) {
            tran$pre2con[which(tran$Country==plant_status$Country[i])]<- tran$pre2con[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }
        }
      }else if (grepl("Perm",plant_status[i,j])) {
        if (j==ncol(plant_status)  || !any(grepl("Perm",plant_status[i,(j+1):ncol(plant_status)]))) {
          tran$perm[which(tran$Country==plant_status$Country[i])] <- tran$perm[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          if (grepl("Can",plant_status[i,latest_col])) {
            tran$perm2can[which(tran$Country==plant_status$Country[i])] <- tran$perm2can[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("She",plant_status[i,latest_col])) {
            tran$perm2she[which(tran$Country==plant_status$Country[i])]<- tran$perm2she[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("Oper",plant_status[i,latest_col])) {
            tran$perm2oper[which(tran$Country==plant_status$Country[i])]<- tran$perm2oper[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("Con",plant_status[i,latest_col])) {
            tran$perm2con[which(tran$Country==plant_status$Country[i])]<- tran$perm2con[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }
        }
      }else if (grepl("Moth",plant_status[i,j])) {
        if (j==ncol(plant_status)  || !any(grepl("Moth",plant_status[i,(j+1):ncol(plant_status)]))) {
          tran$moth[which(tran$Country==plant_status$Country[i])] <- tran$moth[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          if (grepl("Ret",plant_status[i,latest_col])) {
            tran$moth2ret[which(tran$Country==plant_status$Country[i])] <- tran$moth2ret[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (any(grepl("Oper",plant_status[i,latest_col:(j-1)]))) {
            tran$moth2oper[which(tran$Country==plant_status$Country[i])] <- tran$moth2oper[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }
        }
      }

      j <- j-1
      if (grepl("She",plant_status[i,j])) {
        if (plant_status[i,j+1]=="Con") {
          tran$con2she[which(tran$Country==plant_status$Country[i])]<- tran$con2she[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          if (grepl("Oper",plant_status[i,latest_col])) {
            tran$con2she2oper[which(tran$Country==plant_status$Country[i])] <- tran$con2she2oper[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("Con",plant_status[i,latest_col])) {
            tran$con2she2con[which(tran$Country==plant_status$Country[i])] <- tran$con2she2con[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }else if (grepl("Can",plant_status[i,latest_col])) {
            tran$con2she2can[which(tran$Country==plant_status$Country[i])] <- tran$con2she2can[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
          }
        }else if (grepl("Ann",plant_status[i,j+1]) | grepl("Pre",plant_status[i,j+1]) | grepl("Perm",plant_status[i,j+1])) {
          tran$plan2she[which(tran$Country==plant_status$Country[i])] <- tran$plan2she[which(tran$Country==plant_status$Country[i])] + plant_status$MW[i]
        }
      }
    }
  }
  tran <- tran[-which(tran$Country=="Kosovo"),]


  mtran <- as.magpie(tran,spatial=1)
  getYears(mtran) <- "y2020"
  mtran <- setItems(dim = 1, x = mtran,
                    value = toolCountry2isocode(mapping = c("DR Congo" = "COD"), country = getItems(dim = 1, x = mtran)))
  mtran <- toolCountryFill(mtran,fill=0,verbosity=2)
  # Combined cancellation + shelving rates of each project phase, to be used for completion rate calculation
  can_she_rate <- new.magpie(getItems(dim = 1, x = mtran),getYears(mtran),getNames(mtran)[which(!grepl("2",getNames(mtran)) & getNames(mtran)!="moth")],fill=NA)
  # Shelving rate of projects in each phase
  she_rate <- new.magpie(getItems(dim = 1, x = mtran),getYears(mtran),getNames(can_she_rate),fill=NA)
  # Cancellation rate of projects in the shelving phase
  can_rate <- new.magpie(getItems(dim = 1, x = mtran),getYears(mtran),"shelved",fill=NA)

  reg_can_rate <- new.magpie(getItems(dim = 1, x = regAvgRetAge),getYears(mtran),getNames(can_rate))
  reg_can_she_rate <- new.magpie(getItems(dim = 1, x = regAvgRetAge),getYears(mtran),getNames(can_she_rate))
  reg_she_rate <- new.magpie(getItems(dim = 1, x = regAvgRetAge),getYears(mtran),getNames(she_rate))
  for (phase in getNames(can_she_rate)) {
    nonzero_countries <- getItems(dim = 1, x = mtran)[which(mtran[,,phase]!=0)]
    zero_countries <- getItems(dim = 1, x = mtran)[which(mtran[,,phase]==0)]
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
      reg_can_rate <- suppressWarnings(mbind(tmp,new.magpie(getItems(dim = 1, x = reg_can_rate)[which(!(getItems(dim = 1, x = reg_can_rate)) %in% getItems(dim = 1, x = tmp))],getYears(can_rate),phase,glo_can_rate)))
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
    reg_can_she_rate[,,phase] <- suppressWarnings(mbind(tmp_can_she,
                                       new.magpie(getItems(dim = 1, x = reg_can_she_rate)[which(!(getItems(dim = 1, x = reg_can_she_rate)) %in% getItems(dim = 1, x = tmp_can_she))],getYears(mtran),phase,glo_can_she_rate)))
    reg_she_rate[,,phase] <- suppressWarnings(mbind(tmp_she,
                                   new.magpie(getItems(dim = 1, x = reg_she_rate)[which(!(getItems(dim = 1, x = reg_she_rate)) %in% getItems(dim = 1, x = tmp_she))],getYears(mtran),phase,glo_she_rate)))

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
  can_rate[which(immaturity>0.5)] <- reg_can_rate[map$RegionCode[which(map$CountryCode %in% getItems(dim = 1, x = immaturity)[which(immaturity>0.5)])],,]
  can_rate[which(can_rate==0.5)] <- reg_can_rate[map$RegionCode[which(map$CountryCode %in% getItems(dim = 1, x = immaturity)[which(immaturity>0.5)])],,]

  comp_rate <- new.magpie(getItems(dim = 1, x = can_she_rate),getYears(mtran),getNames(can_she_rate),fill=NA)
  comp_rate_brown <- new.magpie(getItems(dim = 1, x = can_she_rate),getYears(mtran),getNames(can_she_rate),fill=NA)
  reg_comp_rate <- new.magpie(getItems(dim = 1, x = reg_can_rate),getYears(mtran),getNames(can_she_rate),fill=NA)
  reg_comp_rate_brown <- new.magpie(getItems(dim = 1, x = reg_can_rate),getYears(mtran),getNames(can_she_rate),fill=NA)
  glo_comp_rate <- new.magpie("GLO",getYears(mtran),getNames(can_she_rate),fill=NA)
  glo_comp_rate_brown <- new.magpie("GLO",getYears(mtran),getNames(can_she_rate),fill=NA)
  for (status in getNames(can_she_rate)) {
    nonzero_countries <- getItems(dim = 1, x = mtran)[which(mtran[,,status]!=0)]
    zero_countries <- getItems(dim = 1, x = mtran)[which(mtran[,,status]==0)]
    can_she_rate[,,status][which(immaturity>0.5)] <- reg_can_she_rate[map$RegionCode[which(map$CountryCode %in% getItems(dim = 1, x = immaturity)[which(immaturity>0.5)])],,status]
    she_rate[,,status][which(immaturity>0.5)] <- reg_she_rate[map$RegionCode[which(map$CountryCode %in% getItems(dim = 1, x = immaturity)[which(immaturity>0.5)])],,status]
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
      reg_comp_rate[,,status] <- suppressWarnings(mbind(tmp,new.magpie(getItems(dim = 1, x = reg_comp_rate)[which(!(getItems(dim = 1, x = reg_comp_rate)) %in% getItems(dim = 1, x = tmp))],getYears(comp_rate),status,glo_comp_rate)))
      reg_comp_rate_brown[,,status] <- suppressWarnings(mbind(tmp_brown,new.magpie(getItems(dim = 1, x = reg_comp_rate_brown)[which(!(getItems(dim = 1, x = reg_comp_rate_brown)) %in% getItems(dim = 1, x = tmp_brown))],getYears(comp_rate_brown),status,glo_comp_rate_brown)))
    }else {
      reg_comp_rate[,,status] <- 0
      reg_comp_rate_brown[,,status] <- 0
    }
  }

  # Countries which have indicated an anti-coal recovery plan are assumed to halve their BAU completion rates in the brown and BAU recovery scenarios
  # In the green recovery scenario, their completion rates will be 0 (except for under-construction plants which will be built at BAU rates)
  # comp_rate_brown[c(nonOECDppca,greenNations),,] <- 0.5 * comp_rate_brown[c(nonOECDppca,greenNations),,]
  # comp_rate[c(nonOECDppca,greenNations),,] <- 0.5 * comp_rate[c(nonOECDppca,greenNations),,]
  # # Under-construction plants in anti-coal nations will all be completed in both scenarios
  # comp_rate[c(nonOECDppca,greenNations),,"newcon"] <- 2 * comp_rate[c(nonOECDppca,greenNations),,"newcon"]
  # comp_rate_brown[c(nonOECDppca,greenNations),,"newcon"] <- 2 * comp_rate_brown[c(nonOECDppca,greenNations),,"newcon"]
  #
  # #OECD PPCA members cannot complete any pipeline projects to meet their 2030 phase-out target
  # comp_rate[oecdPPCA,,] <- 0
  # comp_rate_brown[oecdPPCA,,] <- 0

  #OECD PPCA members and green recovery nations are assumed to cancel all shelved projects and retire mothballed ones.
  comp_rate[c(greenNations,oecdPPCA,nonOECDppca),,"shelved"] <- 0
  comp_rate_brown[c(greenNations,oecdPPCA,nonOECDppca),,"shelved"] <- 0
  cap_sum[c(oecdPPCA,greenNations,nonOECDppca),,"Mothballed"] <- 0
  # browser()
  # Return completion rate magpies
  if (grepl("comp_rates",subtype)) {
    getNames(comp_rate) <- c("Shelved","Construction","Announced","Pre-permit","Permitted")
    getNames(comp_rate_brown) <- c("Shelved_brown","Construction_brown","Announced_brown","Pre-permit_brown","Permitted_brown")
    getNames(glo_comp_rate) <- c("Shelved","Construction","Announced","Pre-permit","Permitted")
    getNames(glo_comp_rate_brown) <- c("Shelved_brown","Construction_brown","Announced_brown","Pre-permit_brown","Permitted_brown")
    getNames(reg_comp_rate) <- c("Shelved","Construction","Announced","Pre-permit","Permitted")
    getNames(reg_comp_rate_brown) <- c("Shelved_brown","Construction_brown","Announced_brown","Pre-permit_brown","Permitted_brown")
    getNames(she_rate) <- c("Shelved_shelving","Construction_shelving","Announced_shelving","Pre-permit_shelving","Permitted_shelving")
    getNames(reg_she_rate) <- c("Shelved_shelving","Construction_shelving","Announced_shelving","Pre-permit_shelving","Permitted_shelving")
    # getNames(glo_she_rate) <- c("Shelved_shelving","Construction_shelving","Announced_shelving","Pre-permit_shelving","Permitted_shelving")

    if (grepl("reg",subtype)) {
      reg_mtran <- toolAggregate(
        setNames(mtran[,,c("shelved","newcon","ann","pre","perm")],
                 c("Shelved","Construction","Announced","Pre-permit","Permitted")),map,NULL)
      reg_all_comp_rate <- toolAggregate(reg_comp_rate,weight=reg_mtran,dim=3,rel=data.frame(from=getNames(reg_mtran),to=rep("all",5)))

      out <- suppressWarnings(mbind(reg_comp_rate,reg_all_comp_rate,reg_she_rate))
    }else if (grepl("glo",subtype)) {
      out <- suppressWarnings(mbind(glo_comp_rate,glo_comp_rate_brown))
      # out <- suppressWarnings(mbind(glo_comp_rate,glo_comp_rate_brown,glo_she_rate)
    }else  out <- suppressWarnings(mbind(comp_rate,comp_rate_brown,she_rate))
    return(out)
  }

  # *********************************Derive Coal capacity in 2025******************************
  # browser
  cap2025_brown <- cap_sum[,,"Operating"] +
    cap_sum[,,"Mothballed"]*0.5 +
    cap_sum[,,"Announced"] * comp_rate_brown[,,"ann"] +
    cap_sum[,,"Pre-permit"] * comp_rate_brown[,,"pre"] +
    cap_sum[,,"Permitted"] * comp_rate_brown[,,"perm"] +
    cap_sum[,,"Construction"] * comp_rate_brown[,,"newcon"] +
    comp_rate_brown[,,"shelved"] * (cap_sum[,,"Shelved"] +
                                      cap_sum[,,"Announced"] * (she_rate[,,"ann"]) +
                                      cap_sum[,,"Pre-permit"] * (she_rate[,,"pre"]) +
                                      cap_sum[,,"Permitted"] * (she_rate[,,"perm"]) +
                                      cap_sum[,,"Construction"] * (she_rate[,,"newcon"])) -
    retireBrown[,,"Retiring_Cap"]

  cap2025_BAU <- cap_sum[,,"Operating"] +
    cap_sum[,,"Mothballed"]*0.5 +
    cap_sum[,,"Announced"] * comp_rate[,,"ann"] +
    cap_sum[,,"Pre-permit"] * comp_rate[,,"pre"] +
    cap_sum[,,"Permitted"] * comp_rate[,,"perm"] +
    cap_sum[,,"Construction"] * comp_rate[,,"newcon"] +
    comp_rate[,,"shelved"] * (cap_sum[,,"Shelved"] +
                                cap_sum[,,"Announced"] * (she_rate[,,"ann"]) +
                                cap_sum[,,"Pre-permit"] * (she_rate[,,"pre"]) +
                                cap_sum[,,"Permitted"] * (she_rate[,,"perm"]) +
                                cap_sum[,,"Construction"] * (she_rate[,,"newcon"])) -
    retireBAU[,,"Retiring_Cap"]

  cap2025_green <- cap_sum[,,"Operating"] +
    cap_sum[,,"Announced"] * comp_rate[,,"ann"]/2 +
    cap_sum[,,"Pre-permit"] * comp_rate[,,"pre"]/2 +
    cap_sum[,,"Permitted"] * comp_rate[,,"perm"]/2 +
    0.5 * comp_rate[,,"newcon"] * cap_sum[,,"Construction"] +
    0.5 * comp_rate[,,"shelved"] * cap_sum[,,"Construction"] * (she_rate[,,"newcon"]) -
    retireBAU[,,"Retiring_Cap"]

  cap2025_norm <- cap_sum[,,"Operating"] +
    cap_sum[,,"Mothballed"]*0.5 +
    cap_sum[,,"Announced"] +
    cap_sum[,,"Pre-permit"] +
    cap_sum[,,"Permitted"] +
    cap_sum[,,"Construction"] +
    cap_sum[,,"Shelved"] -
    retireNorm[,,"Retiring_Cap"]

  cap2025_brown[which(cap2025_brown<0)] <- 0
  cap2025_green[which(cap2025_green<0)] <- 0
  cap2025_norm[which(cap2025_norm<0)] <- 0
  cap2025_BAU[which(cap2025_BAU<0)] <- 0

  getNames(cap2025_brown) <- "Brown"
  getNames(cap2025_green) <- "Green"
  getNames(cap2025_norm) <- "Norm"
  getNames(cap2025_BAU) <- "Neutral"


  getYears(cap2025_brown) <- "y2025"
  getYears(cap2025_green) <- "y2025"
  getYears(cap2025_norm) <- "y2025"
  getYears(cap2025_BAU) <- "y2025"

  mcap2025 <- suppressWarnings(mbind(cap2025_brown/1000,cap2025_green/1000,cap2025_BAU/1000,cap2025_norm/1000))

  # Return Capacity scenarios
  if (grepl("future",subtype) | grepl("FinEx",subtype)) {
    return(mcap2025)
  }
  ###### PEGASOS Policy Brief Calcs ##########
  if (grepl("PEGASOS",subtype)) {
    #PEGASOS policy brief scenario: Popular choice in literature has been to use a globally uniform 35-yr lifespan
    retirePEGA <- retire %>% select(Country,`Planned Retire`,`Plant Age`,`Capacity (MW)`,Status,Avg_Ret_Age,`Heat rate`,`Emission factor`) %>%
      filter(Status %in% c("Operating","operating")) %>%
      filter(`Plant Age` >= 25) %>%
      filter(!is.na(`Capacity (MW)`)) %>%
      group_by(Country) %>% summarise(Retiring_Cap=sum(`Capacity (MW)`))
    retirePEGA$Retiring_Cap[which(is.na(retirePEGA$Retiring_Cap))] <- 0
    retirePEGA <- as.magpie(retirePEGA,spatial=1)
    retirePEGA <- toolCountryFill(retirePEGA,fill=0,no_remove_warning = "KOS",verbosity=2)

    retireNorm <- retire %>% select(Country,`Planned Retire`,`Plant Age`,`Capacity (MW)`,Status,Avg_Ret_Age,`Heat rate`,`Emission factor`) %>%
      filter(Status %in% c("Operating","operating")) %>%
      filter(`Plant Age` >= 30) %>%
      filter(!is.na(`Capacity (MW)`)) %>%
      group_by(Country) %>% summarise(Retiring_Cap=sum(`Capacity (MW)`))
    retireNorm$Retiring_Cap[which(is.na(retireNorm$Retiring_Cap))] <- 0
    retireNorm <- as.magpie(retireNorm,spatial=1)
    retireNorm <- toolCountryFill(retireNorm,fill=0,no_remove_warning = "KOS",verbosity=2)

    retire1p5C <- retire %>% select(Country,`Planned Retire`,`Plant Age`,`Capacity (MW)`,Status,Avg_Ret_Age) %>%
      filter(Status %in% c("Operating","operating")) %>%
      filter(`Plant Age` >= 10) %>%
      filter(!is.na(`Capacity (MW)`)) %>%
      group_by(Country) %>% summarise(Retiring_Cap=sum(`Capacity (MW)`))
    retire1p5C$Retiring_Cap[which(is.na(retire1p5C$Retiring_Cap))] <- 0
    retire1p5C <- as.magpie(retire1p5C,spatial=1)
    retire1p5C <- toolCountryFill(retire1p5C,fill=0,no_remove_warning = "KOS",verbosity=2)

    getYears(retireBAU) <- "y2025"
    getYears(retireGreen) <- "y2025"
    getYears(retireBrown) <- "y2025"
    getYears(retirePEGA) <- "y2025"
    getYears(retire1p5C) <- "y2025"
    getYears(retireNorm) <- "y2025"

    for (ts in seq(2030,2070,5)) {
      #BAU COVID recovery scenario: Countries maintain their historical average lifespans
      retireBAUts <- retire %>% select(Country,`Planned Retire`,`Plant Age`,`Capacity (MW)`,Status,Avg_Ret_Age) %>%
        filter(Status %in% c("Operating","operating")) %>%
        filter(`Planned Retire`<=ts | `Plant Age` >= (Avg_Ret_Age-(ts-2020))) %>%
        filter(!is.na(`Capacity (MW)`)) %>%
        group_by(Country) %>% summarise(Retiring_Cap=sum(`Capacity (MW)`))

      retireBAUts$Retiring_Cap[which(is.na(retireBAUts$Retiring_Cap))] <- 0
      retireBAUts <- as.magpie(retireBAUts,spatial=1)
      retireBAUts <- toolCountryFill(setYears(retireBAUts,ts),fill=0,no_remove_warning = "KOS",verbosity=2)

      #Green COVID recovery scenario: Countries maintain their historical average lifespans
      retireGreents <- retire %>% select(Country,`Planned Retire`,`Plant Age`,`Capacity (MW)`,Status,Avg_Ret_Age) %>%
        filter(Status %in% c("Operating","operating")) %>%
        filter(`Planned Retire`<=ts | `Plant Age` >= (Avg_Ret_Age-(ts-2015))) %>%
        filter(!is.na(`Capacity (MW)`)) %>%
        group_by(Country) %>% summarise(Retiring_Cap=sum(`Capacity (MW)`))

      retireGreents$Retiring_Cap[which(is.na(retireGreents$Retiring_Cap))] <- 0
      retireGreents <- as.magpie(retireGreents,spatial=1)
      retireGreents <- toolCountryFill(setYears(retireGreents,ts),fill=0,no_remove_warning = "KOS",verbosity=2)

      #Brown COVID recovery scenario: Countries maintain their historical average lifespans
      retireBrownts <- retire %>% select(Country,`Planned Retire`,`Plant Age`,`Capacity (MW)`,Status,Avg_Ret_Age) %>%
        filter(Status %in% c("Operating","operating")) %>%
        filter(`Planned Retire`<=ts | `Plant Age` >= (Avg_Ret_Age-(ts-2025))) %>%
        filter(!is.na(`Capacity (MW)`)) %>%
        group_by(Country) %>% summarise(Retiring_Cap=sum(`Capacity (MW)`))

      retireBrownts$Retiring_Cap[which(is.na(retireBrownts$Retiring_Cap))] <- 0
      retireBrownts <- as.magpie(retireBrownts,spatial=1)
      retireBrownts <- toolCountryFill(setYears(retireBrownts,ts),fill=0,no_remove_warning = "KOS",verbosity=2)

      #PEGASOS policy brief scenario: Popular choice in literature has been to use a globally uniform 35-yr lifespan
      retirePEGAts <- retire %>% select(Country,`Planned Retire`,`Plant Age`,`Capacity (MW)`,Status,Avg_Ret_Age) %>%
        filter(Status %in% c("Operating","operating")) %>%
        filter(`Plant Age` + (ts-2020) >= 35) %>%
        filter(!is.na(`Capacity (MW)`)) %>%
        group_by(Country) %>% summarise(Retiring_Cap=sum(`Capacity (MW)`))
      retirePEGAts$Retiring_Cap[which(is.na(retirePEGAts$Retiring_Cap))] <- 0
      retirePEGAts <- as.magpie(retirePEGAts,spatial=1)
      retirePEGAts <- toolCountryFill(setYears(retirePEGAts,ts),fill=0,no_remove_warning = "KOS",verbosity=2)

      #Another common literature figure has been to use a globally uniform 40-yr lifespan
      retireNormts <- retire %>% select(Country,`Planned Retire`,`Plant Age`,`Capacity (MW)`,Status,Avg_Ret_Age) %>%
        filter(Status %in% c("Operating","operating")) %>%
        filter(`Plant Age` + (ts-2020) >= 40) %>%
        filter(!is.na(`Capacity (MW)`)) %>%
        group_by(Country) %>% summarise(Retiring_Cap=sum(`Capacity (MW)`))
      retireNormts$Retiring_Cap[which(is.na(retireNormts$Retiring_Cap))] <- 0
      retireNormts <- as.magpie(retireNormts,spatial=1)
      retireNormts <- toolCountryFill(setYears(retireNormts,ts),fill=0,no_remove_warning = "KOS",verbosity=2)

      #20 year lifespans are roughly compatible with Hi-1p5C scenarios
      retire1p5Cts <- retire %>% select(Country,`Planned Retire`,`Plant Age`,`Capacity (MW)`,Status,Avg_Ret_Age) %>%
        filter(Status %in% c("Operating","operating")) %>%
        filter(`Plant Age` + (ts-2020) >= 20) %>%
        filter(!is.na(`Capacity (MW)`)) %>%
        group_by(Country) %>% summarise(Retiring_Cap=sum(`Capacity (MW)`))
      retire1p5Cts$Retiring_Cap[which(is.na(retire1p5Cts$Retiring_Cap))] <- 0
      retire1p5Cts <- as.magpie(retire1p5Cts,spatial=1)
      retire1p5Cts <- toolCountryFill(setYears(retire1p5Cts,ts),fill=0,no_remove_warning = "KOS",verbosity=2)


      retireBAU <- mbind(retireBAU,retireBAUts)
      retireGreen <- mbind(retireGreen,retireGreents)
      retireBrown <- mbind(retireBrown,retireBrownts)
      retirePEGA <- mbind(retirePEGA,retirePEGAts)
      retire1p5C <- mbind(retire1p5C,retire1p5Cts)
      retireNorm <- mbind(retireNorm,retireNormts)

    }

    if (grepl("pubfinex",subtype)) {
      pubfinex <- read.csv("C:/Users/stephenb/Downloads/madrat_main/sources/GCPT/Public_Foreign_Finance_Cap_2021.csv",sep = ";")
      if (grepl("REsub",subtype)) {
        output <- new.magpie(pubfinex$Country,c(2025,2030),NULL,fill = 0)

        output[,2025,] <- as.numeric(pubfinex$Foreign) * (1-as.numeric(pubfinex$Ann_share))
        output[,2030,] <- as.numeric(pubfinex$Foreign) * as.numeric(pubfinex$Ann_share)
        return(toolCountryFill(output,fill = 0))
      }
      for (iii in c("Ann","Pre","Perm")) {
        cap_sum[pubfinex$Country,,grepl(iii,getNames(cap_sum),ignore.case = F)] <- pubfinex[,paste0(iii,"_dom")]
      }
    }

    ############ CALCULATE FUTURE CAPACITY TRAJECTORIES #################
    brown_PEGA <- cap_sum[,,"Operating"] +
      cap_sum[,,"Mothballed"]*0.5 +
      cap_sum[,,"Announced"] * comp_rate[,,"ann"] +
      cap_sum[,,"Pre-permit"] * comp_rate[,,"pre"] +
      cap_sum[,,"Permitted"] * comp_rate[,,"perm"] +
      cap_sum[,,"Construction"] * comp_rate[,,"newcon"] +
      comp_rate[,,"shelved"] * (cap_sum[,,"Shelved"] +
                                  cap_sum[,,"Announced"] * (she_rate[,,"ann"]) +
                                  cap_sum[,,"Pre-permit"] * (she_rate[,,"pre"]) +
                                  cap_sum[,,"Permitted"] * (she_rate[,,"perm"]) +
                                  cap_sum[,,"Construction"] * (she_rate[,,"newcon"])) -
      retireBAU[,,"Retiring_Cap"]

    cap_green <- cap_sum[,,"Operating"] +
      cap_sum[,,"Announced"] * comp_rate[,,"ann"]/2 +
      cap_sum[,,"Pre-permit"] * comp_rate[,,"pre"]/2 +
      cap_sum[,,"Permitted"] * comp_rate[,,"perm"]/2 +
      0.5 * comp_rate[,,"newcon"] * cap_sum[,,"Construction"] +
      0.5 * comp_rate[,,"shelved"] * cap_sum[,,"Construction"] * (she_rate[,,"newcon"]) -
      retireGreen[,,"Retiring_Cap"]

    upper_gray_PEGA <- cap_sum[,,"Operating"] +
      cap_sum[,,"Mothballed"]*0.5 +
      cap_sum[,,"Announced"] * comp_rate_brown[,,"ann"] +
      cap_sum[,,"Pre-permit"] * comp_rate_brown[,,"pre"] +
      cap_sum[,,"Permitted"] * comp_rate_brown[,,"perm"] +
      cap_sum[,,"Construction"] * comp_rate_brown[,,"newcon"] +
      comp_rate_brown[,,"shelved"] * (cap_sum[,,"Shelved"] +
                                        cap_sum[,,"Announced"] * (she_rate[,,"ann"]) +
                                        cap_sum[,,"Pre-permit"] * (she_rate[,,"pre"]) +
                                        cap_sum[,,"Permitted"] * (she_rate[,,"perm"]) +
                                        cap_sum[,,"Construction"] * (she_rate[,,"newcon"])) -
      retireBrown[,,"Retiring_Cap"]

    if(grepl("PEGASOS",subtype)) {
      red_PEGA <- cap_sum[,,"Operating"] +
        cap_sum[,,"Mothballed"]*0.5 +
        cap_sum[,,"Announced"] +
        cap_sum[,,"Pre-permit"] +
        cap_sum[,,"Permitted"] +
        cap_sum[,,"Construction"] +
        cap_sum[,,"Shelved"] -
        retirePEGA[,,"Retiring_Cap"]
    }else {
      red_PEGA <- cap_sum[,,"Operating"] +
        cap_sum[,,"Mothballed"]*0.5 +
        cap_sum[,,"Announced"] +
        cap_sum[,,"Pre-permit"] +
        cap_sum[,,"Permitted"] +
        cap_sum[,,"Construction"] +
        cap_sum[,,"Shelved"] -
        retireNorm[,,"Retiring_Cap"]
    }

    silver_PEGA <- setYears(cap_sum[,,"Operating"],NULL) - retirePEGA
    lower_gray_PEGA <- setYears(cap_sum[,,"Operating"],NULL) - retire1p5C

    green_PEGA <- setYears(cap_sum[,,"Operating"],NULL) - retireGreen

    gold_PEGA <- setYears(cap_sum[,,"Operating"],NULL) - retireGreen

    bronze_PEGA <- setYears(cap_sum[,,"Operating"],NULL) - retireBrown

    silver_PEGA[which(silver_PEGA<0)] <- 0
    cap_green[which(cap_green<0)] <- 0
    gold_PEGA[which(gold_PEGA<0)] <- 0
    brown_PEGA[which(brown_PEGA<0)] <- 0

    getYears(silver_PEGA) <- seq(2025,2070,5)
    getYears(cap_green) <- seq(2025,2070,5)
    getYears(gold_PEGA) <- seq(2025,2070,5)
    getYears(brown_PEGA) <- seq(2025,2070,5)
    getYears(bronze_PEGA) <- seq(2025,2070,5)
    getYears(upper_gray_PEGA) <- seq(2025,2070,5)
    getYears(red_PEGA) <- seq(2025,2070,5)
    getYears(green_PEGA) <- seq(2025,2070,5)
    getYears(lower_gray_PEGA) <- seq(2025,2070,5)

    ### Time Series: Announced and pre-permit projects finish in 2030 ##
    if (!grepl("2025",subtype)) {
      brown_PEGA[,2025,] <- brown_PEGA[,2025,] - (cap_sum[,,"Announced"] * comp_rate[,,"ann"] +
                                                    cap_sum[,,"Pre-permit"] * comp_rate[,,"pre"])

      cap_green[,2025,] <- cap_green[,2025,] - (cap_sum[,,"Announced"] * comp_rate[,,"ann"] +
                                                  cap_sum[,,"Pre-permit"] * comp_rate[,,"pre"])

      upper_gray_PEGA[,2025,] <- upper_gray_PEGA[,2025,] - (cap_sum[,,"Announced"] * comp_rate[,,"ann"] +
                                                              cap_sum[,,"Pre-permit"] * comp_rate[,,"pre"])

      red_PEGA[,2025,] <- red_PEGA[,2025,] - (cap_sum[,,"Announced"] * comp_rate[,,"ann"] +
                                                cap_sum[,,"Pre-permit"] * comp_rate[,,"pre"])
    }

    # getNames(silver_PEGA) <- "BAU_can"
    getNames(cap_green) <- "Green"
    # getNames(gold_PEGA) <- "Green_can"
    getNames(brown_PEGA) <- ifelse(grepl("REM",subtype), "Neutral", "Brown")
    # getNames(bronze_PEGA) <- "Brown_can"
    getNames(upper_gray_PEGA) <- ifelse(grepl("REM",subtype), "Brown", "upGray")
    getNames(red_PEGA) <- "Red"
    getNames(green_PEGA) <- "Green"
    getNames(lower_gray_PEGA) <- "lowGray"

    if(grepl("REM",subtype)) {
      mcap <- mbind(brown_PEGA,upper_gray_PEGA,cap_green)/1000
      mcap <- mcap[,getYears(mcap)<="y2030",]
    }else {
      mcap <- mbind(brown_PEGA,upper_gray_PEGA,red_PEGA,lower_gray_PEGA,cap_green)/1000
    }

    # Return Capacity scenarios
    return(mcap)
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
  emi_data <- retire %>% select(Country,`Plant Age`,`Capacity (MW)`,Status,`Heat rate`,`Emission factor`,`Combustion technology`,RETIRED,Year) %>%
    filter(!(Status %in% c("Retired","Cancelled","retired","cancelled")) & !is.na(`Capacity (MW)`) & is.na(RETIRED) & !is.na(`Emission factor`)) %>%
    left_join(avgRetAge,by="Country") %>% mutate(Brown_Ret_Age=Avg_Ret_Age+5) %>% mutate(Norm_Ret_Age=40)

  #Convert Year column to numeric, first handling the special non-numeric cases
  emi_data$Year[which(grepl("after",emi_data$Year,ignore.case=TRUE))] <-
    as.numeric(gsub("after ","",emi_data$Year[which(grepl("after",emi_data$Year,ignore.case=TRUE))],ignore.case=TRUE))+1
  emi_data$Year[which(grepl("/",emi_data$Year,fixed=TRUE) | grepl("-",emi_data$Year,fixed=TRUE))] <-
    substr(emi_data$Year[which(grepl("/",emi_data$Year,fixed=TRUE) | grepl("-",emi_data$Year,fixed=TRUE))],1,4)
  emi_data$Year <- as.numeric(emi_data$Year)

  # Set operating plants with an unknown age to 50% of the national average lifespan
  emi_data <- emi_data %>%
    mutate(`Plant Age`=
             ifelse(is.na(Year) & Status %in% c("Operating","operating"),
                    0.5 * Avg_Ret_Age,
                    `Plant Age`))

  # emi_data[which(is.na(emi_data$`Plant Age`)),] <- emi_data[which(is.na(emi_data$`Plant Age`)),] %>% mutate(`Plant Age`=Avg_Ret_Age/2)

  # Set pipeline plants to appropriate ages
  for (Phase in getNames(comp_rate)) {

    emi_data <- emi_data %>%
      mutate(`Plant Age` =
               ifelse(grepl(Phase,Status,ignore.case=F) & is.finite(Year) & Year >=2020,
                      2020 - Year,
                      `Plant Age`))

    meanAge <- emi_data %>%
      filter(grepl(Phase,Status,ignore.case=F) & is.finite(Year) & Year >=2020) %>%
      summarise(age=mean(`Plant Age`))

    # emi_data$`Plant Age`[which(emi_data$Status==Phase & is.finite(emi_data$Year) & emi_data$Year>=2020)] <-
    #   2020 - emi_data$Year[which(emi_data$Status==Phase & is.finite(emi_data$Year) & emi_data$Year>=2020)]

    # meanAge <- mean(emi_data$`Plant Age`[which(emi_data$Status==Phase & is.finite(emi_data$Year) & emi_data$Year>=2020)])

    emi_data$`Plant Age`[which(emi_data$Status==Phase & is.na(emi_data$Year))] <- meanAge$age
  }
  # Read in national average capacity factor assumption for each 5-year time-step
  capFac <- calcOutput("CapacityFactor",aggregate=F)[,seq(2020,2100,5),"pc"]
  capFac <- removeColNa(as.data.frame(capFac))[,-3]
  colnames(capFac) <- c("Country","Period","Cap_Factor")
  capFac$Period <- as.numeric(as.character(capFac$Period))

  # Assign each coal plant its country's average capacity factor per time-step
  emi_data <- emi_data %>% left_join(capFac,by="Country") %>% mutate(`Plant Age`=`Plant Age`+(Period-2020))

  # Calculate emissions from each plant for each 5-yr timestep in each scenario using GCPT methodology
  # 5 * MW * cf * Btu/kWh * kgCO2/TJ * 9.2427e-12
  emi_data <- emi_data %>% mutate(green_5yr_emi=ifelse(emi_data$`Plant Age`>=0 & (emi_data$`Plant Age` < emi_data$Avg_Ret_Age),
                                                       5 * `Capacity (MW)` * Cap_Factor * `Heat rate` * `Emission factor`*9.2427e-12, 0)) %>%
    mutate(bau_5yr_emi=ifelse(emi_data$`Plant Age`>=0 & (emi_data$`Plant Age` < emi_data$Avg_Ret_Age),
                              5 * `Capacity (MW)` * Cap_Factor * `Heat rate` * `Emission factor`*9.2427e-12, 0)) %>%
    mutate(brown_5yr_emi=ifelse(emi_data$`Plant Age`>=0 & (emi_data$`Plant Age` < emi_data$Brown_Ret_Age),
                                5 * `Capacity (MW)` * Cap_Factor * `Heat rate` * `Emission factor`*9.2427e-12, 0)) %>%
    mutate(norm_5yr_emi=ifelse(emi_data$`Plant Age`>=0 & (emi_data$`Plant Age` < emi_data$Norm_Ret_Age),
                               5 * `Capacity (MW)` * 0.53 * `Heat rate` * `Emission factor`*9.2427e-12, 0))

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

  if (grepl("emissions",subtype)) {
    return(mbind(bau_emi,green_emi,brown_emi,norm_emi))
  }else if (grepl("ppca_emi",subtype)) {
    return(as.magpie(ppca_emi))
  }
}
