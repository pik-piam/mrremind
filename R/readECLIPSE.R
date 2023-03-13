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
    ap  <- read_excel("ACTIVITIES_EMF30_aggregated_Ev5a_Nov2015.xlsx", sheet="Air pollutants")
    ap  <- ap[!is.na(ap[[1]]),]

    ap <- ap %>%
      pivot_longer(cols = matches('^[0-9]*$'), names_to = 'year',
                   names_transform = list(year = as.numeric)) %>%
      rename(region = 'Region', sector = 'Sector', unit = 'Unit') %>%
      mutate(region = gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", .data$region))) %>%
      filter(!.data$sector %in% c(rm_sectors, rm_unwanted),
             .data$region != "Global") %>%
      select(-'unit') %>%
      as.data.frame()

    # Remove spurious data
    ap <- ap %>%
      filter(!(.data$region == "Japan" & .data$sector == "End_Use_Residential_Coal")) %>%  # No activity data available
      filter(!(.data$region == "Turkey" & .data$sector == "End_Use_Transport_Coal"))        # No activity data available

    x <- as.magpie(ap, spatial=1, temporal=3)
  }

  if (subtype == "activities.extended") {
    ap  <- read_excel("ACTIVITIES_EMF30_extended_Ev5a_Nov2015.xlsx", sheet="Air pollutants")
    ap  <- ap[!is.na(ap[[1]]),]

    ap <- ap %>%
      pivot_longer(cols = matches('^[0-9]*$'), names_to = 'year',
                   names_transform = list(year = as.numeric)) %>%
      rename(region = 'Region', sector = 'Sector', unit = 'Unit') %>%
      mutate(region = gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", .data$region))) %>%
      filter(!.data$sector %in% c(rm_sectors, rm_unwanted),
             .data$region != "Global") %>%
      select(-'unit') %>%
      as.data.frame()

    x <- as.magpie(ap, spatial=1, temporal=3)
  }

  if (subtype == "emissions.aggregated") {
    species <- c("SO2", "NH3", "NOx", "VOC", "BC", "OC", "CO")

    cle <- do.call(
      "bind_rows",
      lapply(species,
             function(s) {
               out <-
                 read_excel("EMISSIONS_EMF30_aggregated_Ev5a_CLE_Nov2015.xlsx",
                            sheet = s)
               out  <- out[!is.na(out[[1]]), ]
               out <- out %>%
                 pivot_longer(cols = matches('^[0-9]*$'), names_to = 'year',
                              names_transform = list(year = as.numeric)) %>%
                 rename(region = 'Region', sector = 'Sector') %>%
                 mutate(region = gsub("^\\s+|\\s+$", "",
                                      gsub("[0-9]", "", .data$region))) %>%
                 mutate(variable = s) %>%
                 filter(!.data$sector %in% c(rm_sectors, rm_unwanted),
                        .data$region != "Global")

               return(out)
             })) %>%
      mutate(scenario = "CLE")

    mfr <- do.call(
      "bind_rows",
      lapply(species,
             function(s) {
               out <- read_excel("EMISSIONS_EMF30_aggregated_Ev5a_MFR_Nov2015.xlsx", sheet=s)
               out  <- out[!is.na(out[[1]]),]
               out <- out %>%
                 pivot_longer(cols = matches('^[0-9]*$'), names_to = 'year',
                              names_transform = list(year = as.numeric)) %>%
                 rename(region = 'Region', sector = 'Sector') %>%
                 mutate(region = gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", .data$region))) %>%
                 mutate(variable = s) %>%
                 filter(!.data$sector %in% c(rm_sectors, rm_unwanted),
                        .data$region != "Global")
               return(out)
             })) %>%
      mutate(scenario = "MFR")

    # MFR only has 2030 and 2050, so add CLE values for other years
    x <- bind_rows(cle,
                   bind_rows(cle %>% filter(.data$year %in% c(2000,2005,2010,2020)) %>% mutate(scenario = "MFR"), mfr)) %>%
      select('region', 'year', 'sector', 'variable', 'scenario', 'value')

    # Remove spurious data
    x <- x %>%
      filter(!(.data$region == "Japan"  & .data$sector == "End_Use_Residential_Coal")) %>%  # No activity data available
      filter(!(.data$region == "Turkey" & .data$sector == "End_Use_Transport_Coal"))        # No activity data available

    x <- as.magpie(as.data.frame(x), spatial=1, temporal=2)
  }

  if (subtype == "emissions.extended") {
    species <- c("SO2", "NH3", "NOx", "VOC", "BC", "OC", "CO")

    cle <- do.call(
      "bind_rows",
      lapply(species,
             function(s) {
               out <- read_excel("EMISSIONS_EMF30_extended_Ev5a_CLE_Nov2015.xlsx", sheet=s)
               out  <- out[!is.na(out[[1]]),]
               out <- out %>%
                 pivot_longer(cols = matches('^[0-9]*$'), names_to = 'year') %>%
                 rename(region = 'Region', sector = 'Sector') %>%
                 mutate(region = gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", .data$region))) %>%
                 mutate(variable = s) %>%
                 filter(!.data$sector %in% c(rm_sectors, rm_unwanted), .data$region != "Global")
               return(out)
             })) %>%
      mutate(scenario = "CLE")

    mfr <- do.call(
      "bind_rows",
      lapply(species,
             function(s) {
               out <- read_excel("EMISSIONS_EMF30_extended_Ev5a_MFR_Nov2015.xlsx", sheet=s)
               out  <- out[!is.na(out[[1]]),]
               out <- out %>%
                 pivot_longer(cols = matches('^[0-9]*$'), names_to = 'year') %>%
                 rename(region = 'Region', sector = 'Sector') %>%
                 mutate(region = gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", .data$region))) %>%
                 mutate(variable = s) %>%
                 filter(!.data$sector %in% c(rm_sectors, rm_unwanted), .data$region != "Global")
               return(out)
             })) %>%
      mutate(scenario = "MFR")

    # MFR only has 2030 and 2050, so add CLE values for other years
    x <- bind_rows(cle,
                   bind_rows(cle %>% filter(.data$year %in% c(2000,2005,2010,2020)) %>% mutate(scenario = "MFR"), mfr)) %>%
      select('region', 'year', 'sector', 'variable', 'scenario', 'value')

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
