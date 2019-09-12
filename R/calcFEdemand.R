#' Edge data with its original sectoral division
#'
#' Returns the Edge data at the Remind level
#'
#' @param subtype Final energy (FE) or Energy service (ES) or Useful/Final Energy items from EDGEv3 corresponding to REMIND FE items (UE_for_Eff,FE_for_Eff)
#' @importFrom data.table data.table tstrsplit setnames CJ setkey
#' @importFrom stats approx
#' @importFrom dplyr as_tibble tibble last sym between first
#' @importFrom tidyr extract complete nesting replace_na
#' @importFrom quitte seq_range interpolate_missing_periods character.data.frame
#' @author Antoine Levesque
calcFEdemand <- function(subtype = "FE") {

  #----- Functions ------------------
  getScens = function(mag) {
    getNames(mag, dim = "scenario")
  }

  expand_vectors = function(x,y) {
    if (is.data.frame(y)) {
      y = apply(y,1,paste,collapse=".")
    }

    paste(rep(x,each=length(y)),y,sep=".")
  }

  addSDP_transport <- function(rmnditem){
    ## adding dummy vars and funcs to avoid global var complaints
    scenario.item  <- year <- scenario <- item <- region <- value <- variable <- .SD  <- dem_cap  <- fact <- toadd <- Year <- gdp_cap <- ssp2dem <- window <- train_add <- NULL

    ## start of actual function
    trp_nodes <- c("ueelTt", "ueLDVt", "ueHDVt")

    ## we work in the REMIND H12 regions to avoid strange ISO country behavior when rescaling
    mappingfile <- toolMappingFile("regional","regionmappingH12.csv")
    rmnd_reg <- toolAggregate(rmnditem, mappingfile, from="CountryCode", to="RegionCode")

    ## to data.table (we use gdp_SSP2 as a starting point)
    rmndt <- as.data.table(rmnd_reg)
    rmndt[, c("scenario", "item") := tstrsplit(scenario.item, ".", fixed = TRUE)][
      , "scenario.item" := NULL][
      , year := as.numeric(gsub("y", "", year))]
    setnames(rmndt, "V3", "region")
    trpdem <- rmndt[item %in% trp_nodes & scenario == "gdp_SSP2"][, scenario := "gdp_SDP"]

    ## get population
    pop <- as.data.table(calcOutput("Population"))[
      , year := as.numeric(gsub("y", "", year))]
    setnames(pop, c("variable"), c("scenario"))

    ## intrapolate missing years
    yrs <- sort(union(pop$year, trpdem$year))
    pop <- pop[CJ(region=pop$region, year=yrs, scenario=pop$scenario, unique=T),
               on=c("region", "year", "scenario")]
    pop[, value := approx(x=.SD$year, y=.SD$value, xout=.SD$year)$y,
        by=c("region", "scenario")]

    ## merge scenario names
    pop[, scenario := gsub("pop_", "gdp_", scenario)]
    setnames(pop, "value", "pop")

    demPop <- pop[trpdem, on=c("year", "region", "scenario")]
    demPop[, dem_cap := value/pop * 1e3] # EJ/10^6=TJ (pop. in millions), scale to GJ/cap*yr

    gdp_iso <- calcOutput("GDPppp", aggregate = F)[,, "gdp_SDP"]
    gdp_iso <- time_interpolate(gdp_iso, getYears(rmnd_reg))
    gdp_reg <- toolAggregate(gdp_iso, mappingfile, from="CountryCode", to="RegionCode")
    getSets(gdp_reg) <- c("region", "Year", "scenario")
    ## load GDP
    gdp <- as.data.table(gdp_reg)[
      , year := as.numeric(gsub("y", "", Year))][
      , Year := NULL]

    setnames(gdp, "value", "gdp")

    ## merge
    demPop <- gdp[demPop, on=c("year", "region", "scenario")]
    demPop[, gdp_cap := gdp/pop]

    ## add new scenario from SSP2
    newdem <- demPop

    setkey(newdem, "year", "item")
    newdem[, ssp2dem := dem_cap]
    for(yr in seq(2025, 2100, 5)){
      it <- "ueLDVt"
      target <- 7 ## GJ
      switch_yrs <- 10
      prv_row <- newdem[year == yr - 5 & item == it]
      newdem[year == yr & item == it,
             window := ifelse(prv_row$dem_cap - target >= 0,
                              0.135 * pmin((prv_row$dem_cap - target)^2/target^2, 0.2),
                              -0.135 * pmax((target - prv_row$dem_cap)^2/target^2, 0.2))]

      newdem[year == yr & item == it,
             dem_cap := (1-window)^5 * prv_row$dem_cap * pmin((yr - 2020)/switch_yrs, 1) + ssp2dem * (1 - pmin((yr - 2020)/switch_yrs, 1))]

      it <- "ueHDVt"
      target <- 9
      prv_row <- newdem[year == yr - 5 & item == it]
      newdem[year == yr & item == it,
             window := ifelse(prv_row$dem_cap - target >= 0,
                              0.135 * pmin((prv_row$dem_cap - target)^2/target^2, 0.2),
                              -0.135 * pmax((target - prv_row$dem_cap)^2/target^2, 0.2))]
      newdem[year == yr & item == it,
             dem_cap := (1-window)^5 * prv_row$dem_cap * pmin((yr - 2020)/switch_yrs, 1) + ssp2dem * (1 - pmin((yr - 2020)/switch_yrs, 1))]

    }

    newdem[, c("window", "ssp2dem") := NULL]

    ## toplot <- rbind(demPop, newdem)

    ## ggplot(toplot[item == "ueHDVt" & region %in% c("CHN", "USA", "IND", "JPN") & scenario %in% c("gdp_SDP", "gdp_SSP2")], aes(x=year, y=dem_cap)) +
    ##   geom_line(aes(color=scenario)) +
    ##   facet_wrap(~region)

    ## add trains
    trns <- function(year){
      if(year <= 2020)
        return(0)
      else
        return((year-2020)^2 * 0.000018) # at 2100, this is ~ 11.5%
    }

    yrs <- unique(newdem$year)
    trainsdt <- data.table(year=yrs, fact=sapply(yrs, trns))

    newdem <- newdem[trainsdt, on="year"]

    ## both freight and passenger road are reduced in favour of trains
    newdem[item %in% c("ueHDVt", "ueLDVt"), toadd := dem_cap * fact]
    newdem[item %in% c("ueHDVt", "ueLDVt"), dem_cap := dem_cap - toadd]

    newdem[, train_add := 0]
    newdem[item == "ueelTt" & year > 2025, dem_cap := newdem[item == "ueelTt" & year == 2025]$dem_cap, by=year]

    ## we add it to trains
    newdem[year > 2020, train_add := sum(toadd, na.rm = T),
           by=c("year", "region")]
    ## replace old values
    newdem[item == "ueelTt", dem_cap := dem_cap + train_add][
      , c("toadd", "train_add", "fact") := NULL]

    ## ggplot(newdem[region %in% c("SSA", "USA", "IND", "JPN")], aes(x=year, y=dem_cap)) +
    ##   geom_line(aes(color=item)) +
    ##   facet_wrap(~region)

    ## multiply by population
    newdem[, value := dem_cap * pop / 1e3] # back to EJ

    ## constant for t>2100
    newdem[year > 2100, value := newdem[year == 2100]$value, by="year"]

    ## ggplot(newdem[region %in% c("CHN", "USA", "IND", "JPN"), sum(dem_cap), by=.(year, region)], aes(x=year, y=V1)) +
    ##   geom_line() +
    ##   facet_wrap(~region)
    newdem <- suppressWarnings(as.magpie(newdem[, c("region", "year", "scenario", "item", "value")]))
    dem_iso <- toolAggregate(newdem, mappingfile, gdp_iso, from="RegionCode", to="CountryCode")
    getSets(dem_iso)[1] <- "region"

    return(dem_iso)

  }

  addSDP_industry <- function(reminditems) {
    # Modify industry FE trajectories of SSP1 (and SSP2) to generate SDP
    # scenario trajectories

    # mask non-global variables so R doesn't get its panties twisted
    year <- Year <- Data1 <- Data2 <- Region <- Value <- Data3 <- scenario <-
      iso3c <- value <- variable <- pf <- FE <- VA <- GDP <- VApGDP <- FEpVA <-
      gdp_SSP1 <- gdp_SSP2 <- f <- gdp_SDP <- .FE <- f.mod <- gdp <- pop <-
      GDPpC <- NULL

    # output years
    years <- as.integer(sub('^y', '', getYears(reminditems)))

    tmp_GDPpC <- bind_rows(
      # load GDP projections
      tmp_GDP <- calcOutput('GDPppp', FiveYearSteps = FALSE,
                            aggregate = FALSE) %>%
        as.data.frame() %>%
        as_tibble() %>%
        character.data.frame() %>%
        mutate(Year = as.integer(as.character(Year))) %>%
        filter(grepl('^gdp_SSP[12]$', Data1),
               Year %in% years) %>%
        extract(Data1, c('variable', 'scenario'), '^([a-z]{3})_(.*)$') %>%
        select(scenario, iso3c = Region, year = Year, variable, value = Value),

      tmp_pop <- calcOutput('Population', FiveYearSteps = FALSE,
                            aggregate = FALSE) %>%
        as.data.frame() %>%
        as_tibble()  %>%
        character.data.frame() %>%
        mutate(Year = as.integer(as.character(Year))) %>%
        filter(grepl('^pop_SSP[12]$', Data1),
               Year %in% years) %>%
        extract(Data1, c('variable', 'scenario'), '^([a-z]{3})_(.*)$') %>%
        select(scenario, iso3c = Region, year = Year, variable, value = Value)
    ) %>%
      mutate(scenario = paste0('gdp_', scenario)) %>%
      spread(variable, value) %>%
      group_by(scenario, iso3c, year) %>%
      summarise(GDPpC = gdp / pop) %>%
      ungroup()

    # - for each country and scenario, compute a GDPpC-dependent specific energy
    #   use reduction factor according to 3e-5 * GDPpC + 0.2 [%], which is
    #   capped at 0.7 %
    #   - the mean GDPpC of countries with GDPpC > 15000 (in 2015) is about 33k
    #   - so efficiency gains range from 0.4 % at zero GDPpC (more development
    #     leeway) to 1.4 % at 33k GDPpC (more stringent energy efficiency)
    #   - percentage numbers are halved and applied twice, to VA/GDP and FE/VA
    # - linearly reduce this reduction factor from 1 to 0 over the 2020-2150
    #   interval
    # - cumulate the reduction factor over the time horizon

    reduction_factor <- tmp_GDPpC %>%
      interpolate_missing_periods(year = seq_range(range(year)),
                                  value = 'GDPpC') %>%
      group_by(scenario, iso3c) %>%
      mutate(
        f = cumprod(ifelse(2020 > year, 1,
                           1 - ( pmin(0.007, 3e-7 * GDPpC + 0.002)
                               * (1 - (year - 2020) / (2150 - 2020)))))) %>%
      ungroup() %>%
      select(-GDPpC) %>%
      filter(year %in% years)

    bind_rows(
      # select industry FE use
      reminditems %>%
        as.data.frame() %>%
        as_tibble() %>%
        mutate(Year = as.integer(as.character(Year))) %>%
        filter(grepl('^gdp_SSP[12]$', Data1),
               grepl('fe..i', Data2)) %>%
        select(scenario = Data1, iso3c = Region, year = Year, variable = Data2,
               value = Value) %>%
        character.data.frame(),

      # reuse GDP projections
      tmp_GDP %>%
        mutate(variable = 'GDP',
               scenario = paste0('gdp_', scenario)),

      # load VA projections
      readSource('EDGE_Industry', 'projections_VA_iso3c', convert = FALSE) %>%
        as.data.frame() %>%
        as_tibble() %>%
        select(scenario = Data1, iso3c = Data2, year = Year, sector = Data3,
               value = Value) %>%
        filter(grepl('^gdp_SSP[12]$', scenario),
               'Total' != iso3c,
               as.character(year) %in% years) %>%
        mutate(iso3c = as.character(iso3c),
               year = as.integer(as.character(year))) %>%
        group_by(scenario, iso3c, year) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(variable = 'VA') %>%
        interpolate_missing_periods(year = years, expand.values = TRUE) %>%
        character.data.frame()
    ) %>%
      spread(variable, value) %>%
      gather(pf, FE, matches('^fe..i$')) %>%
      inner_join(reduction_factor, c('scenario', 'iso3c', 'year')) %>%
      mutate(VApGDP = VA  / GDP,
             FEpVA  = FE  / VA) %>%
      # Modify reduction factor f based on feeli share in pf
      # f for feeli is sqrt of f; for for others choosen such that total
      # reduction equals f
      group_by(scenario, iso3c, year) %>%
      mutate(f.mod = ifelse('feeli' == pf, FE / sum(FE), 0),
             f.mod = ifelse('feeli' == pf, f.mod, 1 - sum(f.mod)),
             f.mod = ifelse('feeli' == pf, sqrt(f),
                            (f - sqrt(f) * (1 - f.mod)) / f.mod)) %>%
      ungroup() %>%
      select(-f, f = f.mod) %>%
      # gather(variable, value, GDP, FE, VA, VApGDP, FEpVA) %>%
      # SDP scenario is equal to SSP1 scenario, except for VA/GDP and FE/VA
      # indicators, which are equal to the lower value of the SSP1 or SSP2
      # scenario times the reduction factor f(t)
      group_by(iso3c, year, pf) %>%
      mutate(VApGDP = min(VApGDP) * f,
             FEpVA  = min(FEpVA)  * f) %>%
      ungroup() %>%
      select(-f) %>%
      filter('gdp_SSP2' != scenario) %>%
      mutate(scenario = 'gdp_SDP') %>%
      mutate(.FE = FEpVA * VApGDP * GDP,
             value = ifelse(!is.na(.FE), .FE, FE)) %>%
      select(scenario, iso3c, year, item = pf, value) %>%
      as.magpie() %>%
      return()
  }

  #----- READ-IN DATA ------------------
  if (subtype %in%  c("FE","EsUeFe_in","EsUeFe_out" )){

    stationary <- readSource("EDGE",subtype="FE_stationary")
    buildings  <- readSource("EDGE",subtype="FE_buildings")
    
    ## fix issue with trains in transport trajectories: they seem to be 0 for t>2100
    if(all(stationary[, 2105, "SSP2.feelt"] == 0)){
      stationary[, seq(2105, 2150, 5), "feelt"] = time_interpolate(stationary[, 2100, "feelt"], seq(2105, 2150, 5))
    }

    ## common years

    ## stationary year range is in line with requirements on the RMND side
    fill_years <- setdiff(getYears(stationary),getYears(buildings))
    buildings <- time_interpolate(buildings,interpolated_year = fill_years, integrate_interpolated_years = T, extrapolation_type = "constant")

    y = getYears(stationary)
    data = mbind(stationary[,y,],buildings[,y,])
    
    # ---- _ modify Industry FE data to carry on current trends ----
    v <- grep('^SSP[1-5]\\.fe(..i$|ind)', getNames(data), value = TRUE)
    
    dataInd <- data[,,v] %>% 
      as.quitte() %>% 
      as_tibble() %>% 
      select('scenario', 'iso3c' = 'region', 'pf' = 'item', 'year' = 'period', 
             'value') %>% 
      character.data.frame()
    
    regionmapping <- read_delim(
      file = toolMappingFile('regional', 'regionmappingH12.csv'),
      delim = ';',
      col_names = c('country', 'iso3c', 'region'),
      col_types = 'ccc',
      skip = 1)
    
    
    historic_trend <- c(2004, 2015)
    phasein_period <- c(2015, 2050)
    phasein_time   <- phasein_period[2] - phasein_period[1]
    
    dataInd <- bind_rows(
      dataInd %>% 
        filter(phasein_period[1] > !!sym('year')),
      
        inner_join(
          # calculate regional trend
          dataInd %>% 
            # get trend period
            filter(between(!!sym('year'), historic_trend[1], historic_trend[2]),
                   0 != !!sym('value')) %>% 
            # sum regional totals
            full_join(regionmapping %>% select(-!!sym('country')), 'iso3c') %>% 
            group_by(!!sym('scenario'), !!sym('region'), !!sym('pf'), 
                     !!sym('year')) %>% 
            summarise(value = sum(!!sym('value'))) %>% 
            ungroup() %>% 
            # calculate average trend over trend period
            interpolate_missing_periods(year = seq_range(historic_trend),
                                        expand.values = TRUE) %>% 
            group_by(!!sym('scenario'), !!sym('region'), !!sym('pf')) %>% 
            summarise(trend = mean(!!sym('value') / lag(!!sym('value')), 
                                   na.rm = TRUE)) %>% 
            ungroup() %>% 
            # only use negative trends (decreasing energy use)
            mutate(trend = ifelse(!!sym('trend') < 1, !!sym('trend'), NA)),
          
          # modify data projection
          dataInd %>% 
            filter(phasein_period[1] <= !!sym('year')) %>% 
            interpolate_missing_periods(
              year = phasein_period[1]:max(dataInd$year)) %>% 
            group_by(!!sym('scenario'), !!sym('iso3c'), !!sym('pf')) %>% 
            mutate(
              growth = replace_na(!!sym('value') / lag(!!sym('value')), 1)) %>% 
            full_join(regionmapping %>% select(-'country'), 'iso3c'),
          
          c('scenario', 'region', 'pf')
        ) %>% 
          group_by(!!sym('scenario'), !!sym('iso3c'), !!sym('pf')) %>% 
          mutate(
            # replace NA (positive) trends with end. growth rates -> no change
            trend = ifelse(is.na(!!sym('trend')), !!sym('growth'), 
                           !!sym('trend')),
            value_ = first(!!sym('value')) 
            * cumprod(
              ifelse(
                phasein_period[1] == !!sym('year'), 1,
                ( !!sym('trend')
                * pmax(0, phasein_period[2] - !!sym('year') + 1)
                + !!sym('growth') 
                * pmin(phasein_time, !!sym('year') - phasein_period[1] - 1)
                ) / phasein_time)),
            value = ifelse(is.na(!!sym('value_')) | 0 == !!sym('value_'), 
                           !!sym('value'), !!sym('value_'))) %>% 
          ungroup() %>% 
          select(-'region', -'value_', -'trend', -'growth') %>% 
          filter(!!sym('year') %in% unique(dataInd$year))
      ) %>%
      rename('region' = 'iso3c', 'item' = 'pf') %>% 
      as.magpie()
    
    data <- mbind(data[,,v, invert = TRUE], dataInd)
    
    unit_out = "EJ"
    description_out = "demand pathways for final energy in buildings and industry in the original file"

  } else if (subtype == "ES"){
    Unit2Million = 1e-6

    services <- readSource("EDGE",subtype="ES_buildings")
    getSets(services) <- gsub("data", "item", getSets(services))
    data <- services*Unit2Million
    unit_out = "million square meters times degree [1e6.m2.C]"
    description_out = "demand pathways for energy service in buildings"

  } else if ( subtype %in% c("FE_for_Eff", "UE_for_Eff")){

    stationary <- readSource("EDGE",subtype="FE_stationary")
    buildings  <- readSource("EDGE",subtype="FE_buildings")

    #common years

    fill_years <- setdiff(getYears(stationary),getYears(buildings))
    buildings <- time_interpolate(buildings,interpolated_year = fill_years, integrate_interpolated_years = T, extrapolation_type = "constant")
    y = intersect(getYears(stationary),getYears(buildings))
    data = mbind(stationary[,y,],buildings[,y,])

    unit_out = "EJ"
    description_out = "demand pathways for useful/final energy in buildings and industry corresponding to the final energy items in REMIND"

  }

  if (subtype %in% c( "FE","FE_for_Eff","UE_for_Eff","ES")){

    mapping_path <- toolMappingFile("sectoral","structuremappingIO_outputs.csv")
    mapping = read.csv2(mapping_path, stringsAsFactors = F)

    REMIND_dimensions = "REMINDitems_out"
    sets_names = getSets(data)

    } else if (subtype %in% c("EsUeFe_in","EsUeFe_out")){

      mapping_path <- toolMappingFile("sectoral","structuremappingIO_EsUeFe.csv")
      mapping = read.csv2(mapping_path, stringsAsFactors = F)

  }
  #----- PROCESS DATA ------------------

  regions  <- getRegions(data)
  years    <- getYears(data)
  scenarios <- getScens(data)

  if(subtype %in% c("FE_for_Eff", "UE_for_Eff")){

    #Select items from EDGE v3, which is based on the distinct UE and FE
    mapping = mapping[grepl("^.*_fe$",mapping$EDGEitems),]

    # Replace the FE input with UE inputs, but let the output names as in REMIND
    if (subtype %in% c("UE_for_Eff")){
    mapping$EDGEitems = gsub("_fe$","_ue",mapping$EDGEitems)
    }
    # Reduce data set to relevant items
    data = data[,,unique(mapping$EDGEitems)]
  }

  #Modify mapping
  if (subtype == "EsUeFe_in"){
    mapping = mapping[c("EDGEinput","REMINDitems_in","REMINDitems_out","REMINDitems_tech","weight_input")]
    REMIND_dimensions = c("REMINDitems_in","REMINDitems_out","REMINDitems_tech")
    colnames(mapping) = c("EDGEitems",REMIND_dimensions,"weight_Fedemand")

    data = data[,,unique(mapping$EDGEitems)]

    sets_names = c("region","year","scenario","item","out","tech")

  } else if (subtype == "EsUeFe_out"){
    mapping = mapping[c("EDGEoutput","REMINDitems_in","REMINDitems_out","REMINDitems_tech","weight_output")]
    REMIND_dimensions = c("REMINDitems_in","REMINDitems_out","REMINDitems_tech")
    colnames(mapping) = c("EDGEitems",REMIND_dimensions,"weight_Fedemand")

    data = data[,,unique(mapping$EDGEitems)]

    sets_names = c("region","year","scenario","in","item","tech")
  }

  edge_names = getNames(data, dim = "item")



  mapping = na.omit(mapping[c("EDGEitems",REMIND_dimensions,"weight_Fedemand")])
  mapping = mapping[which(mapping$EDGEitems %in% edge_names),]
  mapping = unique(mapping)


  magpnames = mapping[REMIND_dimensions]
  magpnames <- unique(magpnames)
  magpnames <- expand_vectors(scenarios,magpnames)

  if (length(setdiff(edge_names, mapping$EDGEitems) > 0 )) stop("Not all EDGE items are in the mapping")


  # make an empty new magpie object

  reminditems <- as.magpie(array(dim=c(length(regions), length(years), length(magpnames)),
                               dimnames=list(regions, years, magpnames)))
  getSets(reminditems) <- sets_names

  datatmp <- data
  #Take the names of reminditems without the scenario dimension already in data
  names_NoScen <- sub('^[^\\.]*\\.', '', getNames(reminditems))

  for (reminditem in names_NoScen){
    # Concatenate names from mapping columns so that they are comparable with names from magclass object
    if (length(REMIND_dimensions) > 1) {
      names_mapping = apply(mapping[REMIND_dimensions],1,paste,collapse=".")
    } else {
      names_mapping = mapping[[REMIND_dimensions]]
    }
    #Only select EDGE variables which correspond to the remind
    testdf = mapping[names_mapping == reminditem ,c("EDGEitems","weight_Fedemand")]
    prfl <- testdf[,"EDGEitems"]
    vec <- as.numeric(mapping[rownames(testdf),"weight_Fedemand"])
    names(vec) <- prfl
    datatmp[,,prfl] <- data[,,prfl] * as.magpie(vec)
    reminditems[,,reminditem]<-dimSums(datatmp[,,prfl],dim="item",na.rm = TRUE)
  }

  #Change the scenario names for consistency with REMIND sets
  getNames(reminditems) <- gsub("^SSP","gdp_SSP",getNames(reminditems))
  getNames(reminditems) <- gsub("SDP","gdp_SDP",getNames(reminditems))

  if ('FE' == subtype) {
    # add SDP transport and industry scenarios
    SDP_industry_transport <- mbind(addSDP_transport(reminditems),
                                    addSDP_industry(reminditems))

    # delete punk SDP data calculated illicitly in readEDGE('FE_stationary')
    reminditems <- mbind(
      reminditems[,,setdiff(getNames(reminditems),
                            getNames(SDP_industry_transport))],
      SDP_industry_transport)
    
    # ---- Industry subsectors data stubs ----
    industry_subsectors <- calcOutput(
      type = 'IO', subtype = 'output_Industry_subsectors', round = 8, 
      aggregate = FALSE) %>% 
      as.data.frame() %>% 
      as_tibble() %>% 
      select('period' = 'Year', 'region' = 'Region', 'pf' = 'Data2', 
             'value' = 'Value') %>% 
      # get 2005-15 industry subsector data
      character.data.frame() %>% 
      mutate(period = as.integer(!!sym('period'))) %>% 
      filter(grepl('^fe.*_(cement|chemicals|steel|otherInd)', !!sym('pf')), 
             !!sym('period') %in% as.integer(
               sub('^y', '', getYears(!!sym('reminditems'))))) %>% 
      # sum up fossil and bio SEs
      group_by(!!sym('period'), !!sym('region'), !!sym('pf')) %>% 
      summarise(value = sum(!!sym('value'))) %>% 
      ungroup() %>% 
      # rename feel steel to feel steel primary (feel steel secondary is 
      # handled within REMIND)
      mutate(period = as.integer(!!sym('period')),
             pf = ifelse('feel_steel' == !!sym('pf'), 'feel_steel_primary', 
                         !!sym('pf'))) %>% 
      # extend time horizon
      complete(
        nesting(!!sym('region'), !!sym('pf')),
        period = as.integer(sub('^y', '', 
                                unique(getYears(!!sym('reminditems')))))) %>% 
      # decrease values by 0.5 % p.a. (this is just dummy data to get the 
      # calibration rolling)
      group_by(!!sym('region'), !!sym('pf')) %>% 
      arrange(!!sym('period')) %>% 
      mutate(
        value = ifelse(!is.na(!!sym('value')), !!sym('value'),
                       ( last(na.omit(!!sym('value'))) 
                       * 0.995 ^ (!!sym('period') - 2015)
                       ))) %>% 
      ungroup() %>% 
      select('period', 'region', 'pf', 'value') %>% 
      # extend to SSP scenarios
      mutate(scenario = '') %>% 
      complete(nesting(!!sym('period'), !!sym('region'), !!sym('pf'), 
                       !!sym('value')), 
               scenario = paste0('gdp_SSP', 1:5)) %>% 
      mutate(scenario.item = paste(!!sym('scenario'), !!sym('pf'), sep = '.'),
             year = paste0('y', !!sym('period'))) %>% 
      select('region', 'year', 'scenario.item', 'value') %>% 
      as.magpie()
    
    reminditems <- mbind(reminditems, industry_subsectors)
  }

  return(list(x=reminditems,weight=NULL,
              unit = unit_out,
              description = description_out))
}
