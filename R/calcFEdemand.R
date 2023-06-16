#' Edge data with its original sectoral division
#'
#' Returns the Edge data at the Remind level
#'
#' @param subtype Final energy (FE) or Energy service (ES) or Useful/Final
#'   Energy items from EDGEv3 corresponding to REMIND FE items (UE_for_Eff,
#'   FE_for_Eff)
#' @param use_ODYM_RECC per-capita pathways for `SDP_xx` scenarios?  (Defaults
#'   to `FALSE`.)
#'
#' @importFrom assertr assert not_na verify
#' @importFrom data.table :=
#' @importFrom dplyr anti_join arrange as_tibble between bind_rows case_when
#'   distinct filter first full_join group_by inner_join lag last left_join
#'   matches mutate n rename right_join select semi_join summarise ungroup
#' @importFrom magclass mselect getItems getItems<-
#' @importFrom magrittr %>% %<>%
#' @importFrom quitte as.quitte cartesian character.data.frame
#'   interpolate_missing_periods interpolate_missing_periods_ madrat_mule
#'   magclass_to_tibble overwrite seq_range
#' @importFrom readr read_delim
#' @importFrom rlang .data sym syms !!! !!
#' @importFrom tibble tribble
#' @importFrom tidyr complete crossing extract gather nesting pivot_longer
#'   pivot_wider replace_na separate spread unite
#' @importFrom zoo na.fill
#'
#' @author Antoine Levesque
calcFEdemand <- function(subtype = "FE", use_ODYM_RECC = FALSE) {

    #----- Functions ------------------
    getScens = function(mag) {
      getNames(mag, dim = "scenario")
    }

    addDim <- function(x, addnm, dim, dimCode = 3.2) {
      do.call("mbind", lapply(addnm, function(item) {
        add_dimension(x, dim = dimCode, add = dim, nm = item)
      }))
    }

    fillNa <- function(x) {
      itemsFull <- apply(
        do.call("expand.grid", lapply(tail(getSets(x), -2), function(dim) {
          getItems(x, dim)
        })),
        1, "paste", collapse = "."
      )
      xFull <- new.magpie(
        cells_and_regions = getRegions(x),
        years = getYears(x),
        names = itemsFull,
        sets = getSets(x)
      )
      xFull[getRegions(x), getYears(x), getItems(x, 3)] <- x
      return(xFull)
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
      mappingfile <- toolGetMapping(type = "regional", name = "regionmappingH12.csv", returnPathOnly = TRUE, where = "mappingfolder")
      rmnd_reg <- toolAggregate(rmnditem, mappingfile, from="CountryCode", to="RegionCode")

      ## to data.table (we use gdp_SSP2 as a starting point)
      rmndt <- data.table::as.data.table(rmnd_reg)
      rmndt[, c("scenario", "item") := data.table::tstrsplit(scenario.item, ".", fixed = TRUE)][
        , "scenario.item" := NULL][
        , year := as.numeric(gsub("y", "", year))]
      data.table::setnames(rmndt, "V3", "region", skip_absent = TRUE)
      trpdem <- rmndt[item %in% trp_nodes & scenario == "gdp_SSP2"][, scenario := "gdp_SDP"]

      ## get population
      pop <- data.table::as.data.table(calcOutput("Population"))[
        , year := as.numeric(gsub("y", "", year))]
      data.table::setnames(pop, c("variable", 'iso3c'), c("scenario", 'region'),
                           skip_absent = TRUE)

      ## intrapolate missing years
      yrs <- sort(union(pop$year, trpdem$year))
      pop <- pop[data.table::CJ(region=pop$region, year=yrs, scenario=pop$scenario, unique=T),
                 on=c("region", "year", "scenario")]
      pop[, value := stats::approx(x=.SD$year, y=.SD$value, xout=.SD$year)$y,
          by=c("region", "scenario")]

      ## merge scenario names
      pop[, scenario := gsub("pop_", "gdp_", scenario)]
      data.table::setnames(pop, "value", "pop")

      demPop <- pop[trpdem, on=c("year", "region", "scenario")]
      demPop[, dem_cap := value/pop * 1e3] # EJ/10^6=TJ (pop. in millions), scale to GJ/cap*yr

      gdp_iso <- calcOutput("GDP", aggregate = F)[,, "gdp_SDP"]
      gdp_iso <- time_interpolate(gdp_iso, getYears(rmnd_reg))
      gdp_reg <- toolAggregate(gdp_iso, mappingfile, from="CountryCode", to="RegionCode")
      getSets(gdp_reg) <- c("region", "Year", "scenario")
      ## load GDP
      gdp <- data.table::as.data.table(gdp_reg)[
        , year := as.numeric(gsub("y", "", Year))][
        , Year := NULL]

      data.table::setnames(gdp, "value", "gdp")

      ## merge
      demPop <- gdp[demPop, on=c("year", "region", "scenario")]
      demPop[, gdp_cap := gdp/pop]

      ## add new scenario from SSP2
      newdem <- demPop

      data.table::setkey(newdem, "year", "item")
      newdem[, ssp2dem := dem_cap]
      for(yr in seq(2025, 2100, 5)){
        it <- "ueLDVt"
        target <- 7 ## GJ
        switch_yrs <- 10
        drive <- 0.12
        prv_row <- newdem[year == yr - 5 & item == it]
        newdem[year == yr & item == it,
               window := ifelse(prv_row$dem_cap - target >= 0,
                                drive * pmin((prv_row$dem_cap - target)^2/target^2, 0.2),
                                -drive * (target - prv_row$dem_cap)/target)]

        newdem[year == yr & item == it,
               dem_cap := (1-window)^5 * prv_row$dem_cap * pmin((yr - 2020)/switch_yrs, 1) + ssp2dem * (1 - pmin((yr - 2020)/switch_yrs, 1))]

        it <- "ueHDVt"
        target <- 9
        prv_row <- newdem[year == yr - 5 & item == it]
        newdem[year == yr & item == it,
               window := ifelse(prv_row$dem_cap - target >= 0,
                                drive * pmin((prv_row$dem_cap - target)^2/target^2, 0.2),
                                -drive * (target - prv_row$dem_cap)/target)]
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
      trainsdt <- data.table::data.table(year=yrs, fact=sapply(yrs, trns))

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
        GDPpC <- item <- NULL

      # output years
      years <- as.integer(sub('^y', '', getYears(reminditems)))

      tmp_GDPpC <- bind_rows(
        # load GDP projections
        tmp_GDP <- calcOutput('GDP', average2020 = FALSE, aggregate = FALSE) %>%
          as.data.frame() %>%
          as_tibble() %>%
          character.data.frame() %>%
          mutate(Year = as.integer(as.character(Year))) %>%
          filter(grepl('^gdp_SSP[12]$', Data1),
                 Year %in% years) %>%
          separate(col = 'Data1', into = c('variable', 'scenario'), sep = '_') %>%
          select('scenario', iso3c = 'Region', year = 'Year', 'variable',
                 value = 'Value'),

        tmp_pop <- calcOutput('Population', aggregate = FALSE) %>%
          as.data.frame() %>%
          as_tibble()  %>%
          character.data.frame() %>%
          mutate(Year = as.integer(as.character(Year))) %>%
          filter(grepl('^pop_SSP[12]$', Data1),
                 Year %in% years) %>%
          separate(col = 'Data1', into = c('variable', 'scenario'), sep = '_') %>%
          select('scenario', iso3c = 'Region', year = 'Year', 'variable',
                 value = 'Value')
      ) %>%
        mutate(scenario = paste0('gdp_', scenario)) %>%
        pivot_wider(names_from = 'variable') %>%
        group_by(.data$scenario, .data$iso3c, .data$year) %>%
        summarise(GDPpC = .data$gdp / .data$pop, .groups = 'drop')

      # - for each country and scenario, compute a GDPpC-dependent specific energy
      #   use reduction factor according to 3e-7 * GDPpC + 0.2 [%], which is
      #   capped at 0.7 %
      #   - the mean GDPpC of countries with GDPpC > 15000 (in 2015) is about 33k
      #   - so efficiency gains range from 0.4 % at zero GDPpC (more development
      #     leeway) to 1.4 % at 33k GDPpC (more stringent energy efficiency)
      #   - percentage numbers are halved and applied twice, to VA/GDP and FE/VA
      # - linearly reduce this reduction factor from 1 to 0 over the 2020-2150
      #   interval
      # - cumulate the reduction factor over the time horizon

      SSA_countries <- read_delim(
        file = toolGetMapping(type = 'regional', name = 'regionmappingH12.csv', returnPathOnly = TRUE, where = "mappingfolder"),
        delim = ';',
        col_names = c('country', 'iso3c', 'region'),
        col_types = 'ccc',
        skip = 1
      ) %>%
        filter('SSA' == !!sym('region')) %>%
        select(-'country', -'region') %>%
        getElement('iso3c')

      sgma <- 8e3
      cutoff <- 1.018
      epsilon <- 0.018
      exp1 <- 3
      exp2 <- 1.5

      reduction_factor <- tmp_GDPpC %>%
        interpolate_missing_periods(year = seq_range(range(year)), value = 'GDPpC') %>%
        group_by(scenario, iso3c) %>%
        mutate(
          # no reduction for SSA countries before 2050, to allow for more
          # equitable industry and infrastructure development
          f = cumprod(ifelse(2020 > year, 1, pmin(cutoff, 1 + 4*epsilon*((sgma/GDPpC)^exp1 - (sgma/GDPpC)^exp2))
                             ))) %>%
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
        calcOutput(
          type = 'Industry_Value_Added',
          subtype = 'economic',
          match.steel.historic.values = TRUE,
          match.steel.estimates = 'IEA_ETP',
          China_Production = readSource(type = 'ExpertGuess',
                                        subtype = 'Chinese_Steel_Production',
                                        convert = FALSE) %>%
            madrat_mule(),
          aggregate = FALSE, years = years, supplementary = FALSE) %>%
          magclass_to_tibble() %>%
          filter(grepl('^gdp_SSP(1|2EU)$', .data$scenario)) %>%
          group_by(scenario, iso3c, year) %>%
          summarise(value = sum(value), .groups = 'drop') %>%
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
        mutate(f.mod = ifelse('feeli' == pf & f < 1, sqrt(f), f)) %>%
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
        # TODO: differentiate these scenarios if there is a applicable storyline
        # for them
        complete(nesting(iso3c, year, item, value),
                 scenario = paste0('gdp_SDP', c('', '_EI', '_MC', '_RC'))) %>%
        select(scenario, iso3c, year, item, value) %>%
        as.magpie() %>%
        return()
    }

    #----- READ-IN DATA ------------------
    if (subtype %in% c("FE", "EsUeFe_in", "EsUeFe_out", "FE_buildings",
                       "UE_buildings", "FE_for_Eff", "UE_for_Eff")) {

      stationary <- readSource("EDGE",subtype="FE_stationary")
      buildings  <- readSource("EDGE",subtype="FE_buildings")

      # filter RCP scenario
      if (subtype %in% c("FE_buildings", "UE_buildings")) {
        rcps <- paste0("rcp", gsub("p", "", getItems(buildings, "rcp")))
        rcps <- gsub("rcpfixed", "none", rcps)
        getItems(buildings, "rcp") <- rcps
        stationary <- addDim(stationary, rcps, "rcp")
        # add NAs for Navigate scenarios and RCP variants
        stationary <- fillNa(stationary)
        buildings  <- fillNa(buildings)
      } else {
        rcps <- NULL
        buildings <- mselect(buildings, rcp = "fixed", collapseNames = TRUE)
      }

      ## fix issue with trains in transport trajectories: they seem to be 0 for t>2100
      if (subtype %in% c("FE", "EsUeFe_in", "EsUeFe_out", "FE_buildings", "UE_buildings") &
          all(mselect(stationary, year = "y2105", scenario = "SSP2", item = "feelt") == 0)) {
        stationary[, seq(2105, 2150, 5), "feelt"] = time_interpolate(stationary[, 2100, "feelt"], seq(2105, 2150, 5))
      }

      ## common years

      ## stationary year range is in line with requirements on the RMND side
      fill_years <- setdiff(getYears(stationary),getYears(buildings))
      buildings <- time_interpolate(buildings,interpolated_year = fill_years, integrate_interpolated_years = T, extrapolation_type = "constant")

      y <- if (subtype %in% c("FE", "EsUeFe_in", "EsUeFe_out", "FE_buildings", "UE_Buildings")) {
        getYears(stationary)  # >= 1993
      } else {
        intersect(getYears(stationary), getYears(buildings))  # >= 2000
      }

      data <- mbind(stationary[, y, ], buildings[, y, ])

      unit_out = "EJ"
      description_out <- ifelse(subtype %in% c("FE_for_Eff", "UE_for_Eff"),
        "demand pathways for useful/final energy in buildings and industry corresponding to the final energy items in REMIND",
        "demand pathways for final energy in buildings and industry in the original file")
    }

    if (subtype %in% c("FE", "EsUeFe_in", "EsUeFe_out")) {
      # ---- _ modify Industry FE data to carry on current trends ----
      v <- grep('\\.fe(..i$|ind)', getNames(data), value = TRUE)

      dataInd <- data[,,v] %>%
        as.quitte() %>%
        as_tibble() %>%
        select('scenario', 'iso3c' = 'region', 'pf' = 'item', 'year' = 'period',
               'value') %>%
        character.data.frame()

      regionmapping <- toolGetMapping(type = 'regional',
                                      name = 'regionmappingH12.csv', where = "mappingfolder") %>%
        select(country = 'X', iso3c = 'CountryCode', region = 'RegionCode')

      historic_trend <- c(2004, 2020)
      phasein_period <- c(2020, 2050)   # FIXME: extend to 2055 to keep 35 yrs?
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
              summarise(value = sum(!!sym('value')), .groups = 'drop') %>%
            # calculate average trend over trend period
            interpolate_missing_periods(year = seq_range(historic_trend), expand.values = TRUE) %>%
            group_by(!!sym('scenario'), !!sym('region'), !!sym('pf')) %>%
            summarise(trend = mean(!!sym('value') / lag(!!sym('value')),
                                     na.rm = TRUE),
                        .groups = 'drop') %>%
            # only use negative trends (decreasing energy use)
            mutate(trend = ifelse(!!sym('trend') < 1, !!sym('trend'), NA)),
          # modify data projection
          dataInd %>%
            filter(phasein_period[1] <= !!sym('year')) %>%
            interpolate_missing_periods(year = phasein_period[1]:max(dataInd$year)) %>%
            group_by(!!sym('scenario'), !!sym('iso3c'), !!sym('pf')) %>%
            mutate(growth = replace_na(!!sym('value') / lag(!!sym('value')), 1)) %>%
            full_join(regionmapping %>% select(-'country'), 'iso3c'),
          c('scenario', 'region', 'pf')) %>%
          group_by(!!sym('scenario'), !!sym('iso3c'), !!sym('pf')) %>%
          mutate(
          # replace NA (positive) trends with end. growth rates -> no change
          trend = ifelse(is.na(!!sym('trend')), !!sym('growth'), !!sym('trend')),
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

      # ---- _ modify SSP1 Industry FE demand ----
      # compute a reduction factor of 1 before 2021, 0.84 in 2050, and increasing
      # to 0.78 in 2150
      f <- as.integer(sub('^y', '', y)) - 2020
      f[f < 0] <- 0
      f <- 0.95 ^ pmax(0, log(f))

      # get Industry FE items
      v <- grep('^SSP1\\.fe(..i$|ind)', getNames(data), value = TRUE)

      # apply changes
      for (i in 1:length(y)) {
        if (1 != f[i]) {
          data[,y[i],v] <- data[,y[i],v] * f[i]
        }
      }

    } else if (subtype == "ES"){
      Unit2Million = 1e-6

      services <- readSource("EDGE",subtype="ES_buildings")
      getSets(services) <- gsub("data", "item", getSets(services))
      data <- services*Unit2Million
      unit_out = "million square meters times degree [1e6.m2.C]"
      description_out = "demand pathways for energy service in buildings"
    }

    if (subtype %in% c("FE", "FE_buildings", "UE_buildings", "FE_for_Eff", "UE_for_Eff", "ES")) {
      mapping = toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv", where = "mappingfolder")

      REMIND_dimensions = "REMINDitems_out"
      sets_names = getSets(data)

      # add total buildings electricity demand: feelb = feelcb + feelhpb + feelrhb
      mapping <- rbind(
        mapping,
        mapping %>%
          filter(.data$REMINDitems_out %in% c("feelcb", "feelhpb", "feelrhb")) %>%
          mutate(REMINDitems_out = "feelb")
      )

    } else if (subtype %in% c("EsUeFe_in","EsUeFe_out")){

        mapping_path <- toolGetMapping(type = "sectoral", name = "structuremappingIO_EsUeFe.csv", returnPathOnly = TRUE, where = "mappingfolder")
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
    if (subtype %in% c("FE_buildings", "UE_buildings")) {
      # filter only buildings (simple) ppf
      magpnames <- filter(magpnames, grepl("^fe..b$|^feel..b$|^feelcb$", .data$REMINDitems_out))
    }
    if (subtype == "UE_buildings") {
      # change mapping from FE to UE
      mapping <- mapping %>%
        mutate(EDGEitems = gsub("_fe$", "_ue", .data[["EDGEitems"]]),
               REMINDitems_out = gsub("^fe", "ue", .data[["REMINDitems_out"]])) %>%
        rbind(mapping)
      magpnames <- magpnames %>%
        mutate(REMINDitems_out = gsub("^fe", "ue", .data[["REMINDitems_out"]]))
    }
    magpnames <- expand_vectors(
      if (subtype %in% c("FE_buildings", "UE_buildings")) {
        cartesian(scenarios, rcps)
      } else {
        scenarios
      },
      magpnames)

    if (length(setdiff(edge_names, mapping$EDGEitems) > 0 )) stop("Not all EDGE items are in the mapping")


    # make an empty new magpie object
    reminditems <- as.magpie(array(dim = c(length(regions), length(years), length(magpnames)),
                                   dimnames = list(regions, years, magpnames)))
    getSets(reminditems) <- sets_names

    datatmp <- data
    # Take the names of reminditems without scenario and rcp dimension
    names_NoScen <- unique(lapply(getNames(reminditems), function(name) {
      strsplit(name, ".", fixed = TRUE)[[1]] %>%
        `[`(!tail(getSets(reminditems), -2) %in% c("scenario", "rcp")) %>%
        paste(collapse = ".")
    }))

    for (reminditem in names_NoScen) {
      # Concatenate names from mapping columns so that they are comparable with
      # names from magclass object
      if (length(REMIND_dimensions) > 1) {
        names_mapping <- apply(mapping[REMIND_dimensions], 1, paste, collapse = ".")
      } else {
        names_mapping <- mapping[[REMIND_dimensions]]
      }
      # Only select EDGE variables which correspond to the remind
      testdf <- mapping[names_mapping == reminditem,
                        c("EDGEitems", "weight_Fedemand")]
      prfl <- testdf[,"EDGEitems"]
      vec <- as.numeric(mapping[rownames(testdf),"weight_Fedemand"])
      names(vec) <- prfl
      mselect(datatmp, item = prfl) <- mselect(data, item = prfl) * as.magpie(vec)
      reminditems[, , reminditem] <- dimSums(mselect(datatmp, item = prfl),
                                             dim = "item", na.rm = TRUE)
    }

    # remove missing Navigate scenarios
    if (subtype %in% c("FE_buildings", "UE_buildings")) {
      reminditems <- reminditems[, , grep("SSP2EU_NAV_.{3}\\.rcp", getItems(reminditems, 3), value = TRUE), invert = TRUE]
    }

    #Change the scenario names for consistency with REMIND sets
    getNames(reminditems) <- gsub("^SSP","gdp_SSP",getNames(reminditems))
    getNames(reminditems) <- gsub("SDP","gdp_SDP",getNames(reminditems))

    if ('FE' == subtype) {

      # ---- _modify SSP1/SSP2 data of CHN/IND further ----
      # To achieve projections more in line with local experts, apply tuning
      # factor f to liquids and gas consumption in industry in CHN and IND.
      # Apply additional energy intensity reductions 2015-30, that are phased
      # out halfway until 2040 again.
      # IEIR - initial energy intensity reduction [% p.a.] in 2016
      # FEIR - final energy intensity recovery [% p.a.] in 2040
      # The energy intensity reduction is cumulative over the 2016-40 interval
      # and thereafter constant.

      mod_factors <- tribble(
        # enter tuning factors for regions/energy carriers
        ~region,   ~pf,        ~IEIR,   ~FEIR,
        'CHN',     'fehoi',     2.5,    -0.5,
        'CHN',     'fegai',    -2.5,     3,
        'CHN',     'feeli',     0.5,     1.5,
        'IND',     'fehoi',     3,       0,
        'IND',     'fegai',    12,      -5) %>%
        # SSP1 factors are half those of SSP2
        gather('variable', 'gdp_SSP2', !!sym('IEIR'), !!sym('FEIR'),
               factor_key = TRUE) %>%
        mutate(gdp_SSP1 = !!sym('gdp_SSP2') / 2) %>%
        gather('scenario', 'value', matches('^gdp_SSP')) %>%
        spread('variable', 'value') %>%
        mutate(t = as.integer(2016)) %>%
        # add missing combinations (neutral multiplication by 1) for easy
        # joining
        complete(crossing(!!sym('scenario'), !!sym('region'), !!sym('pf'), !!sym('t')),
                 fill = list(IEIR = 0, FEIR = 0)) %>%
        # fill 2016-40 values
        complete(nesting(!!sym('scenario'), !!sym('region'), !!sym('pf'),
                         !!sym('IEIR'), !!sym('FEIR')),
                 t = 2016:2040) %>%
        group_by(!!sym('scenario'), !!sym('region'), !!sym('pf')) %>%
        mutate(
          f = seq(1 - unique(!!sym('IEIR')) / 100,
                  1 - unique(!!sym('FEIR')) / 100,
                  along.with = !!sym('t'))) %>%
        # extend beyond 2050 (neutral multiplication by 1)
        complete(t = c(1993:2015, 2041:2150), fill = list(f = 1)) %>%
        arrange(!!sym('t')) %>%
        mutate(f = cumprod(!!sym('f'))) %>%
        filter(t %in% as.integer(sub('y', '', y))) %>%
        ungroup() %>%
        select(-'IEIR', -'FEIR')

      mod_factors <- bind_rows(
        mod_factors,

        mod_factors %>%
          filter('gdp_SSP2' == .data$scenario) %>%
          mutate(scenario = 'gdp_SSP2EU')
      )

      mod_r <- unique(mod_factors$region)
      mod_sp <- cartesian(unique(mod_factors$scenario), unique(mod_factors$pf))

      reminditems[mod_r,,mod_sp] <- reminditems[mod_r,,mod_sp] %>%
        as.quitte() %>%
        as_tibble() %>%
        mutate(scenario = as.character(!!sym('scenario')),
               region   = as.character(!!sym('region')),
               item     = as.character(!!sym('item'))) %>%
        full_join(mod_factors, c('scenario', 'region', 'period' = 't',
                                 'item' = 'pf')) %>%
        mutate(value = !!sym('f') * !!sym('value')) %>%
        select(-'f') %>%
        as.quitte() %>%
        as.magpie()

      # add SDP transport and industry scenarios
      SDP_industry_transport <- mbind(addSDP_transport(reminditems),
                                      addSDP_industry(reminditems))

      # delete punk SDP data calculated illicitly in readEDGE('FE_stationary')
      reminditems <- mbind(
        reminditems[,,setdiff(getNames(reminditems),
                              getNames(SDP_industry_transport))],
        SDP_industry_transport)

      ## calculate *real* useful (i.e., motive) energy instead of
      ## fossil-fuel equivalents for light- and heavy-duty vehicles
      ## sources for TtW efficiencies:
      ##  Cox, B., et al. (2020) Life cycle environmental and cost comparison of
      ##  current and future passenger cars under different energy scenarios.
      ##  Applied Energy2.
      ## Sacchi, R., et al. (2020) carculator: an open-source tool for
      ## prospective environmental and economic life cycle assessment of
      ## vehicles. When, Where and How can battery-electric vehicles help reduce
      ##  greenhouse gas emissions? Renewable and Sustainable Energy Reviews,
      ##  submitted (in review). https://www.psi.ch/en/media/57994/download

      #reminditems[,, "ueLDVt"] <- reminditems[,, "ueLDVt"] * 0.22
      #reminditems[,, "ueHDVt"] <- reminditems[,, "ueHDVt"] * 0.24


      # ---- Industry subsectors data and FE stubs ----
      ## subsector activity projections ----
      industry_subsectors_ue <- mbind(
        calcOutput(
          type = 'Industry_Value_Added',
          match.steel.historic.values = TRUE,
          match.steel.estimates = 'IEA_ETP',
          China_Production = readSource(type = 'ExpertGuess',
                                        subtype = 'Chinese_Steel_Production',
                                        convert = FALSE) %>%
            madrat_mule(),
          aggregate = FALSE,
          years = getYears(reminditems), supplementary = FALSE),

        calcOutput(
          type = 'Steel_Projections',
          subtype = 'production',
          match.steel.historic.values = TRUE,
          match.steel.estimates = 'IEA_ETP',
          China_Production = readSource(type = 'ExpertGuess',
                                        subtype = 'Chinese_Steel_Production',
                                        convert = FALSE) %>%
            madrat_mule(),
          aggregate = FALSE,
          years = getYears(reminditems), supplementary = FALSE)
      )

      ## re-curve specific industry activity per unit GDP ----
      GDP <- calcOutput(type = 'GDP', average2020 = FALSE,
                        years = getYears(reminditems), aggregate = FALSE,
                        supplementary = FALSE) %>%
        as.data.frame() %>%
        as_tibble() %>%
        select(iso3c = 'Region', year = 'Year', scenario = 'Data1',
               GDP = 'Value') %>%
        character.data.frame() %>%
        mutate(year = as.integer(.data$year))

      ### fix missing GDP numbers ----
      # (see https://github.com/pik-piam/mrdrivers/issues/40)
      if (any(0 == GDP$GDP)) {
        GDP_fuckup_point <- GDP %>%
          filter(0 == .data$GDP) %>%
          group_by(!!!syms(c('iso3c', 'scenario'))) %>%
          filter(min(.data$year) == .data$year) %>%
          ungroup()

        GDP_replacement_scenario <- setdiff(
          unique(GDP$scenario),

          GDP_fuckup_point %>%
            pull('scenario') %>%
            unique()
        ) %>%
          first()

        GDP_fuckup_point <- GDP_fuckup_point %>%
          group_by(!!!syms(c('iso3c', 'scenario'))) %>%
          mutate(base.year = getYears(reminditems, TRUE) %>%
                   `[`(which(getYears(reminditems, TRUE) == !!sym('year')) - 1),
                 base.scenario = GDP_replacement_scenario) %>%
          ungroup() %>%
          select(-'GDP')

        GDP_replacement <- full_join(
          GDP %>%
            semi_join(
              GDP_fuckup_point,
              c('iso3c', 'scenario')
            ) %>%
            left_join(
              GDP_fuckup_point %>%
                select('iso3c', 'scenario', 'base.year'),

              c('iso3c', 'scenario')
            ) %>%
            filter(.data$year >= .data$base.year) %>%
            select('iso3c', 'year', 'scenario', 'GDP'),

          GDP %>%
            semi_join(
              GDP_fuckup_point,

              c('iso3c', scenario = 'base.scenario')
            ) %>%
            left_join(
              GDP_fuckup_point %>%
                select('iso3c', 'base.year', scenario = 'base.scenario') %>%
                distinct(),

              c('iso3c', 'scenario')
            ) %>%
            filter(.data$year >= .data$base.year) %>%
            select(-'base.year') %>%
            group_by(!!!syms(c('iso3c', 'scenario'))) %>%
            mutate(factor = .data$GDP / lag(.data$GDP, default = first(.data$GDP),
                                            order_by = .data$year)) %>%
            ungroup() %>%
            select('iso3c', 'year', 'factor'),

          c('iso3c', 'year')
        ) %>%
          group_by(!!!syms(c('iso3c', 'scenario'))) %>%
          mutate(GDP = first(.data$GDP) * .data$factor) %>%
          ungroup() %>%
          select(all_of(colnames(GDP)))

        GDP <- bind_rows(
          anti_join(GDP, GDP_replacement, by = c('iso3c', 'scenario', 'year')),
          GDP_replacement
        ) %>%
          verify(expr = 0 < .data$GDP, description = 'All GDP numbers > 0')
      }

      ### calculate specific material demand factors ----
      foo <- full_join(
        industry_subsectors_ue %>%
          as.data.frame() %>%
          as_tibble() %>%
          select(iso3c = 'Region', year = 'Year', scenario = 'Data1', subsector = 'Data2', value = 'Value') %>%
          character.data.frame() %>%
          mutate(year = as.integer(.data$year)) %>%
          # remove zero activity from historic data, to be extended backwards by
          # first projection below
          filter(!(0 == .data$value & 2020 >= .data$year)) %>%
          verify(expr = .data$value != 0, description = 'No zero subsector activity after 2020'),
        GDP,
        by = c('iso3c', 'year', 'scenario')
      ) %>%
        # expand time series of per-GDP production back into time for
        # non-existent production in historic periods (e.g. primary steel in
        # NEN)
        complete(nesting(!!!syms(c('iso3c', 'year', 'scenario', 'GDP'))),
                 subsector = unique(.data$subsector),
                 fill = list(value = NA_real_)) %>%
        group_by(.data$iso3c, .data$scenario, .data$subsector) %>%
        mutate(
          value = .data$GDP
                * na.fill(object = .data$value / .data$GDP,
                          fill = first(
                            x = .data$value[!is.na(.data$value)]
                              / .data$GDP[!is.na(.data$value)],
                            order_by = .data$year[!is.na(.data$value)]))
        ) %>%
        ungroup()

      region_mapping_21 <- toolGetMapping('regionmapping_21_EU11.csv', 'regional', where = "mappingfolder") %>%
        as_tibble() %>%
        select(iso3c = 'CountryCode', region = 'RegionCode')

      industry_subsectors_material_alpha <- calcOutput(
        type = 'industry_subsectors_specific', subtype = 'material_alpha',
        scenarios = c(getNames(x = industry_subsectors_ue, dim = 1),
                      'gdp_SSP2_lowEn'),
        regions = unique(region_mapping_21$region),
        aggregate = FALSE
      ) %>%
        as.data.frame() %>%
        as_tibble() %>%
        select(scenario = 'Data1', region = 'Data2', subsector = 'Data3',
               name = 'Data4', value = 'Value') %>%
        character.data.frame() %>%
        pivot_wider() %>%
        mutate(subsector = paste0('ue_', .data$subsector))

      industry_subsectors_material_relative <- calcOutput(
        type = 'industry_subsectors_specific', subtype = 'material_relative',
        scenarios = c(getNames(x = industry_subsectors_ue, dim = 1),
                      'gdp_SSP2_lowEn'),
        regions = unique(region_mapping_21$region),
        aggregate = FALSE
      ) %>%
        as.data.frame() %>%
        as_tibble() %>%
        select(scenario = 'Data1', base.scenario = 'Data2', region = 'Data3',
               subsector = 'Data4', name = 'Data5', value = 'Value') %>%
        character.data.frame() %>%
        pivot_wider() %>%
        mutate(subsector = paste0('ue_', .data$subsector))

      if (use_ODYM_RECC) {
        industry_subsectors_material_relative %<>%
          filter(!.data$scenario %in% c('gdp_SDP_EI', 'gdp_SDP_MC',
                                        'gdp_SDP_RC'))
      }


      industry_subsectors_material_relative_change <- calcOutput(
        type = 'industry_subsectors_specific',
        subtype = 'material_relative_change',
        scenarios = c(getNames(x = industry_subsectors_ue, dim = 1),
                      'gdp_SSP2_lowEn'),
        regions = unique(region_mapping_21$region),
        aggregate = FALSE
      ) %>%
        as.data.frame() %>%
        as_tibble() %>%
        select(scenario = 'Data1', base.scenario = 'Data2', region = 'Data3',
               subsector = 'Data4', name = 'Data5', value = 'Value') %>%
        character.data.frame() %>%
        pivot_wider() %>%
        mutate(subsector = paste0('ue_', .data$subsector))

      if (use_ODYM_RECC) {
        industry_subsectors_material_percapita <- calcOutput(
          type = 'ODYM_RECC',
          subtype = 'REMIND_industry_trends',
          aggregate = FALSE) %>%
          magclass_to_tibble() %>%
          mutate(scenario = sub('^(gdp_)?', 'gdp_', .data$scenario)) %>%
          filter(!.data$scenario %in% c(
            unique(industry_subsectors_material_alpha$scenario),
            unique(industry_subsectors_material_relative$scenario),
            unique(industry_subsectors_material_relative_change$scenario))) %>%
          interpolate_missing_periods_(
            periods = list(
              year = unique(pmax(as.integer(sub('y', '', years, fixed = TRUE)),
                                 min(.$year)))),
            expand.values = TRUE)
      }

      bind_rows(
        industry_subsectors_material_alpha %>% select('scenario', 'region', 'subsector'),
        industry_subsectors_material_relative %>% select('scenario', 'region', 'subsector'),
        industry_subsectors_material_relative_change %>% select('scenario', 'region', 'subsector')
      ) %>%
        group_by(!!!syms(c('scenario', 'region', 'subsector'))) %>%
        summarise(count = n(), .groups = 'drop') %>%
        verify(expr = 1 == .data$count,
               error_fun = function(errors, data) {
                 stop('Industry specific material is over-specified for:\n',
                      paste(
                        format(data[errors[[1]]$error_df$index,],
                               width = 80,
                               n = errors[[1]]$num.violations),
                        collapse = '\n'))
               })

      foo2 <- bind_rows(
        ### SSP2EU default scenario ----
        foo %>% filter('gdp_SSP2EU' == .data$scenario),

        ### alpha relative to SSP2EU ----
        industry_subsectors_material_alpha %>%
          full_join(
            full_join(
              foo %>%
                filter('gdp_SSP2EU' == .data$scenario) %>%
                # keep specific production constant for historic years without
                # production
                group_by(!!!syms(c('iso3c', 'subsector'))) %>%
                mutate(specific.production = .data$value / .data$GDP,
                       specific.production = ifelse(
                         0 != .data$value, .data$specific.production,
                         first(.data$specific.production[0 != .data$value],
                               order_by = .data$year))) %>%
                ungroup() %>%
                select('iso3c', 'year', 'subsector', 'specific.production') %>%
                interpolate_missing_periods_(
                  periods = list(year = seq_range(range(.$year))),
                  value = 'specific.production',
                  method = 'linear'),

              # mark years which have no historic production so that we can set
              # them to 0 once we calculated future production for the new
              # scenarios
              foo %>%
                filter('gdp_SSP2EU' == .data$scenario) %>%
                mutate(fake.value = 0 == .data$value) %>%
                select('iso3c', 'year', 'subsector', 'fake.value') %>%
                interpolate_missing_periods_(
                  periods = list(year = seq_range(range(.$year))),
                  value = 'fake.value',
                  method = 'linear') %>%
                mutate(fake.value = 0 != .data$fake.value),

              c('iso3c', 'year', 'subsector')
            ) %>%
              full_join(region_mapping_21, 'iso3c'),

            by = c('region', 'subsector'),
            relationship = 'many-to-many'
          ) %>%
          group_by(!!!syms(c('scenario', 'subsector', 'iso3c'))) %>%
          mutate(
            # alpha factors converge linearly towards zero over the convergence
            # time
            conv.fctr = 1 - pmin(1, ( (.data$year - 2015)
                                      / .data$convergence_time)),
            alpha.conv = ifelse(2015 >= .data$year, 1,
                                1 - .data$alpha * .data$conv.fctr),
            # specific production is scaled with the cumulated converged alpha
            # factors
            cum.fctr = cumprod(.data$alpha.conv),
            specific.production = ifelse(
              2020 >= .data$year, .data$specific.production,
              ( .data$specific.production[2020 == .data$year]
                * .data$cum.fctr
              ))
            # ensure that years without historic production are 0
            * (1 - .data$fake.value)) %>%
          ungroup() %>%
          select('region', 'iso3c', 'year', 'scenario', 'subsector',
                 'specific.production') %>%
          filter(.data$year %in% unique(foo$year)) %>%
          left_join(
            bind_rows(
              foo,

              foo %>%
                filter('gdp_SSP2EU' == .data$scenario) %>%
                mutate(scenario = 'gdp_SSP2_lowEn')
            ),

            c('scenario', 'subsector', 'iso3c', 'year')
          ) %>%
          mutate(value = .data$specific.production * .data$GDP) %>%
          select(all_of(colnames(foo))) %>%
          assert(not_na, everything())
      )

      foo3 <- bind_rows(
        foo2,

        ### specific production relative baseline ----
        industry_subsectors_material_relative %>%
          left_join(
            foo2 %>%
              mutate(specific.production = .data$value / .data$GDP) %>%
              select(base.scenario = 'scenario', 'subsector', 'iso3c', 'year',
                     'specific.production') %>%
              full_join(region_mapping_21, 'iso3c'),

            by = c('base.scenario', 'region', 'subsector'),
            relationship = 'many-to-many'
          ) %>%
          left_join(
            foo %>%
              select('scenario', 'subsector', 'iso3c', 'year', 'GDP', 'value'),

            c('scenario', 'subsector', 'iso3c', 'year')
          ) %>%
          assert(not_na, everything()) %>%
          # scale factor in from 2020-35
          mutate(l = pmin(1, pmax(0, (.data$year - 2020) / (2035 - 2020))),
                 value = .data$specific.production
                 * .data$GDP
                 * (.data$factor * .data$l + 1 * (1 - .data$l))) %>%
          select(all_of(colnames(foo))),

        ### specific production change relative baseline ----
        full_join(
          # base scenario data
          foo %>%
            rename(base.scenario = 'scenario') %>%
            semi_join(
              industry_subsectors_material_relative_change,

              c('base.scenario', 'subsector')
            ) %>%
            full_join(region_mapping_21, 'iso3c'),

          # change parameters
          industry_subsectors_material_relative_change,

          c('base.scenario', 'region', 'subsector')
        ) %>%
          select('scenario', 'iso3c', 'region', 'subsector', 'year',
                 base.value = 'value', base.GDP = 'GDP', 'factor') %>%
          # GDP trajectories of target scenarios
          left_join(
            foo %>%
              select('scenario', 'iso3c', 'subsector', 'year', 'GDP'),

            c('scenario', 'iso3c', 'subsector', 'year')
          ) %>%
          group_by(!!!syms(c('scenario', 'iso3c', 'subsector'))) %>%
          mutate(
            # specific production of base scenarios
            base.specific.production = .data$base.value / .data$base.GDP,
            # change in specific production of base scenarios relative to 2020
            base.change = ( .data$base.specific.production
                          / .data$base.specific.production[2020 == .data$year]),
            # modified change of target scenarios
            # If base change is below (above) 1, i.e. material efficiency is
            # improving (deteriorating), efficiency gains (losses) are halved
            # (doubled).  Changes of historic values (i.e. before 2015) are
            # identical to base scenario.  Not finite changes (e.g. division by
            # zero) lead to constant values.
            change = case_when(
              !is.finite(.data$base.change) ~ 1,
              2020 >= .data$year ~ .data$base.change,
              TRUE ~  ( ( (.data$base.change - 1)
                          * .data$factor ^ sign(1 - .data$base.change)
              )
              + 1)),
            specific.production =
              ( .data$base.specific.production[2015 == .data$year]
                * .data$change
              ),
            value = ifelse(!is.finite(.data$base.change),
                           .data$base.value,
                           .data$specific.production * .data$GDP)) %>%
          ungroup() %>%
          select('scenario', 'iso3c', 'subsector', 'year', 'value', 'GDP')
      )

      ### per-capita projections ----
      . <- NULL

      if (use_ODYM_RECC) {
        foo4 <- bind_rows(
          foo3 %>% select(-'GDP'),

          foo3 %>%
            filter(
              'gdp_SSP2EU' == .data$scenario,
              min(industry_subsectors_material_percapita$year) > .data$year) %>%
            select(-'GDP') %>%
            complete(
              nesting(!!!syms(c('iso3c', 'year', 'subsector', 'value'))),
              scenario = unique(industry_subsectors_material_percapita$scenario)
            ) %>%
            filter('gdp_SSP2EU' != .data$scenario),

          foo3 %>%
            filter(
              'gdp_SSP2EU' == .data$scenario,
              min(industry_subsectors_material_percapita$year) == .data$year
              ) %>%
            select(-'scenario', -'GDP', -'year') %>%
            inner_join(
              calcOutput(type = 'Population', aggregate = FALSE,
                         years = getYears(reminditems)) %>%
                magclass_to_tibble() %>%
                select('iso3c', 'scenario' = 'variable', 'year',
                       'population' = 'value') %>%
                mutate(scenario = sub('^pop_', 'gdp_', .data$scenario)) %>%
                filter(
                  min(industry_subsectors_material_percapita$year) <= .data$year,
                  .data$scenario %in%
                    unique(industry_subsectors_material_percapita$scenario)
                ) %>%
                group_by(.data$iso3c, .data$scenario) %>%
                mutate(
                  population = .data$population
                  / first(.data$population, order_by = .data$year)) %>%
                ungroup(),

              'iso3c') %>%
            inner_join(
              industry_subsectors_material_percapita %>%
                mutate(subsector = paste0('ue_', .data$subsector)) %>%
                interpolate_missing_periods_(
                  periods = list(year = seq_range(range(.$year)))) %>%
                rename(activity = 'value'),

              c('iso3c', 'subsector', 'year', 'scenario')
            ) %>%
            mutate(value = .data$value * .data$population * .data$activity) %>%
            select('iso3c', 'scenario', 'subsector', 'year', 'value')
        )

        industry_subsectors_ue <- foo4 %>%
          select('iso3c', 'year', 'scenario', pf = 'subsector', 'value') %>%
          as.magpie(spatial = 1, temporal = 2, data = ncol(.))
      } else {
        industry_subsectors_ue <- foo3 %>%
          select('iso3c', 'year', 'scenario', pf = 'subsector', 'value') %>%
          as.magpie(spatial = 1, temporal = 2, data = ncol(.))
      }

      ## subsector FE shares ----
      ### get 1993-2020 industry FE ----
      industry_subsectors_en <- calcOutput(
        type = 'IO', subtype = 'output_Industry_subsectors',
        aggregate = FALSE
      ) %>%
        # convert to data frame
        as.data.frame() %>%
        as_tibble() %>%
        select(iso3c = 'Region', year = 'Year', pf = 'Data2',
               value = 'Value') %>%
        character.data.frame() %>%
        mutate(year = as.integer(as.character(.data$year))) %>%
        # get 1993-2020 industry FE data
        filter(grepl('^fe.*_(cement|chemicals|steel|otherInd)', .data$pf),
               between(.data$year, 1993, 2020)) %>%
        # sum up fossil and bio SE (which produce the same FE), aggregate
        # regions
        full_join(region_mapping_21, 'iso3c') %>%
        group_by(!!!syms(c('year', 'region', 'pf'))) %>%
        summarise(value = sum(.data$value), .groups = 'drop') %>%
        # split feel steel into primary and secondary production
        left_join(
          industry_subsectors_ue[,,'ue_steel', pmatch = TRUE] %>%
            as.data.frame() %>%
            as_tibble() %>%
            select(iso3c = 'Region', scenario = 'Data1', year = 'Year',
                   subsector = 'Data2', production = 'Value') %>%
            filter('gdp_SSP2' == .data$scenario) %>%
            select(-'scenario') %>%
            # aggregate regions
            full_join(region_mapping_21, 'iso3c') %>%
            group_by(!!!syms(c('year', 'region', 'subsector'))) %>%
            summarise(production = sum(.data$production), .groups = 'drop') %>%
            mutate(year      = as.integer(as.character(.data$year)),
                   pf        = 'feel_steel',
                   subsector = sub('_production$', '', .data$subsector)) %>%
            pivot_wider(names_from = 'subsector', values_from = 'production'),

          c('region', 'year', 'pf')
        ) %>%
        group_by(!!!syms(c('year', 'region', 'pf'))) %>%
        # if there was no historic steel production, but feel_steel was
        # allocated, assume primary and secondary production to be identical
        mutate(
          foo = .data$ue_steel_primary + .data$ue_steel_secondary,
          ue_steel_primary   = ifelse(0 == .data$foo, 1,
                                      .data$ue_steel_primary),
          ue_steel_secondary = ifelse(0 == .data$foo, 1,
                                      .data$ue_steel_secondary)
        ) %>%
        select(-'foo') %>%
        # assume that secondary steel production is nine times as electricity
        # intensive (not energy intensive!) as primary production, since
        # detailed data is missing so far
        mutate(feel_steel_secondary =
                 (9 * .data$ue_steel_secondary * .data$value)
               / (9 * .data$ue_steel_secondary + .data$ue_steel_primary),
               feel_steel_primary = .data$value - .data$feel_steel_secondary
        ) %>%
        ungroup() %>%
        select(-'ue_steel_primary', -'ue_steel_secondary') %>%
        pivot_wider(names_from = 'pf') %>%
        select(-'feel_steel') %>%
        pivot_longer(matches('^fe.*'), names_to = 'pf', values_drop_na = TRUE)

      # can be removed once feelwlth is replaced by feel in all mappings for
      # calcIO()
      industry_subsectors_en <- industry_subsectors_en %>%
        mutate(pf = sub('^feelwlth_', 'feel_', .data$pf))

      ### extend to 2020 using WEO 2021 data ----
      IEA_WEO_2021_ref_year <- 2020L
      last_industry_subsectors_en_year <- max(industry_subsectors_en$year)
      if (IEA_WEO_2021_ref_year > last_industry_subsectors_en_year) {
        industry_subsectors_en <- bind_rows(
          industry_subsectors_en,

          inner_join(
            industry_subsectors_en %>%
              filter(2010 == .data$year),

            readSource(type = 'IEA_WEO_2021', subtype = 'region',
                       convert = TRUE) %>%
              magclass_to_tibble(c('iso3c', 'year', 'scenario', 'variable', 'industry.FE')) %>%
              filter(.data$year %in% c(2010, IEA_WEO_2021_ref_year),
                     'Stated Policies Scenario' == .data$scenario,
                     'Energy-Total-Industry (EJ)' == .data$variable) %>%
              left_join(region_mapping_21, 'iso3c') %>%
              group_by(!!!syms(c('region', 'year'))) %>%
              summarise(industry.FE = sum(.data$industry.FE, na.rm = TRUE),
                        .groups = 'drop_last') %>%
              summarise(change = last(.data$industry.FE, order_by = .data$year)
                        / first(.data$industry.FE, order_by = .data$year),
                        .groups = 'drop'),

            'region'
          ) %>%
            mutate(value = .data$value * .data$change,
                   year  = IEA_WEO_2021_ref_year) %>%
            select(-'change')
        ) %>%
          pivot_wider(names_from = 'year') %>%
          mutate(`2015` = ( !!sym('2010')
                            + !!sym('2015')
                            + !!sym(as.character(IEA_WEO_2021_ref_year))
          )
          / 3) %>%
          pivot_longer(matches('^[0-9]*$'), names_to = 'year',
                       names_transform = list(year = as.integer)) %>%
          interpolate_missing_periods_(periods = list(year = seq_range(range(.$year)))) %>%
          assert(not_na, everything()) %>%
          verify(is.finite(.data$value))

        FE_alpha_mod <- 0.9   # found by fiddling around
      } else {
        FE_alpha_mod <- 1
      }

      ### calculate 1993-2020 industry subsector FE shares ----
      industry_subsectors_en_shares <- industry_subsectors_en %>%
        mutate(subsector = sub('^[^_]+_', '', .data$pf),
               subsector = ifelse('steel' == .data$subsector, 'steel_primary',
                                  .data$subsector)) %>%
        group_by(!!!syms(c('year', 'region', 'subsector'))) %>%
        mutate(share = .data$value / sum(.data$value),
               share = ifelse(is.finite(.data$share), .data$share, 0)) %>%
        ungroup() %>%
        select(-'value')

      failed_share_sum <- industry_subsectors_en_shares %>%
        group_by(!!!syms(c('year', 'region', 'subsector'))) %>%
        summarise(failed = abs(sum(.data$share) - 1) > 1e-15, .groups = 'drop')

      if (any(failed_share_sum$failed)) {
        stop('industry_subsectors_en_shares don\'t add up to 1.')
      }

      ### future subsector FE shares from IEA ETP 2017 ----
      IEA_ETP_Ind_FE_shares <- readSource('IEA_ETP', 'industry',
                                          convert = FALSE) %>%
        # filter for OECD and Non-OECD regions and RTS scenario
        `[`(c('OECD', 'Non-OECD'),,'RTS', pmatch = 'left') %>%
        # convert to data frame
        as.data.frame() %>%
        as_tibble() %>%
        select(region = 'Region', year = 'Year', variable = 'Data2',
               value = 'Value') %>%
        character.data.frame() %>%
        mutate(year = as.integer(as.character(.data$year))) %>%
        # filter for future data
        filter(max(industry_subsectors_en$year) < .data$year) %>%
        # rename variables
        right_join(
          tribble(
            ~subsector,    ~fety,    ~variable,
            'cement',      'feso',   'Industry|Cement - final energy consumption|Coal',
            'cement',      'feso',   'Industry|Cement - final energy consumption|Biomass',
            'cement',      'feso',   'Industry|Cement - final energy consumption|Waste',
            'cement',      'feso',   'Industry|Cement - final energy consumption|Other renewables',
            'cement',      'feli',   'Industry|Cement - final energy consumption|Oil',
            'cement',      'fega',   'Industry|Cement - final energy consumption|Natural gas',
            'cement',      'feel',   'Industry|Cement - final energy consumption|Electricity',

            'chemicals',   'feso',   'Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|Coal',
            'chemicals',   'feso',   'Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|Biomass',
            'chemicals',   'feso',   'Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|Waste',
            'chemicals',   'feso',   'Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|Other renewables',
            'chemicals',   'feli',   'Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|Oil',
            'chemicals',   'fega',   'Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|Natural gas',
            'chemicals',   'feel',   'Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|Electricity',

            'steel',       'feso',   'Industry|Iron and steel - final energy consumption incl_ blast furnaces and coke ovens|Coal',
            'steel',       'feso',   'Industry|Iron and steel - final energy consumption incl_ blast furnaces and coke ovens|Biomass',
            'steel',       'feso',   'Industry|Iron and steel - final energy consumption incl_ blast furnaces and coke ovens|Waste',
            'steel',       'feso',   'Industry|Iron and steel - final energy consumption incl_ blast furnaces and coke ovens|Other renewables',
            'steel',       'feli',   'Industry|Iron and steel - final energy consumption incl_ blast furnaces and coke ovens|Oil',
            'steel',       'fega',   'Industry|Iron and steel - final energy consumption incl_ blast furnaces and coke ovens|Natural gas',
            'steel',       'feel',   'Industry|Iron and steel - final energy consumption incl_ blast furnaces and coke ovens|Electricity',

            'total',       'feso',   'Industry|Total industry final energy consumption|Coal',
            'total',       'feso',   'Industry|Total industry final energy consumption|Biomass',
            'total',       'feso',   'Industry|Total industry final energy consumption|Waste',
            'total',       'feso',   'Industry|Total industry final energy consumption|Other renewables',
            'total',       'feli',   'Industry|Total industry final energy consumption|Oil',
            'total',       'fega',   'Industry|Total industry final energy consumption|Natural gas',
            'total',       'fehe',   'Industry|Total industry final energy consumption|Heat',
            'total',       'feel',   'Industry|Total industry final energy consumption|Electricity'),

          'variable'
        ) %>%
        # drop 2055/OECD/Chemicals as the data is faulty
        filter(!(  'OECD' == .data$region
                   & 2055 == .data$year
                   & 'chemicals' == .data$subsector)) %>%
        # aggregate by subsector and fety
        group_by(!!!syms(c('region', 'year', 'subsector', 'fety'))) %>%
        summarise(value = sum(.data$value), .groups = 'drop') %>%
        # fill 2055/OECD/Chemicals gap with interpolated data
        complete(nesting(!!!syms(c('region', 'subsector', 'fety'))), year = unique(.data$year)) %>%
        interpolate_missing_periods_(periods = list(year = unique(.$year))) %>%
        # calculate otherInd as total - cement - chemicals - steel
        pivot_wider(names_from = 'subsector', values_fill = 0) %>%
        mutate(otherInd = .data$total - (.data$cement + .data$chemicals + .data$steel)) %>%
        select(-'total') %>%
        pivot_longer(c('cement', 'chemicals', 'steel', 'otherInd'), names_to = 'subsector') %>%
        filter(0 != .data$value) %>%
        # calculate share
        group_by(!!!syms(c('region', 'year', 'subsector'))) %>%
        mutate(share = .data$value / sum(.data$value)) %>%
        ungroup() %>%
        select(-'value')

      ### split feel shares and extend to SSP scenarios ----
      IEA_ETP_Ind_FE_shares <- bind_rows(
        # all pf that don't need splitting
        IEA_ETP_Ind_FE_shares %>%
          semi_join(
            industry_subsectors_en %>%
              distinct(.data$pf) %>%
              separate('pf', c('fety', 'subsector'), sep = '_',
                       extra = 'merge'),

            c('fety', 'subsector')
          ) %>%
          unite('pf', c('fety', 'subsector'), sep = '_') %>%
          # extend to SSP scenarios
          mutate(scenario = 'gdp_SSP1') %>%
          complete(nesting(!!sym('year'), !!sym('region'), !!sym('pf'),
                           !!sym('share')),
                   scenario = unique(sub('\\..*$', '',
                                         getNames(industry_subsectors_ue)))),

        # split feel_steel into primary and secondary steel
        IEA_ETP_Ind_FE_shares %>%
          filter('feel' == .data$fety, 'steel' == .data$subsector) %>%
          select(-'fety', -'subsector') %>%
          inner_join(
            industry_subsectors_ue %>%
              `[`(,,'ue_steel_', pmatch = TRUE) %>%
              as.data.frame() %>%
              as_tibble() %>%
              select(iso3c = 'Region', year = 'Year', scenario = 'Data1',
                     pf = 'Data2', value = 'Value') %>%
              character.data.frame() %>%
              mutate(year = as.integer(as.character(.data$year))) %>%
              inner_join(
                toolGetMapping(name = 'regionmappingOECD.csv',
                               type = 'regional', where = "mappingfolder") %>%
                  as_tibble() %>%
                  select(iso3c = 'CountryCode', region = 'RegionCode'),

                'iso3c'
              ) %>%
              group_by(!!!syms(c('region', 'year', 'scenario', 'pf'))) %>%
              summarise(value = sum(.data$value), .groups = 'drop'),

            c('region', 'year')
          ) %>%
          pivot_wider(names_from = 'pf') %>%
          # as above, assume that secondary steel production is nine times as
          # electricity intensive (not energy intensive!) as primary production,
          # since detailed data is missing so far
          # reduce primary steel electricity share accordingly; secondary steel
          # electricity share is 1 since only electricity is assumed to be used
          mutate(
            feel_steel_primary
            = .data$share
            - ( (9 * .data$ue_steel_secondary * .data$share)
                / (9 * .data$ue_steel_secondary + .data$ue_steel_primary)
            ),
            feel_steel_secondary = 1) %>%
          select(-'share', -'ue_steel_primary', -'ue_steel_secondary') %>%
          pivot_longer(c('feel_steel_primary', 'feel_steel_secondary'),
                       names_to = 'pf', values_to = 'share')
      )

      ### extend time horizon and convert regions ----
      IEA_ETP_Ind_FE_shares <- IEA_ETP_Ind_FE_shares %>%
        mutate(subsector = sub('^[^_]+_', '', .data$pf),
               subsector = ifelse('steel' == .data$subsector, 'steel_primary',
                                  .data$subsector)) %>%
        interpolate_missing_periods_(
          periods = list(year = unique(pmax(min(IEA_ETP_Ind_FE_shares$year),
                                            getYears(reminditems,
                                                     as.integer = TRUE)))),
          value = 'share',
          expand.values = TRUE) %>%
        inner_join(
          toolGetMapping(name = 'regionmappingOECD.csv',
                         type = 'regional', where = "mappingfolder") %>%
            as_tibble() %>%
            select(iso3c = 'CountryCode', region = 'RegionCode'),

          'region'
        ) %>%
        select('scenario', 'iso3c', period = 'year', 'pf', 'subsector',
               'share') %>%
        # weight by subsector activity
        left_join(
          industry_subsectors_ue %>%
            as.data.frame() %>%
            as_tibble() %>%
            select(iso3c = 'Region', period = 'Year', scenario = 'Data1',
                   subsector = 'Data2', activity = 'Value') %>%
            character.data.frame() %>%
            mutate(period = as.integer(as.character(.data$period)),
                   subsector = sub('^ue_', '', .data$subsector)),

          c('scenario', 'iso3c', 'period', 'subsector')
        ) %>%
        full_join(region_mapping_21, 'iso3c') %>%
        group_by(
          !!!syms(c('scenario', 'region', 'period', 'subsector', 'pf'))) %>%
        summarise(
          share = ifelse(0 == sum(.data$activity), 0,
                           sum(.data$share * .data$activity)
                         / sum(.data$activity)),
          .groups = 'drop_last') %>%
        # re-normalise shares
        mutate(share = case_when(
          0 == sum(.data$share) ~ 1 / n(),
          TRUE                  ~ .data$share / sum(.data$share))) %>%
        verify(expr = is.finite(.data$share),
               description = paste('Finite IEA ETP industry FE shares after',
                                   'time horizon extension.')) %>%
        ungroup()

      ### combine historic and future industry FE shares ----
      industry_subsectors_en_shares <- inner_join(
        industry_subsectors_en_shares %>%
          mutate(scenario = first(IEA_ETP_Ind_FE_shares$scenario)) %>%
          complete(nesting(!!!syms(setdiff(colnames(.), 'scenario'))),
                   scenario = unique(IEA_ETP_Ind_FE_shares$scenario)) %>%
          interpolate_missing_periods_(
            periods = list(year = unique(c(industry_subsectors_en_shares$year,
                                           getYears(reminditems,
                                                    as.integer = TRUE)))),
            value = 'share',
            expand.values = TRUE) %>%
          select('scenario', 'year', 'region', 'pf', 'subsector',
                 share.hist = 'share') %>%
          # re-normalise steel_primary to extended feel_steel_primary
          group_by(!!!syms(c('scenario', 'year', 'region', 'subsector'))) %>%
          mutate(share.hist = .data$share.hist / sum(.data$share.hist)) %>%
          ungroup(),

        IEA_ETP_Ind_FE_shares %>%
          interpolate_missing_periods_(
            periods = list(period = unique(c(industry_subsectors_en_shares$year,
                                             getYears(reminditems,
                                                      as.integer = TRUE)))),
            value = 'share',
            expand.values = TRUE) %>%
          select('scenario', year = 'period', 'region', 'pf', 'subsector',
                 share.future  = 'share'),

        c('scenario', 'year', 'region', 'pf', 'subsector')
      ) %>%
        mutate(foo = pmin(1, pmax(0, (.data$year - 2015) / (2100 - 2015))),
               share = .data$share.hist * (1 - .data$foo)
               + .data$share.future * .data$foo,
               subsector = ifelse('steel' == .data$subsector,
                                  'steel_primary', .data$subsector)) %>%
        verify(expr = is.finite(.data$share),
               description = paste('Finite industry FE shares after combining',
                                   'historic and future values')) %>%
        select(-'foo', -'share.hist', -'share.future')

      failed_share_sum <- industry_subsectors_en_shares %>%
        group_by(!!!syms(c('scenario', 'year', 'region', 'subsector'))) %>%
        summarise(failed = abs(sum(.data$share) - 1) > 1e-15, .groups = 'drop')

      if (any(failed_share_sum$failed)) {
        stop('industry_subsectors_en_shares don\'t add up to 1.')
      }

      ### extend to H2 and HTH_el shares ----
      feh2_share_in_fega <- 0.3
      # H2 shares grow linearly from 0.1 % to feh2_share_in_fega of fega from
      # 2020 to 2050 and are constant afterwards
      industry_subsectors_en_shares <- bind_rows(
        industry_subsectors_en_shares %>%
          filter(!grepl('^fega', .data$pf)),

        industry_subsectors_en_shares %>%
          filter(grepl('^fega', .data$pf)) %>%
          extract('pf', c('pf.fety', 'pf.subsector'), '^([^_]*)_(.*)') %>%
          complete(nesting(!!!syms(setdiff(colnames(.), 'pf.fety'))),
                   pf.fety = c('fega', 'feh2')) %>%
          pivot_wider(names_from = 'pf.fety', values_from = 'share') %>%
          mutate(feh2 = pmin(feh2_share_in_fega,
                             pmax(0.01,
                                  feh2_share_in_fega
                                  * (.data$year - 2020) / (2050 - 2020)))
                 * .data$fega,
                 fega = .data$fega - .data$feh2) %>%
          pivot_longer(c('fega', 'feh2'), names_to = 'pf.fety',
                       values_to = 'share') %>%
          unite('pf', c('pf.fety', 'pf.subsector'), sep = '_')
      )

      feelhth_share_in_fuel <- 0.08
      # HTH_el shares grow linearly from 0.1 % to feelhth_share_in_fuel of all
      # FE but feel from 2020 to 2050 and are constant afterwards
      industry_subsectors_en_shares <- bind_rows(
        # subsectors w/o HTH_el
        industry_subsectors_en_shares %>%
          filter(!.data$subsector %in% c('chemicals', 'otherInd')),

        # WLTH_el
        industry_subsectors_en_shares %>%
          filter(.data$subsector %in% c('chemicals', 'otherInd'),
                 grepl('^feel', .data$pf)) %>%
          mutate(pf = sub('^feel_', 'feelwlth_', .data$pf)),

        # HTH_el based on share of fuels
        industry_subsectors_en_shares %>%
          filter(.data$subsector %in% c('chemicals', 'otherInd'),
                 !grepl('^feel', .data$pf)) %>%
          mutate(pf = sub('(_[^_]+)$', '', .data$pf)) %>%
          group_by(!!!syms(setdiff(colnames(.), c('pf', 'share')))) %>%
          mutate(feelhth = pmin(feelhth_share_in_fuel,
                                pmax(0.01,
                                     feelhth_share_in_fuel
                                     * (.data$year - 2020) / (2050 - 2020)))
                 * sum(.data$share),
                 share = .data$share
                 / sum(.data$share)
                 * (sum(.data$share) - .data$feelhth)) %>%
          ungroup() %>%
          pivot_wider(names_from = 'pf', values_from = 'share') %>%
          pivot_longer(c(-'scenario', -'year', -'region', -'subsector'),
                       names_to = 'pf', values_to = 'share',
                       values_drop_na = TRUE) %>%
          mutate(pf = paste(.data$pf, .data$subsector, sep = '_'))
      )

      failed_share_sum <- industry_subsectors_en_shares %>%
        group_by(!!!syms(c('scenario', 'year', 'region', 'subsector'))) %>%
        summarise(failed = abs(sum(.data$share) - 1) > 1e-15, .groups = 'drop')

      if (any(failed_share_sum$failed)) {
        stop('industry_subsectors_en_shares don\'t add up to 1.')
      }

      ### calculate industry total FE level ----
      # scale industry subsector total FE by subsector activity and exogenous
      # energy efficiency gains

      specific_FE_limits <- readSource(type = 'ExpertGuess',
                                       subtype = 'industry_specific_FE_limits',
                                       convert = FALSE) %>%
        madrat_mule()

      industry_subsectors_specific_FE <- calcOutput(
        type = 'industry_subsectors_specific', subtype = 'FE',
        scenarios = c(getNames(x = industry_subsectors_ue, dim = 1),
                      'gdp_SSP2_lowEn'),
        regions = unique(region_mapping_21$region),
        aggregate = FALSE
      ) %>%
        as.data.frame() %>%
        as_tibble() %>%
        select(scenario = 'Data1', region = 'Data2', subsector = 'Data3',
               name = 'Data4', value = 'Value') %>%
        character.data.frame() %>%
        pivot_wider() %>%
        mutate(alpha = .data$alpha * FE_alpha_mod)

      industry_subsectors_specific_energy <- inner_join(
        industry_subsectors_en %>%
          mutate(subsector = sub('^[^_]+_', '', .data$pf),
                 subsector = ifelse('steel' == .data$subsector, 'steel_primary',
                                    .data$subsector),
                 scenario = first(IEA_ETP_Ind_FE_shares$scenario)) %>%
          # extend to SSP scenarios
          complete(nesting(!!!syms(setdiff(colnames(.), 'scenario'))),
                   scenario = unique(industry_subsectors_en_shares$scenario)
          ) %>%
          group_by(!!!syms(c('scenario', 'region', 'year', 'subsector'))) %>%
          summarise(value = sum(.data$value), .groups = 'drop'),

        industry_subsectors_ue %>%
          as.data.frame() %>%
          as_tibble() %>%
          select(scenario = 'Data1', iso3c = 'Region', year = 'Year',
                 pf = 'Data2', level = 'Value') %>%
          character.data.frame() %>%
          mutate(year = as.integer(as.character(.data$year))) %>%
          filter(.data$year %in% unique(industry_subsectors_en$year)) %>%
          # aggregate regions
          full_join(region_mapping_21, 'iso3c') %>%
          group_by(!!!syms(c('scenario', 'region', 'year', 'pf'))) %>%
          summarise(level = sum(.data$level), .groups = 'drop') %>%
          extract('pf', 'subsector', '^ue_(.*)$'),

        c('scenario', 'region', 'year', 'subsector')
      ) %>%
        mutate(specific.energy = .data$value / .data$level,
               specific.energy = ifelse(is.finite(.data$specific.energy),
                                        .data$specific.energy, 0)) %>%
        select('scenario', 'region', 'year', 'subsector', 'specific.energy')

      # replace 0 specific energy (e.g. primary steel NEN) with global averages
      industry_subsectors_specific_energy <-
        industry_subsectors_specific_energy %>%
        anti_join(
          industry_subsectors_specific_energy %>%
            filter(0 == .data$specific.energy),

          c('scenario', 'region', 'year', 'subsector')
        ) %>%
        bind_rows(
          left_join(
            industry_subsectors_specific_energy %>%
              filter(0 == .data$specific.energy) %>%
              select(-'specific.energy'),

            industry_subsectors_specific_energy %>%
              filter(0 != .data$specific.energy) %>%
              group_by(!!!syms(c('scenario', 'year', 'subsector'))) %>%
              summarise(specific.energy = mean(.data$specific.energy),
                        .groups = 'drop'),

            c('scenario', 'year', 'subsector')
          )
        ) %>%
        verify(expr = 0 < .data$specific.energy,
               description = 'All specific energy factors above 0')

      # replace absurdly high specific energy (e.g. primary steel NEN after IEA
      # 2021 data update) with EUR averages (considered peer-countries to NEN –
      # CHE, GRL, ISL, LIE, NOR, SJM).
      industry_subsectors_specific_energy <- industry_subsectors_specific_energy %>%
        filter('steel_primary' == .data$subsector, 100 < .data$specific.energy) %>%
        select(-'specific.energy') %>%
        left_join(
          industry_subsectors_specific_energy %>%
            filter('steel_primary' == .data$subsector,
                   .data$region %in% c('DEU', 'ECE', 'ECS', 'ENC', 'ESC', 'ESW', 'EWN', 'FRA', 'UKI')) %>%
            group_by(.data$scenario, .data$year, .data$subsector) %>%
            summarise(specific.energy = mean(.data$specific.energy), .groups = 'drop'),

          c('scenario', 'year', 'subsector')
        ) %>%
        overwrite(industry_subsectors_specific_energy, except = 'specific.energy')

      # extend time horizon
      industry_subsectors_specific_energy <-
        industry_subsectors_specific_energy %>%
        interpolate_missing_periods_(
          periods = list(year = unique(industry_subsectors_en_shares$year)),
          value = 'specific.energy', expand.values = TRUE)

      # correct lower-then-thermodynamic limit projections
      too_low_projections <- industry_subsectors_specific_energy %>%
        left_join(
          specific_FE_limits %>%
            filter('absolute' == .data$type) %>%
            select(-'type'),

          'subsector'
        ) %>%
        filter(2020 < .data$year,
               !is.na(.data$limit),
               .data$specific.energy < .data$limit) %>%
        select('scenario', 'region', 'subsector', 'year')

      if (0 != nrow(too_low_projections)) {
        industry_subsectors_specific_energy <- bind_rows(
          industry_subsectors_specific_energy %>%
            anti_join(
              too_low_projections,

              c('scenario', 'region', 'subsector', 'year')
            ),

          industry_subsectors_specific_energy %>%
            semi_join(
              too_low_projections %>%
                select(-'region'),

              c('scenario', 'subsector', 'year')
            ) %>%
            anti_join(
              too_low_projections,

              c('scenario', 'region', 'subsector', 'year')
            ) %>%
            group_by(!!!syms(c('scenario', 'subsector', 'year'))) %>%
            summarise(specific.energy = mean(.data$specific.energy),
                      .groups = 'drop') %>%
            full_join(
              too_low_projections,

              c('scenario', 'subsector', 'year')
            ) %>%
            assert(not_na, everything())
        )
      }

      # decrease values by alpha p.a.
      industry_subsectors_specific_energy <-
        industry_subsectors_specific_energy %>%
        # FIXME this factors should be derived from region data
        # since the IEA data needs fixing first, they were derived manually for
        # now
        inner_join(
          industry_subsectors_specific_FE,

          c('scenario', 'region', 'subsector')
        ) %>%
        inner_join(specific_FE_limits, 'subsector') %>%
        group_by(!!!syms(c('scenario', 'region', 'subsector'))) %>%
        arrange(.data$year) %>%
        mutate(
          specific.energy = ifelse(
            'absolute' == .data$type,
            ( (.data$specific.energy - .data$limit)
              * pmin(1, (1 - .data$alpha) ^ (.data$year - 2020))
            )
            + .data$limit,

            ( .data$specific.energy * (1 - .data$limit)
              * pmin(1, (1 - .data$alpha) ^ (.data$year - 2020))
            )
            + (.data$specific.energy * .data$limit))) %>%
        ungroup() %>%
        select('scenario', 'region', 'year', 'subsector', 'specific.energy')

      ### converge subsector en shares to global value ----
      # calculate global shares, weighted by subsector activity
      industry_subsectors_en_shares_global <- industry_subsectors_en_shares %>%
        inner_join(
          industry_subsectors_ue %>%
            as.data.frame() %>%
            as_tibble() %>%
            select(scenario = 'Data1', iso3c = 'Region', year = 'Year',
                   pf = 'Data2', level = 'Value') %>%
            character.data.frame() %>%
            mutate(year = as.integer(as.character(.data$year))) %>%
            extract('pf', 'subsector', '^ue_(.*)$') %>%
            inner_join(region_mapping_21, 'iso3c') %>%
            group_by(!!!syms(c('scenario', 'region', 'year', 'subsector'))) %>%
            summarise(level = sum(.data$level), .groups = 'drop'),

          c('scenario', 'region', 'year', 'subsector')
        ) %>%
        group_by(!!!syms(c('scenario', 'year', 'subsector', 'pf'))) %>%
        summarise(
          share.global = sum(.data$share * .data$level) / sum(.data$level),
          .groups = 'drop_last') %>%
        mutate(share.global = .data$share.global / sum(.data$share.global)) %>%
        ungroup()

      # converge
      industry_subsectors_en_shares <- inner_join(
        industry_subsectors_en_shares,
        industry_subsectors_en_shares_global,

        c('scenario', 'year', 'subsector', 'pf')
      ) %>%
        mutate(
          # converge from 2020 to 2100
          foo = pmin(1, pmax(0, (.data$year - 2020) / (2100 - 2020))),
          share = (1 - .data$foo) * .data$share
          # use minimum of regional and global share, so regions doing
          # better than the average don't regress
          + .data$foo       * pmin(.data$share, .data$share.global)) %>%
        select(-'foo', -'share.global') %>%
        # fill possible gaps in the time steps
        interpolate_missing_periods_(
          periods = list(year = getYears(reminditems, TRUE)),
          value = 'share',
          expand.values = TRUE) %>%
        group_by(!!!syms(c('scenario', 'year', 'region', 'subsector'))) %>%
        mutate(share = .data$share / sum(.data$share)) %>%
        ungroup()

      failed_share_sum <- industry_subsectors_en_shares %>%
        group_by(!!!syms(c('scenario', 'year', 'region', 'subsector'))) %>%
        summarise(failed = abs(sum(.data$share) - 1) > 1e-15, .groups = 'drop')

      if (any(failed_share_sum$failed)) {
        stop('industry_subsectors_en_shares don\'t add up to 1.')
      }

      industry_subsectors_en <- inner_join(
        industry_subsectors_specific_energy %>%
          # expand regions to iso3c
          full_join(region_mapping_21, 'region') %>%
          select(-'region'),

        industry_subsectors_ue %>%
          as.data.frame() %>%
          as_tibble() %>%
          select(scenario = 'Data1', iso3c = 'Region', year = 'Year', pf = 'Data2', level = 'Value') %>%
          character.data.frame() %>%
          mutate(year = as.integer(as.character(.data$year))) %>%
          extract('pf', 'subsector', '^ue_(.*)$') %>%
          group_by(!!!syms(c('scenario', 'iso3c', 'year', 'subsector'))) %>%
          summarise(level = sum(.data$level), .groups = 'drop'),

        c('scenario', 'iso3c', 'year', 'subsector')
      ) %>%
        mutate(value = .data$level * .data$specific.energy) %>%
        select('scenario', 'iso3c', 'year', 'subsector', 'value') %>%
        assert(is.finite, 'value') %>%
        inner_join(
          industry_subsectors_en_shares %>%
            full_join(region_mapping_21, 'region') %>%
            select(-'region'),

          c('scenario', 'iso3c', 'year', 'subsector')
        ) %>%
        group_by(!!!syms(c('scenario', 'iso3c', 'year', 'subsector'))) %>%
        mutate(share = .data$share / sum(.data$share)) %>%
        ungroup() %>%
        mutate(value = .data$value * .data$share) %>%
        select('scenario', region = 'iso3c', 'year', item = 'pf', 'value') %>%
        verify(expr = is.finite(.data$value), description = 'Finite industry_subsectors_en values') %>%
        as.magpie(spatial = 2, temporal = 3, datacol = 5)


      reminditems <- mbind(reminditems, industry_subsectors_en,
                           industry_subsectors_ue)

      unit_out <- paste0(unit_out,
                         ', except ue_cement (Gt), ue_primary_steel and ',
                         'ue_secondary_steel (Gt) and ue_chemicals and ',
                         'ue_otherInd ($tn)')
    }
    if (subtype == "UE_buildings") {
      # change item names back from UE to FE
      getItems(reminditems, "item") <- gsub("^ue", "fe", getItems(reminditems, "item"))
      description_out <- "useful energy demand in buildings"
    }
    if (subtype == "FE") {
      # duplicate SSP2EU scenarios of industry for Navigate scenarios
      industryItems <- grep("(.*i$)|chemicals|steel|otherInd|cement",
                            getItems(reminditems, 3.2), value = TRUE)
      nonIndustryItems <- setdiff(getItems(reminditems, 3.2), industryItems)
      navigateScenarios <- grep("SSP2EU_NAV_", getItems(reminditems, 3.1), value = TRUE)
      nonNavigateScenarios <- setdiff(getItems(reminditems, 3.1), navigateScenarios)
      reminditems <- mbind(
        mselect(reminditems, scenario = nonNavigateScenarios),
        mselect(reminditems, scenario = navigateScenarios, item = nonIndustryItems),
        addDim(mselect(reminditems, scenario = "gdp_SSP2EU", item = industryItems,
                       collapseNames = TRUE),
               paste0("gdp_SSP2EU_NAV_", c("act", "tec", "ele", "lce", "all")),
               "scenario", 3.1)
      )
    }

    structure_data <- switch(subtype,
                             FE = "^gdp_(SSP[1-5].*|SDP.*)\\.(fe|ue)",
                             FE_buildings = "^gdp_(SSP[1-5]|SDP).*\\..*\\.fe.*b$",
                             UE_buildings = "^gdp_(SSP[1-5]|SDP).*\\..*\\.fe.*b$",
                             FE_for_Eff = "^gdp_(SSP[1-5]|SDP).*\\.fe.*(b|s)$",
                             UE_for_Eff = "^gdp_(SSP[1-5]|SDP).*\\.fe.*(b|s)$",
                             ES = "^gdp_(SSP[1-5]|SDP).*\\.esswb$",
                             "^gdp_(SSP[1-5].*|SDP.*)\\.fe..s\\.ue.*b\\.te_ue.*b$")

    return(list(x=reminditems,weight=NULL,
                unit = unit_out,
                description = description_out,
                structure.data = structure_data))
}
