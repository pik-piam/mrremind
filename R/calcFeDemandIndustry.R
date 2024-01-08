# add documentation
calcFeDemandIndustry <- function() {

  stationary <- readSource("Stationary")

  # aggregate to 5-year averages to suppress volatility
  stationary <- toolAggregateTimeSteps(stationary)

  # fix issue with trains in transport trajectories: they seem to be 0 for t > 2100
  if (all(mselect(stationary, year = "y2105", scenario = "SSP2", item = "feelt") == 0)) {
    stationary[, seq(2105, 2150, 5), "feelt"] <- time_interpolate(stationary[, 2100, "feelt"], seq(2105, 2150, 5))
  }

  # ---- _ modify Industry FE data to carry on current trends ----
  v <- grep("\\.fe(..i$|ind)", getNames(stationary), value = TRUE)

  dataInd <- stationary[, , v] %>%
    as.quitte() %>%
    as_tibble() %>%
    select("scenario", "iso3c" = "region", "pf" = "item", "year" = "period",
           "value") %>%
    character.data.frame()

  regionmapping <- toolGetMapping(type = "regional",
                                  name = "regionmappingH12.csv", where = "mappingfolder") %>%
    select(country = "X", iso3c = "CountryCode", region = "RegionCode")

  historic_trend <- c(2004, 2020)
  phasein_period <- c(2020, 2050)   # FIXME: extend to 2055 to keep 35 yrs?
  phasein_time   <- phasein_period[2] - phasein_period[1]

  dataInd <- bind_rows(
    dataInd %>%
      filter(phasein_period[1] > !!sym("year")),
    inner_join(
      # calculate regional trend
      dataInd %>%
        # get trend period
        filter(between(!!sym("year"), historic_trend[1], historic_trend[2]),
               0 != !!sym("value")) %>%
        # sum regional totals
        full_join(regionmapping %>% select(-!!sym("country")), "iso3c") %>%
        group_by(!!sym("scenario"), !!sym("region"), !!sym("pf"),
                 !!sym("year")) %>%
        summarise(value = sum(!!sym("value")), .groups = "drop") %>%
        # calculate average trend over trend period
        interpolate_missing_periods(year = seq_range(historic_trend), expand.values = TRUE) %>%
        group_by(!!sym("scenario"), !!sym("region"), !!sym("pf")) %>%
        summarise(trend = mean(!!sym("value") / lag(!!sym("value")),
                               na.rm = TRUE),
                  .groups = "drop") %>%
        # only use negative trends (decreasing energy use)
        mutate(trend = ifelse(!!sym("trend") < 1, !!sym("trend"), NA)),
      # modify data projection
      dataInd %>%
        filter(phasein_period[1] <= !!sym("year")) %>%
        interpolate_missing_periods(year = phasein_period[1]:max(dataInd$year)) %>%
        group_by(!!sym("scenario"), !!sym("iso3c"), !!sym("pf")) %>%
        mutate(growth = replace_na(!!sym("value") / lag(!!sym("value")), 1)) %>%
        full_join(regionmapping %>% select(-"country"), "iso3c"),
      c("scenario", "region", "pf")) %>%
      group_by(!!sym("scenario"), !!sym("iso3c"), !!sym("pf")) %>%
      mutate(
        # replace NA (positive) trends with end. growth rates -> no change
        trend = ifelse(is.na(!!sym("trend")), !!sym("growth"), !!sym("trend")),
        value_ = first(!!sym("value"))
        * cumprod(
          ifelse(
            phasein_period[1] == !!sym("year"), 1,
            (!!sym("trend")
              * pmax(0, phasein_period[2] - !!sym("year") + 1)
              + !!sym("growth")
              * pmin(phasein_time, !!sym("year") - phasein_period[1] - 1)
            ) / phasein_time)),
        value = ifelse(is.na(!!sym("value_")) | 0 == !!sym("value_"),
                       !!sym("value"), !!sym("value_"))) %>%
      ungroup() %>%
      select(-"region", -"value_", -"trend", -"growth") %>%
      filter(!!sym("year") %in% unique(dataInd$year))
  ) %>%
    rename("region" = "iso3c", "item" = "pf") %>%
    as.magpie()

  # ---- _ modify SSP1 Industry FE demand ----
  # compute a reduction factor of 1 before 2021, 0.84 in 2050, and increasing
  # to 0.78 in 2150
  y <- getYears(stationary, as.integer = TRUE)
  f <- y - 2020
  f[f < 0] <- 0
  f <- 0.95^pmax(0, log(f))

  # get Industry FE items
  v <- grep("^SSP1\\.fe(..i$|ind)", getNames(dataInd), value = TRUE)

  # apply changes
  for (i in 1:length(y)) {
    if (1 != f[i]) {
      dataInd[, y[i], v] <- dataInd[, y[i], v] * f[i]
    }
  }

  return(list(x = dataInd, weight = NULL, unit = "EJ",
              description = "final energy demand in industry"))

}
