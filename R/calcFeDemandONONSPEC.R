#' Calculate historic and projected other non-specified energy demand
#'
#' Project the IEA flow ONONSPEC into the future. As we have no idea, where this
#' energy demand comes from, we use a very generic methos to project it into the
#' future: We assume an asymptotic model. It starts from the level
#' \eqn{x_\text{EOD}} at the end of data (EOD) with EOD slope
#' \eqn{\dot{x}_\text{EOD}} and approaches the fraction \eqn{\varepsilon} of
#' this slope within \eqn{\Delta t}. Both \eqn{x_\text{EOD}} and
#' \eqn{\dot{x}_\text{EOD}} are determined through a linear regression of the
#' last \eqn{n} time steps with IEA data.
#' \deqn{x(t) = x_\text{EOD} + \dfrac{\dot{x}_\text{EOD}}{c}
#'       \cdot [1 - \exp (-c \cdot (t - t_\text{EOD}))]}
#' with the decay rate \eqn{c = -\dfrac{\ln \varepsilon}{\Delta t}} \cr
#' \eqn{\Delta t} is differentiated by scenarios thus approaching a `low`,
#' `med` and `high` long-term level. To assure that scenarios don't differ at
#' the end of history (EOH), time steps between EOD and EOH are projected with
#' `med` value of \eqn{\Delta t}.
#'
#' Each scenario \eqn{s} has a differentiated \eqn{\Delta t_s}. For
#' \eqn{\dot{x}_\text{EOD} > 0}, the `high` (`low`) scenario assumes a longer
#' (shorter) time span and for \eqn{\dot{x}_\text{EOD} < 0} vice versa to reach
#' a higher (lower) long-term value. We want to make sure that until the end of
#' history (EOH), all scenarios are still identical. So we take the `med`
#' parameterisation until EOH. Afterwards, we adjust the model such that we
#' start with EOH level and slope and still reach the target slope
#' \eqn{\varepsilon \cdot \dot{x}_\text{EOD}} until
#' \eqn{t_\text{EOD} + \Delta t_s}:
#' \deqn{x_s(t) = x_\text{EOH} + \dfrac{\dot{x}_\text{EOH}}{c_s}
#'       \cdot [1 - \exp (-c_s \cdot (t - t_\text{EOH}))]}
#' with the decay rate
#' \eqn{c_s = -\dfrac{\ln \varepsilon }
#'                   {\Delta t_s - (t_\text{EOH} - t_\text{EOD})}
#'      \cdot \left(1 - \dfrac{t_\text{EOH} - t_\text{EOD}}
#'                            {\Delta t_\text{med}} \right)}
#'
#' @author Robin Hasse
#'
#' @param scenario character vector of remind demand scenarios
#' @param eoh numeric, end of history: last time step without scenario
#'   differentiation

#' @returns list with MagPIE object
#'
#' @importFrom dplyr filter group_by group_modify slice_tail ungroup left_join
#'   mutate select

calcFeDemandONONSPEC <- function(scenario, eoh) {

  # SETTINGS -------------------------------------------------------------------

  # project in 5yr time steps until
  endOfProjection <- 2150

  # approach constant level after ... years
  yearsUntilConst <- c(15, 20, 25)

  # ratio of EOD slope that qualifies as constant
  constTolerance <- 0.05

  # number of last time steps to consider in linear regression of EOD
  nTimeStepsEOD <- 10



  # FUNCTIONS ------------------------------------------------------------------

  .getFlowsIEA <- function() {
    data <- calcOutput("IOEdgeBuildings",
                       subtype = "output_EDGE",
                       aggregate = FALSE)
    getSets(data) <- c("region", "period", "item")
    # ONONSPEC have been mapped to feoth* before
    itemsONONSPEC <- grep("^feoth.+$", getItems(data, 3), value = TRUE)
    data[, , itemsONONSPEC]
  }


  .getEODcoefs <- function(x, key, model = "linear") {
    m <- stats::smooth.spline(x$period, x$value, spar = 0.7)
    eod <- max(x$period)
    data.frame(eod = eod,
               valueEOD = stats::predict(m, eod)$y,
               slopeEOD = stats::predict(m, eod, deriv = 1)$y)
  }


  .expandScenarios <- function(x, key = NULL, dt = NULL) {
    scens <- data.frame(levelScen = c("low", "med", "high"))
    if (!is.null(dt)) {
      scens$dt = if (all(x$slopeEOD > 0)) dt else rev(dt)
    }
    merge(scens, x)
  }


  .expandPeriods <- function(x, key) {
    start <- unique(x$eod) + 1
    tHist <- if (start <= eoh - 1) seq(start, eoh - 1, 1) else NULL
    tFuture <- seq(eoh, endOfProjection, 5)
    periods <- data.frame(period = c(tHist, tFuture))
    merge(periods, x)
  }


  .calcAsymptotic <- function(xStart, xDotStart, dt, t, tStart, epsCorrection = 0,
                              eps = constTolerance) {
    decay <- -log(eps) * (1 + epsCorrection) / dt
    x <- xStart + xDotStart / decay * (1 - exp(-decay * (t - tStart)))
    pmax(0, x)
  }


  .manipulateCHN <- function(x, how) {
    mask <- x$region == "CHN" & x$item == "feothelec"
    if ("eod" %in% colnames(x) && max(x$eod[mask]) >= 2025) {
      stop("This manipulation was a temporary fix and should be removed ",
           "now that data is available until 2025!")
    }
    switch(how,
      dropOutlier = {
        x <- x[!(mask & x$period == 2017), ]
      },
      boostSlope = {
        x$slopeEOD <- x$slopeEOD * ifelse(mask, 5.2, 1)
      },
      fasterToConst = {
        x$dt <- x$dt - ifelse(mask, 7, 0)
      },
      lowerTail = {
        t <- x$period - (x$eod + x$dt * 0.7)
        factor <- 0.7
        factor <- (1 - factor) / (1 + exp(0.2 * t)) + factor
        x$value <- x$value * ifelse(mask & x$period > eoh, factor, 1)
      },
      stop("Unknown manipulation.")
    )
    x
  }


  .projectFlows <- function(flowsData) {
    flowsData %>%
      filter(!is.na(.data$value)) %>%
      group_by(dplyr::across(tidyselect::all_of(c("region", "item")))) %>%
      arrange(.data$period) %>%
      .manipulateCHN("dropOutlier") %>%
      slice_tail(n = nTimeStepsEOD) %>%
      group_modify(.getEODcoefs) %>%
      group_modify(.expandPeriods) %>%
      .manipulateCHN("boostSlope") %>%
      mutate(valueHist = .calcAsymptotic(xStart = .data$valueEOD,
                                         xDotStart = .data$slopeEOD,
                                         dt = yearsUntilConst[2],
                                         t = .data$period,
                                         tStart = .data$eod)) %>%
      group_modify(.expandScenarios, yearsUntilConst) %>%
      .manipulateCHN("fasterToConst") %>%
      mutate(valueFuture = .calcAsymptotic(xStart = .data$valueHist[.data$period == eoh],
                                           xDotStart = .data$slopeEOD * constTolerance^((eoh - .data$eod) / yearsUntilConst[2]),
                                           dt = .data$dt - (eoh - .data$eod),
                                           t = .data$period,
                                           tStart = eoh,
                                           epsCorrection = -(eoh - .data$eod) / yearsUntilConst[2]),
             value = ifelse(.data$period > eoh, .data$valueFuture, .data$valueHist)) %>%
      ungroup() %>%
      .manipulateCHN("lowerTail") %>%
      select("region", "period", "levelScen", "item", "value")
  }


  .extrapolateFlows <- function(flowsData) {
    flowsProj <- .projectFlows(flowsData)
    rbind(.expandScenarios(flowsData), flowsProj)
  }


  .createScenMapping <- function(scenario) {
    map <- data.frame(scenario = scenario, levelScen = "med")
    map[map$scenario %in% c("SSP3", "SSP5", "SSP2IndiaHigh"), "levelScen"] <- "high"
    map[map$scenario %in% c("SSP1", "SSP2_lowEn"), "levelScen"] <- "low"
    map
  }


  .mapRemindScens <- function(flows, scenario) {
    .createScenMapping(scenario) %>%
      left_join(flows, by = "levelScen", relationship = "many-to-many") %>%
      select("region", "period", "scenario", "item", "value")
  }



  # CALCULATE ------------------------------------------------------------------

  flowsData <- tibble::as_tibble(.getFlowsIEA())
  flows <- .extrapolateFlows(flowsData)
  flows <- .mapRemindScens(flows, scenario)
  flows <- as.magpie(flows, spatial = "region", temporal = "period")



  # OUTPUT ---------------------------------------------------------------------

  list(x = flows,
       weight = NULL,
       min = 0,
       unit = "EJ/yr",
       description = "Other non-specified energy demand")
}
