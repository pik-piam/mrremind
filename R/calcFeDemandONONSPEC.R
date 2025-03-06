#' Calculate historic and projected other non-specified energy demand
#'
#' Project the IEA flow ONONSPEC into the future.
#'
#' @author Robin Hasse
#'
#' @returns list with MagPIE object
#'
#' @importFrom madrat toolGetMapping calcOutput
#' @importFrom magclass getItems getSets<-
#' @importFrom dplyr %>% .data filter group_by across all_of group_modify
#'   slice_tail ungroup reframe

calcFeDemandONONSPEC <- function() {

  # project yearly until
  endOfProjection <- 2100

  # approach constant level after ... years
  yearsUntilConst <- c(15, 20, 25)

  # ratio of EOH slope that qualifies as constant
  constTolerance <- 0.05




  # FUNCTIONS ------------------------------------------------------------------

  .getHistFlows <- function() {
    data <- calcOutput("IOEdgeBuildings",
                       subtype = "output_EDGE",
                       aggregate = FALSE)
    getSets(data) <- c("region", "period", "flow")
    # ONONSPEC have been mapped to feoth* before
    itemsONONSPEC <- grep("^feoth.+$", getItems(data, 3), value = TRUE)
    data[, , itemsONONSPEC]
  }


  .getEOHcoefs <- function(x, key) {
    m <- lm(value~period, x)
    periodEOH <- max(x$period)
    coefs <- data.frame(periodEOH = periodEOH,
                        slopeEOH = coef(m)[["period"]],
                        valueEOH = predict(m, list(period = periodEOH)))
  }


  .expandScenarios <- function(x, key = NULL, .yearsUntilConst = NULL) {
    scens <- data.frame(scenario = c("low", "med", "high"))
    if (!is.null(.yearsUntilConst)) {
      scens$.yearsUntilConst = if (x$slopeEOH > 0) .yearsUntilConst else rev(.yearsUntilConst)
    }
    merge(scens, x)
  }


  .expandPeriods <- function(x, key, endOfProjection) {
    startOfProjection <- unique(x$periodEOH) + 1
    periods <- data.frame(period = startOfProjection:endOfProjection)
    merge(periods, x)
  }


  .calcAsymptotic <- function(x) {
    decay <- -log(constTolerance) / x$.yearsUntilConst
    x$value <- pmax(0,
      x$valueEOH + x$slopeEOH / decay * (1 - exp(-decay * (x$period - x$periodEOH)))
    )
    x
  }


  .projectFlows <- function(flows, endOfProjection, nTimeStepsEOH = 5) {
    flows %>%
      filter(!is.na(.data$value)) %>%
      group_by(across(all_of(c("region", "flow")))) %>%
      arrange(.data$period) %>%
      slice_tail(n = nTimeStepsEOH) %>%
      group_modify(.getEOHcoefs) %>%
      group_modify(.expandScenarios, yearsUntilConst) %>%
      group_modify(.expandPeriods, endOfProjection) %>%
      ungroup() %>%
      .calcAsymptotic() %>%
      select("region", "period", "scenario", "flow", "value")
  }


  .extrapolateFlows <- function(flowsHist, endOfProjection) {
    flowsProj <- .projectFlows(flowsHist, endOfProjection)
    rbind(.expandScenarios(flowsHist), flowsProj)
  }



  # CALCULATE ------------------------------------------------------------------

  flowsHist <- as_tibble(.getHistFlows())
  flows <- .extrapolateFlows(flowsHist, endOfProjection)
  flows <- as.magpie(flows, spatial = "region", temporal = "period")



  # OUTPUT ---------------------------------------------------------------------

  list(x = flows,
       weight = NULL,
       min = 0,
       unit = "EJ/yr",
       description = "Other non-specified energy demand")
}
