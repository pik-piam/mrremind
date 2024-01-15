#' Aggregate values to n-year averages to suppress volatility
#'
#' @param x a magclass object
#' @param nYears time steps to be used for averaging, defaults to 5
#' @author Robin Hasse
#' @returns magclass object with averages
#' @export
toolAggregateTimeSteps <- function(x, nYears = 5) {

  periods <- sort(getYears(x, as.integer = TRUE))

  # periods in n steps
  periodsTarget <- min(periods):max(periods)
  periodsTarget <- periodsTarget[periodsTarget %% nYears == 0]

  periodsMissing <- setdiff(periodsTarget, periods)

  # periods with difference smaller or greater to next period
  periodsSubN <- sort(union(head(periods, -1)[diff(periods) != nYears],
                            tail(periods, -1)[diff(periods) != nYears]))

  # periods that need to be aggregated
  periodsFill <- intersect(periodsTarget, union(periodsSubN, periodsMissing))

  # periods that can be left as is
  periodsKeep <- setdiff(periodsTarget, periodsFill)

  # buffer around periods to be aggregated
  periodsBuffer <- unique(do.call(c, lapply(periodsFill, function(y) {
    (y - round(nYears / 2 - 0.1)):(y + round(nYears / 2 - 0.1))
  })))

  # fill x with extrapolated values for buffer years
  xBuffer <- time_interpolate(x, periodsBuffer)

  # create mapping table describing how to aggregate years
  rel <- expand.grid(period = periodsBuffer, periodAgg = periodsFill)
  rel <- rel[abs(rel$period - rel$periodAgg) <= round(nYears / 2 - 0.1), ]
  rel$w <- ifelse(abs(rel$period - rel$periodAgg) < round(nYears / 2 + 0.1), 1, 0.5)

  # create magpie object with weights
  w <- xBuffer

  for (y in periodsBuffer) {
    w[, y, ] <- unique(rel[rel$period == y, "w"])
  }

  rel$period <- paste0("y", rel$period)
  rel$periodAgg <- paste0("y", rel$periodAgg)

  xAgg <- toolAggregate(xBuffer, rel = rel, weight = w,
                        from = "period", to = "periodAgg", dim = 2)

  return(mbind(xAgg, x[, periodsKeep, ]))
}
