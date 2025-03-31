#' Calculate LDV Shares using EDGE-Transport
#'
#' @author Johanna Hoppe, Falk Benke
calcLDVShares <- function() {

  # only the first EDGE-T scenario for SSP2 is used as a proxy for the LDV shares
  edgeResults <- calcOutput(
    type = "EdgeTransportSA",
    aggregate = FALSE,
    supplementary = FALSE,
    SSPscen = "SSP2",
    transportPolScen = "Mix1",
    isICEban = FALSE,
    demScen = "default",
    isTransportReported = FALSE,
    isREMINDinputReported = TRUE,
    isStored = FALSE
  )

  x <- as.magpie(edgeResults[["shares_LDV_transport"]]) %>%
    collapseDim(keepdim = 3.4)

  for (year in getYears(x, as.integer = TRUE)) {
    x[, year, ] <- as.vector(x[, c(2010), ]) + ((0.55 - as.vector(x[, c(2010), ])) / (2100 - 2010)) * (year - 2010)
  }

  # extending years via interpolation

  x <- time_interpolate(x,
    integrate_interpolated_years = TRUE,
    interpolated_year = seq(from = 1990, to = 2100),
    extrapolation_type = "linear"
  )

  x <- time_interpolate(x,
    integrate_interpolated_years = TRUE,
    interpolated_year = c(seq(from = 1970, to = 1989), seq(from = 2101, to = 2150)),
    extrapolation_type = "constant"
  )

  return(list(x = x, weight = NULL, unit = "unitless",
              description = "LDV shares provided by EDGE-Transport"))

}
