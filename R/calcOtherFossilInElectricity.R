#' Calculate projected electricity from waste and other fossils
#' using energy demands from IEA Energy Balances.
#'
#' This is used in remind2 reporting as input data to calculate additional
#' capacity and secondary energy variables.
#'
#' The projection focuses on a tight mitigation scenario and assumes that all
#' fossil emissions from waste burning / other fossil processes can be reduced
#' to 0 by 2050. Should be replaced in the future by actual modeling of waste /
#' other fossil plants, or at least connected to RCP scenario assumptions.
#'
#' @author Robert Pietzcker, Falk Benke
#'
calcOtherFossilInElectricity <- function() {
  # read in data and convert from ktoe to EJ
  data <- readSource("IEA", subtype = "EnergyBalances") * 4.1868e-5

  el <- c("ELAUTOC", "ELAUTOE", "ELMAINC", "ELMAINE")

  waste <- dimSums(data[, , el][, , c("INDWASTE", "MUNWASTEN")],
    dim = 3, na.rm = TRUE
  )
  getItems(waste, dim = 3) <- "SE|Electricity|Other Fossil|Waste|Projected (EJ/yr)"

  other <- dimSums(data[, , el][, , c(
    "GASWKSGS", "COKEOVGS", "BLFURGS", "OGASES",
    "REFINGAS"
  )], dim = 3, na.rm = TRUE) + waste
  getItems(other, dim = 3) <- "SE|Electricity|Other Fossil|Projected (EJ/yr)"

  d <- mbind(waste, other)
  d <- d[, 2021, , invert = TRUE]

  y <- c(seq(2005, 2060, 5), seq(2070, 2100, 10), seq(2110, 2150, 20))

  x.fill <- new.magpie(
    cells_and_regions = getItems(d, dim = 1),
    years = union(getYears(d, as.integer = TRUE), y),
    names = getNames(d),
    fill = 0
  ) %>% magpiesort()

  x.fill[, getYears(d), ] <- d
  x.fill[, c(2025, 2030), ] <- d[, 2020, ]
  x.interpolated <- time_interpolate(x.fill[, c(2030, 2050), ], c(2035, 2040, 2045),
                                     integrate_interpolated_years = FALSE)
  x.fill[, c(2035, 2040, 2045)] <- x.interpolated

  return(
    list(
      x = x.fill,
      weight = NULL,
      unit = "EJ",
      description = "Projected Electricity from Waste and Other Fossils"
    )
  )

}
