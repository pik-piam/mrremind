#' Calculate waste energy use shares based on IEA World Energy Balances
#'
#' The output of this function is used in remind2 for reporting purposes.
#'
#' @author Robert Pietzcker, Falk Benke
#'
calcWasteEnergyUseShares <- function() {
  x <- readSource("IEA", subtype = "EnergyBalances")
  x <- dimSums(x[, , c("INDWASTE", "MUNWASTEN")], dim = 3.1, na.rm = TRUE)

  numerator <- NULL
  denominator <- NULL

  numerator <- mbind(numerator, setNames(
    x[, , "TFC"],
    "Waste for FE|Share in Waste"
  ))

  denominator <- mbind(denominator, setNames(
    x[, , "TFC"] - x[, , "TOTTRANF"],
    "Waste for FE|Share in Waste"
  ))

  numerator <- mbind(numerator, setNames(
    -x[, , "TOTTRANF"],
    "Waste for Energy Supply|Share in Waste"
  ))

  denominator <- mbind(denominator, setNames(
    x[, , "TFC"] - x[, , "TOTTRANF"],
    "Waste for Energy Supply|Share in Waste"
  ))

  numerator <- mbind(numerator, setNames(
    x[, , "TOTIND"],
    "Waste for Industry|Share in Waste for FE"
  ))

  denominator <- mbind(denominator, setNames(
    x[, , "TFC"],
    "Waste for Industry|Share in Waste for FE"
  ))

  numerator <- mbind(numerator, setNames(
    x[, , "COMMPUB"],
    "Waste for CommPub|Share in Waste for FE"
  ))

  denominator <- mbind(denominator, setNames(
    x[, , "TFC"],
    "Waste for CommPub|Share in Waste for FE"
  ))

  # the factor 2 represents the lower conversion efficiency of heat to electricity
  numerator <- mbind(numerator, setNames(
    2 * x[, , "ELOUTPUT"],
    "Waste for Electricity|Share in Waste for Energy Supply"
  ))

  denominator <- mbind(denominator, setNames(
    2 * x[, , "ELOUTPUT"] + x[, , "HEATOUT"],
    "Waste for Electricity|Share in Waste for Energy Supply"
  ))

  numerator <- mbind(numerator, setNames(
    x[, , "HEATOUT"],
    "Waste for Heat|Share in Waste for Energy Supply"
  ))

  denominator <- mbind(denominator, setNames(
    2 * x[, , "ELOUTPUT"] + x[, , "HEATOUT"],
    "Waste for Heat|Share in Waste for Energy Supply"
  ))

  .calcShares <- function(x, rel, denominator) {
    return(
      toolAggregate(x, rel = rel) /
        toolAggregate(denominator, rel = rel)
    )
  }

  return(list(
    x = numerator,
    weight = NULL,
    unit = "%",
    aggregationFunction = .calcShares,
    aggregationArguments = list(denominator = denominator),
    min = 0, max = 1,
    description = "Waste Energy Use Shares based on IEA World Energy Balances"
  ))
}
