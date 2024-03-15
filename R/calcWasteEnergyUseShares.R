#' Calculate waste energy use shares based on IEA World Energy Balances
#'
#' @author Robert Pietzcker, Falk Benke
#' @param mapping an optional mapping for regional (dis)aggregration, if not provided,
#' "regionmapping_21_EU11.csv" is used
#'
calcWasteEnergyUseShares <- function(mapping = NULL) {

  data <- readSource("IEA", subtype = "EnergyBalances")

  data <- dimSums(data[, , c("INDWASTE", "MUNWASTEN")], dim = 3.1, na.rm = TRUE)

  if (is.null(mapping)) {
    mapping <- toolGetMapping("regionmapping_21_EU11.csv",
      where = "mappingfolder", type = "regional"
    )
  }
  x <- toolAggregate(data, rel = mapping)

  out <- NULL

  out <- mbind(out, setNames(
    x[, , "TFC"] / (x[, , "TFC"] - x[, , "TOTTRANF"]),
    "Waste for FE|Share in Waste"
  ))

  out <- mbind(out, setNames(
    -x[, , "TOTTRANF"] / (x[, , "TFC"] - x[, , "TOTTRANF"]),
    "Waste for Energy Supply|Share in Waste"
  ))

  out <- mbind(out, setNames(
    x[, , "TOTIND"] / x[, , "TFC"],
    "Waste for Industry|Share in Waste for FE"
  ))

  out <- mbind(out, setNames(
    x[, , "COMMPUB"] / x[, , "TFC"],
    "Waste for CommPub|Share in Waste for FE"
  ))

  # the factor 2 represents the lower conversion efficiency of heat to electricity
  out <- mbind(out, setNames(
    2 * x[, , "ELOUTPUT"] / (2 * x[, , "ELOUTPUT"] + x[, , "HEATOUT"]),
    "Waste for Electricity|Share in Waste for Energy Supply"
  ))

  out <- mbind(out, setNames(
    x[, , "HEATOUT"] / (2 * x[, , "ELOUTPUT"] + x[, , "HEATOUT"]),
    "Waste for Heat|Share in Waste for Energy Supply"
  ))

  out <- toolAggregate(out, rel = mapping, from = "RegionCode", to = "CountryCode")
  weight <- out
  weight[] <- 1

  return(list(
    x = out,
    weight = weight,
    unit = "%",
    mixed_aggregation = TRUE,
    description = "Waste Energy Use Shares based on IEA World Energy Balances"
  ))
}
