#' @title calc Capacity Factor
#' @description provides capacity factor values
#'
#' @return magpie object of the capacity factor data
#' @author Lavinia Baumstark
#' @examples
#' \dontrun{
#' calcOutput("StorageFactor")
#' }
#'
calcStorageFactor <- function() {
  # Read storage factor inputs
  x <- readSource("REMIND_11Regi", subtype = "storageFactor")
  getSets(x)[3] <- "all_te"

  # read in weight data
  # for solar
  tmp <- calcOutput("Solar", aggregate = FALSE)
  weightSolar <- dimSums(tmp[, , "area"][, , "PV"][, , c("0-50", "50-100")], dim = c(3.4, 3.3))
  weightSolar <- collapseNames(weightSolar)
  getNames(weightSolar) <- c("spv")
  weightSolar <- mbind(weightSolar, setNames(weightSolar, "csp"))
  # for wind onshore
  weightOnshore <- calcOutput("PotentialWindOn", aggregate = FALSE)
  weightOnshore <- collapseNames(dimSums(weightOnshore, dim = 3))
  getSets(weightOnshore)[1] <- getSets(weightSolar)[1]
  getSets(weightOnshore)[2] <- getSets(weightSolar)[2]
  getNames(weightOnshore) <- "windon"
  getYears(weightOnshore) <- getYears(weightSolar)
  # for wind onshore
  weightOffshore <- calcOutput("PotentialWindOff", aggregate = FALSE)
  weightOffshore <- collapseNames(dimSums(weightOffshore, dim = 3))
  getSets(weightOffshore)[1] <- getSets(weightSolar)[1]
  getSets(weightOffshore)[2] <- getSets(weightSolar)[2]
  getNames(weightOffshore) <- "windoff"
  getYears(weightOffshore) <- getYears(weightSolar)
  # combile all weights
  w <- mbind(weightSolar, weightOnshore, weightOffshore)
  getSets(w)[3] <- "all_te"

  return(list(
    x = x,
    weight = w,
    unit = "% of capacity",
    description = paste0("multiplicative factor that scales total curtailment and ",
                         "storage requirements up or down in different regions for ",
                         "different technologies")
  ))
}
