#' @title calc Capacity Factor
#' @description provides capacity factor values
#'
#' @return magpie object of the capacity factor data
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}
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
  w1_tmp <- calcOutput("Solar", aggregate = FALSE)
  w1 <- dimSums(w1_tmp[, , "area"][, , "PV"][, , c("0-50", "50-100")], dim = c(3.4, 3.3))
  w1 <- collapseNames(w1)
  getNames(w1) <- c("spv")
  w1 <- mbind(w1, setNames(w1, "csp"))
  # for wind
  w2 <- calcOutput("PotentialWindOn", aggregate = FALSE)
  w2 <- collapseNames(dimSums(w2, dim = 3))
  getSets(w2)[1] <- getSets(w1)[1]
  getSets(w2)[2] <- getSets(w1)[2]
  getNames(w2) <- "wind"
  getYears(w2) <- getYears(w1)
  # combile all weights
  w <- mbind(w1, w2)

  return(list(
    x = x,
    weight = w,
    unit = "% of capacity",
    description = paste0("multiplicative factor that scales total curtailment and ",
                         "storage requirements up or down in different regions for ",
                         "different technologies")
  ))
}
