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

  # read storage factor inputs
  x <- readSource("ExpertGuess", subtype = "storageFactor")
  getSets(x)[3] <- "all_te"

  # read in weight data

  ## for solar and csp
  tmp <- calcOutput("Solar", aggregate = FALSE)
  weightSolar <- dimSums(tmp[, , "area"][, , "PV"][, , c("0-50", "50-100")], dim = c(3.4, 3.3))
  weightSolar <- collapseNames(weightSolar)
  getNames(weightSolar) <- c("spv")
  weightSolar <- mbind(weightSolar, setNames(weightSolar, "csp"))

  ## for wind onshore
  weightOnshore <- calcOutput("PotentialWindOn", aggregate = FALSE)
  weightOnshore <- collapseNames(dimSums(weightOnshore, dim = 3))
  getSets(weightOnshore)[1] <- getSets(weightSolar)[1]
  getSets(weightOnshore)[2] <- getSets(weightSolar)[2]
  getNames(weightOnshore) <- "windon"
  getYears(weightOnshore) <- getYears(weightSolar)

  ## for wind onshore
  weightOffshore <- calcOutput("PotentialWindOff", aggregate = FALSE)
  weightOffshore <- collapseNames(dimSums(weightOffshore, dim = 3))
  getSets(weightOffshore)[1] <- getSets(weightSolar)[1]
  getSets(weightOffshore)[2] <- getSets(weightSolar)[2]
  getNames(weightOffshore) <- "windoff"
  getYears(weightOffshore) <- getYears(weightSolar)

  ## combine all weights
  w <- mbind(weightSolar, weightOnshore, weightOffshore)
  getSets(w)[3] <- "all_te"

  return(list(
    x = x,
    weight = w,
    unit = "% of capacity",
    description = glue::glue(
      'Multiplicative factor that scales total curtailment and storage requirements \\
      up or down in different regions for different technologies.  \\
      The regional storage parameterization is based on the regional data from this paper: \\
      "Ueckerdt, F., Pietzcker, R., Scholz, Y., Stetter, D., Giannousakis, A., Luderer, G., 2017. \\
      Decarbonizing global power supply under region-specific consideration of challenges and \\
      options of integrating variable renewables in the REMIND model. Energy Economics 64, 665-684. \\
      https://doi.org/10.1016/j.eneco.2016.05.012" and - for EU regions - a further differentiation \\
      based on "how much are storage needs reduced because solar aligns well with cooling demands", \\
      which is proxied by "how far south does a region lie".')
  ))
}
