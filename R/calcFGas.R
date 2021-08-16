#' generate F-Gases based on IMAGE data
#'
#' @return magpie object with F-gases information
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readSource}}
#' @examples
#' \dontrun{
#' x <- calcOutput("FGas")
#' }
#' @importFrom magclass getNames<- getSets clean_magpie

calcFGas <- function() {
  # read in IMAGE data
  x <- readSource("IMAGE")
  # make the dimension needed for REMIND
  getNames(x, dim = 1) <- gsub("-", ".", getNames(x, dim = 1))
  x <- clean_magpie(x, what = "sets")
  x <- collapseNames(x)
  getSets(x)[2] <- "Year"
  getSets(x)[4] <- "rcp"
  getSets(x)[5] <- "SPAscen"
  getSets(x)[6] <- "Variable"
  getSets(x)[7] <- "Unit"

  # change names to the REMIND-set-names
  getNames(x, dim = 1) <- paste0("forcing_", getNames(x, dim = 1))
  getNames(x, dim = 2) <- gsub("34", "37", getNames(x, dim = 2))
  getNames(x, dim = 2) <- paste0("rcp", getNames(x, dim = 2))
  getNames(x, dim = 2) <- gsub("rcpRef", "none", getNames(x, dim = 2))
  getNames(x, dim = 3) <- gsub("[1-9]", "x", getNames(x, dim = 3))

  xCO2 <- x[, , "Emissions|CO2|Fossil Fuels and Industry"]
  x <- x[, , "Emissions|CO2|Fossil Fuels and Industry", invert = TRUE]

  emi_mapping <- list("Emissions|F-Gases"      = "emiFgasTotal",
                      "Emissions|HFC"          = "emiFgasHFC",
                      "Emissions|HFC|HFC125"   = "emiFgasHFC125",
                      "Emissions|HFC|HFC134a"  = "emiFgasHFC134a",
                      "Emissions|HFC|HFC143a"  = "emiFgasHFC143a",
                      "Emissions|HFC|HFC227ea" = "emiFgasHFC227ea",
                      "Emissions|HFC|HFC23"    = "emiFgasHFC23",
                      "Emissions|HFC|HFC245fa" = "emiFgasHFC245fa",
                      "Emissions|HFC|HFC32"    = "emiFgasHFC32",
                      "Emissions|HFC|HFC43-10" = "emiFgasHFC43-10",
                      "Emissions|PFC"          = "emiFgasPFC",
                      "Emissions|SF6"          = "emiFgasSF6",
                      "Emissions|C2F6"         = "emiFgasC2F6",
                      "Emissions|C6F14"        = "emiFgasC6F14",
                      "Emissions|CF4"          = "emiFgasCF4")
  getNames(x, dim = 4) <- emi_mapping[getNames(x, dim = 4)]

  # remove the unit dimension
  x <- collapseNames(x, collapsedim = 5)
  xCO2 <- collapseNames(xCO2, collapsedim = 4)

    # To calculate SSP4 and SSP5 policy emissions we scale SSP2 policy F-gas emissions
  # with SSP5 F-gas baselines and correct for the ratio of CO2 baseline
  # emissions which is a proxy for the CO2 mitigation effort and therefore
  # for the CO2 price.
  # Fgas_SSP5_pol = Fgas_SSP2_pol *
  # (Fgas_SSP5_ref / Fgas_SSP2_ref) *
  # (CO2_SSP5_ref / CO2_SSP2_ref)

  x_SSP5 <- new.magpie(getRegions(x), getYears(x), getNames(x[, , "forcing_SSP2"]))
  getNames(x_SSP5) <- gsub("forcing_SSP2", "forcing_SSP5", getNames(x_SSP5))
  x_SSP5 <- x_SSP5[, , "none", invert = TRUE]

  # loop over rcp scenarios
  for (r in getNames(x_SSP5, dim = 2)) {

    x_SSP5[, , "forcing_SSP5"][, , r] <- collapseNames(x[, , "forcing_SSP2"])[, , r] *
      (collapseNames(x[, , "forcing_SSP5"][, , "none"]) / collapseNames(x[, , "forcing_SSP2"][, , "none"])) *
      (collapseNames(xCO2[, , "forcing_SSP5"][, , "none"]) / collapseNames(xCO2[, , "forcing_SSP2"][, , "none"]))
  }
  x <- mbind(x, x_SSP5)
  x[is.na(x)] <- 0

  # interpolate time to match REMIND years
  rem_years <- c(seq(2015, 2105, 10), seq(2110, 2150, 5))
  rem_years <- seq(2005, 2150, 5)
  x <- time_interpolate(x, interpolated_year = rem_years, extrapolation_type = "constant")

  # add data for rcp 2.0, use data from rcp 2.6
  x_20 <- x[, , "rcp26"]
  getNames(x_20) <- gsub("rcp26", "rcp20", getNames(x_20))
  x <- mbind(x, x_20)

  return(list(x           = x,
              weight      = NULL,
              unit        = "Mt CO2-equiv/yr, kt HFC134a-equiv/yr, kt HFC125/yr, kt HFC134a/yr, kt HFC143a/yr, kt HFC227ea/yr, kt HFC23/yr, kt HFC245fa/yr, kt HFC32/yr, kt HFC43-10/yr, kt CF4-equiv/yr, kt SF6/yr, kt C2F6/yr, kt C6F14/yr, kt CF4/yr",
              description = "F-gases from IMAGE"))
}
