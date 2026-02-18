#' Calculate geological storage potential
#'
#' Provides geological storage potential
#'
#'
#' @return geological storage potential data MAgPIE object
#' @author David Klein
#' @examples
#'
#' \dontrun{
#' calcOutput("PotentialGeologicalStorage")
#' }
#'

calcPotentialGeologicalStorage <- function(source) {

  #if (source == "Gidden2025") {
    # read geological storage potential from Gidden et al. 2025
    new <- readSource(type = "Gidden2025_geological_storage_potential")
    new <- new * 12/44 # GtCO2 -> GtC

  #} else if (source == "MixedOld") {
    # move content of calcLimitCCS.R here and delete it
    # Read capacity factor inputs
    data <- readSource("REMIND_11Regi", subtype = "ccs")

    # add dimensions that are the same for all regions but a dimension of the parameter in the GAMS code
    getNames(data) <- "mixedOld"
    #data <- add_dimension(data, dim = 3.2, add = "rlf", nm = "1")

    # overwrite values for regions that belong to EUR and NEU
    data_ccs_storage_potential <- readSource("CCS_StoragePotential")
    eur.34 <- c("ALA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FRO",
                "FIN", "FRA", "DEU", "GIB", "GRC", "GGY", "HUN", "IRL", "IMN", "ITA",
                "JEY", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK",
                "SVN", "ESP", "SWE", "GBR")
    non.eu <- c("ALB", "AND", "BIH", "GRL", "VAT", "ISL", "LIE", "MKD", "MCO", "MNE",
                "NOR", "SMR", "SRB", "SJM", "CHE", "TUR")
    overwrite <- c(eur.34, non.eu)
    data[overwrite, , ] <- data_ccs_storage_potential[overwrite, , ]
    old <- data

  #} else {
  #  stop("'", source, "' is an unknown source for calcPotentialGeologicalStorage")
  #}

  pot <- mbind(new, old)

  return(list(x                 = pot,
              weight            = NULL,
              unit              = "GtC",
              description       = "Geological Storage Potential"
  ))
}
