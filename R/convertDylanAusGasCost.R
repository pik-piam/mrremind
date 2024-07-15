#' Converts Dylan's Australian gas cost to magpie
#' @param x MAgPIE object to be converted
#' @return magpie object of the CEMO data
#' @author Felix Schreyer
#' @seealso \code{\link{readSource}}

convertDylanAusGasCost <- function(x) {

  x %>%
    # Converting from constant 2015 Australian dollars to constant 2017 Int$PPP
    GDPuc::convertGDP(unit_in = "constant 2015 LCU", unit_out = "constant 2017 Int$PPP") %>%
    add_dimension(dim = 3.4, add = "unit", nm = "Natural Gas Extraction Cost [2017USD/GJ]") %>%
    toolCountryFill(fill = 0) %>%
    return()

}
