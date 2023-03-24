#' @title calcEDGAR7Fgases
#' @author Gabriel Abrahao
#' @param x  magpie object to be converted
#'
#' @importFrom stringr str_remove
#' @export

calcEDGAR7Fgases <- function() {
    # F-gases emissions
    xout <- readSource("EDGAR7Fgases")

    # Write units more or less automatically from the mapping names
    outunits <- paste0("kt ",gsub("emiFgas","",getNames(xout)),"/yr")
    outunits[1] <- "Mt CO2eq/yr"

    return(list(
        x = xout,
        weight = NULL,
        unit = outunits,
        description = "F-gas emissions from EDGAR7"
    ))
}
