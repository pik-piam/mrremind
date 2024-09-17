#' @title calc Shared Target
#' @description provides region specific target
#'
#' @param subtype data subtype. Either "FErenewablesShare", ...
#' @return target data magpie object
#' @author Renato Rodrigues
#' @examples
#'
#' \dontrun{
#' calcOutput("SharedTarget",subtype="FErenewablesShare")
#' }
#'


calcSharedTarget <- function(subtype){

  if (subtype == "FErenewablesShare") {

    description <- "Lower bound on renewables share"
    unit <- "percentage (e.g. 0.2 for 20%)"

    # determine European renewables targets
    EURcountries <-c("ALA","AUT","BEL","BGR","CYP","CZE","DEU","DNK","ESP","EST","FIN","FRA","FRO","GBR","GGY","GIB","GRC","HRV","HUN","IMN","IRL","ITA","JEY","LTU","LUX","LVA","MLT","NLD","POL","PRT","ROU","SVK","SVN","SWE")
    years <- c(seq(2030,2060,5),seq(2070,2110,10), 2130, 2150) # apply targets to REMIND years from 2030 on
    EURTarget <- 0.32 + (years-2030)*0.005 # set renewables target based on the EU Energy and Climate package for 2030, with 2018 Agreement update
    EURTarget[which(EURTarget>0.75)] <- 0.75 # set maximum target to 75%
    names(EURTarget) <- years
    # create magpie object with targets
    out <- new.magpie(cells_and_regions = EURcountries,years = years, names = "FErenewablesShare")
    for (year in years)
      out[,year,] <- EURTarget[as.character(year)]
    out <- toolCountryFill(out, fill = 0, verbosity = 2) # fill other countries with zero as target
    getNames(out) <- NULL
    # weight
    weight <- new.magpie(cells_and_regions = getRegions(out),years = getYears(out), names = getNames(out), fill=1)

  } else {
    stop("Not a valid subtype!")
  }

  #Returning capacity values
  return(list(x=out, weight=weight,
              unit=unit,
              description=description
  ))
}
