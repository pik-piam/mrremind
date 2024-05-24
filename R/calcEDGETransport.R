#' @title Prepare EDGETransport inputs
#'
#' @author Johanna Hoppe
#' @seealso \code{\link{readSource}}
#' @param subtype  REMIND/iterative EDGE-T input data subtypes
#' @return REMIND/iterative EDGE-T input data for all scenario combinations
#' @examples
#' \dontrun{ a <- calcOutput(type = "EDGETransport", subtype = "CAPEXandNonFuelOPEX", aggregate = F)
#' }

calcEDGETransport <- function(subtype) {

  gdp <- calcOutput("GDP", aggregate = FALSE)[,,"gdp_SSP2"]
  x <- readSource("EDGETransport", subtype)

  switch(subtype,
         "p35_esCapCost" = {
           weight = gdp |> time_interpolate(getYears(x))
           unit = "2005US$/(p|t)km"
           description = "Capital cost (purchase) per energy service demand on CES level."
         },
         "p35_fe2es" = {
           weight = gdp |> time_interpolate(getYears(x))
           unit = "trn (p|t)km/Twa"
           description = "Energy efficiency on CES level."
         },
         "p35_demByTech" = {
           weight = NULL
           unit = "TWa"
           description = "Final energy demand on CES level."
         },
         "CAPEXandNonFuelOPEX" = {
           weight = gdp |> time_interpolate(getYears(x))
           unit = "2005US$/(p|t)km"
           description = "Capital cost (purchase) and non-fuel operational costs on technology level."
         },
         "scenSpecPrefTrends" = {
           weight = gdp |> time_interpolate(getYears(x))
           unit = "-"
           description = "Scenario specific preference trends on technology level."
         },
         "scenSpecLoadFactor" = {
           weight = gdp |> time_interpolate(getYears(x))
           unit = "-"
           description = "Scenario specific load factor on technology level."
         },
         "scenSpecEnIntensity" = {
           weight = gdp |> time_interpolate(getYears(x))
           unit = "MJ/vehkm"
           description = "Scenario specific energy intensity on technology level."
         },
         "initialIncoCosts" = {
           weight = gdp |> time_interpolate(getYears(x))
           unit = "2005US$/(p|t)km"
           description = "Initial inconvenience cost values."
         },
         "annualMileage" = {
           weight = gdp |> time_interpolate(getYears(x))
           unit = "vehkm/yr"
           description = "Annual vehicle km traveled."
         },
         "timeValueCosts" = {
           weight = gdp |> time_interpolate(getYears(x))
           unit = "2005US$/(p|t)km"
           description = "Value of time cost equivalent."
         }
       )

  return(list(x           = x,
              weight      = weight,
              unit        = unit,
              description = description))
}
