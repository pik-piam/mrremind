#' @title Prepare EDGETransport inputs
#'
#' All subtypes should be used with the aggregate=FALSE flag.
#' 
#' @return magpie object of EDGEtransport iterative inputs
#' @author Alois Dirnaichner, Marianna Rottoli
#' @seealso \code{\link{readSource}}
#' @param subtype refer to the code for a list of allowed subtypes.
#'
#' @examples
#' \dontrun{ a <- calcOutput(type="EDGETransport", subtype="logit_exponent", aggregate=F)
#' }
#' 
calcEDGETransport <- function(subtype = "logit_exponent") {
  if (subtype %in% c("logit_exponent")) {
    data <- readSource("EDGETransport", subtype = "logit_exponent", convert = F)
  }else{
    data <- readSource("EDGETransport", subtype)    
  }
  year_inter = getYears(data)
  ## define weight for intensive entries (for weighted average)
  weightInt <- calcOutput("GDPppp", aggregate = F)[,, "gdp_SSP2"]
  weightInt <- time_interpolate(weightInt,year_inter,extrapolation_type="constant",integrate_interpolated_years=TRUE)[, getYears(data),]

  ## define weight for extensive entries (for summation)
  weightExt <- NULL
  
  
  switch(subtype,
         "logit_exponent" = {
           weight = NULL
           unit = "[-]"
           description = "Logit exponent values for transport alternatives"
         },
         "harmonized_intensities" = {
           weight = weightInt
           unit = "Passenger transport: [EJ/Mpkm]; freight transport: [EJ/Mtkm]"
           description = "Energy intensity for transport modes and fuel, harmonized on IEA balances"
         },
         "value_time" = {
           weight = weightInt
           unit = "Passenger transport: [1990$/pkm]; freight transport: [1990$/tkm]"
           description = "Value of time for passenger transport modes"
         },
         "SW" = {
           weight = weightInt
           unit = "[-]"
           description = "Share weight, a dimensionless parameter reflecting consumer preferences."
         },
         "price_nonmot" = {
           weight = weightInt
           unit = "Passenger transport: [1990$/pkm]; freight transport: [1990$/tkm]"
           description = "Price for non motorized transport modes (Walking; Cycling)"
         },
         "UCD_NEC_iso" = {
           weight = weightInt
           unit = "Passenger transport: [1990$/pkm]; freight transport: [1990$/tkm]"
           description = "Non energy costs for all motorized transport modes"
         },
         "pm_trp_demand" = {
           weight = weightExt
           unit = "Passenger transport: [trn pkm]; freight transport: [trn tkm]"
           description = "Transport ES demand trajectories for the CES tree."
         },
         "esCapCost" = {
           weight = weightInt
           unit = "Passenger transport [2005US$/pkm]; freight transport: [2005US$/tkm]"
           description = "Transport ES demand trajectories for the CES tree."
         },
         "fe2es" = {
           weight = weightInt
           unit = "Passenger transport [trn pkm/Twa], freight transport [trn tkm/Twa]"
           description = "Energy efficiency of CES level nodes for Transport."
         },
         "demByTech" = {
           weight = weightExt
           description = "FE demand divided by technologies for different ES on the CES level."
           unit = "[-]"
         })
  
  return(list(x           = data,
              weight      = weight, 
              unit        = unit, 
              description = description))
}
