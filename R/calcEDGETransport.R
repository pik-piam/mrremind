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

  value <- i.value <- NULL

  if (subtype %in% c("logit_exponent")) {
    conv = FALSE
  }else{
    conv = TRUE
  }

  weightInt <- calcOutput("GDPppp", aggregate = F)
  get_weight <- function(data){
    year_inter = getYears(data)
    ## define weight for intensive entries (for weighted average)
    weightInt <- time_interpolate(
      weightInt,
      year_inter,
      extrapolation_type="constant")[,, getNames(data, dim=1)]
  }

  if(subtype == "pm_trp_demand"){
    fe_dem <- as.data.table(as.quitte(readSource("EDGETransport", "fe_demand_tech")))[
    , c("model", "scenario", "variable", "unit") := NULL
    ]
    fe2es  <- as.data.table(as.quitte(readSource("EDGETransport", "fe2es")))[
    , c("model", "scenario", "variable", "unit") := NULL
    ]
    es_dem <- fe_dem[fe2es, on=c("period", "region", "GDP_scenario", "EDGE_scenario", "all_teEs")][
    , sum(value * i.value), by=c("period", "region", "GDP_scenario", "EDGE_scenario", "all_in")
    ]
    data <- as.magpie(es_dem, spatial=2, temporal=1, datacol=6)
  }else{
    data <- readSource("EDGETransport", subtype, convert = conv)
  }

  switch(subtype,
         "logit_exponent" = {
           weight = NULL
           unit = "[-]"
           description = "Logit exponent values for transport alternatives"
         },
         "harmonized_intensities" = {
           weight = NULL
           unit = "Passenger transport: [EJ/Mpkm]; freight transport: [EJ/Mtkm]"
           description = "Energy intensity for transport modes and fuel, harmonized on IEA balances"
         },
         "value_time" = {
           weight = NULL
           unit = "Passenger transport: [1990$/pkm]; freight transport: [1990$/tkm]"
           description = "Value of time for passenger transport modes"
         },
         "SW" = {
           weight = NULL
           unit = "[-]"
           description = "Share weight, a dimensionless parameter reflecting consumer preferences."
         },
         "price_nonmot" = {
           weight = NULL
           unit = "Passenger transport: [1990$/pkm]; freight transport: [1990$/tkm]"
           description = "Price for non motorized transport modes (Walking; Cycling)"
         },
         "UCD_NEC_iso" = {
           weight = NULL
           unit = "Passenger transport: [1990$/pkm]; freight transport: [1990$/tkm]"
           description = "Non energy costs for all motorized transport modes"
         },
         "esCapCost" = {
           weight = get_weight(data)
           unit = "Passenger transport [2005US$/pkm]; freight transport: [2005US$/tkm]"
           description = "Transport ES demand trajectories for the CES tree."
         },
         "fe2es" = {
           weight = get_weight(data)
           unit = "Passenger transport [trn pkm/Twa], freight transport [trn tkm/Twa]"
           description = "Energy efficiency of CES level nodes for Transport."
         },
         "pm_trp_demand" = {
           weight = NULL
           unit = "Passenger transport [trn pkm], freight transport [trn tkm]"
           description = "CES level transport demand."
         },
         "fe_demand_tech" = {
           weight = get_weight(data)
           unit = "TWa"
           description = "FE demand divided by technologies for different ES on the CES level."
         })

  return(list(x           = data,
              weight      = weight,
              unit        = unit,
              description = description))
}
