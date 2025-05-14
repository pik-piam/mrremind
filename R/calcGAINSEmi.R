#' Calculate air pollution emissions and emission factors from GAINS data
#'
#' Provides input data for exoGAINSAirpollutants.R
#'
#' @return Emissions and emission factors
#' @author Sebastian Rauner
#' @param subtype "emission_factors", "emissions","emissions_starting_values"
#'
#' @examples
#' \dontrun{
#' calcOutput("GAINSEmi")
#' }
calcGAINSEmi <- function(subtype = "emissions") {

  if (!(subtype %in% c("emission_factors", "emissions", "emissions_starting_values"))) stop('subtype must be in c("emission_factors", "emissions","emissions_starting_values")')

  if (subtype == "emissions") {
    emi_gains_ext_in <- calcOutput("GAINS", subtype = "emissions", sectoral_resolution = "extended", aggregate = FALSE) # in Mt
    emi_gains_agg_in <- calcOutput("GAINS", subtype = "emissions", sectoral_resolution = "aggregated", aggregate = FALSE) # in Mt


    # mainly: combine extended and aggregated GAINS data by appending
    # waste sectors from extended to aggregated and then using
    # aggregated only

    # construct mappings: add waste from extended mapping to aggregated mapping
    map_mix <- toolGetMapping(type = "sectoral", name = "mappingGAINSmixedtoREMIND17activities.csv",
                              where = "mappingfolder")
    map_waste <- map_mix[map_mix$GAINS %in% c("Waste_Solid_Industrial", "Waste_Solid_Municipal", "Waste_Water_Industrial", "Waste_Water_Municipal"), ]

    # append waste ef and emi from extended to aggregated
    emi_gains <- mbind(emi_gains_agg_in, emi_gains_ext_in[, , map_waste$GAINS])

    # many of the 2005 values seem to be outliers
    emi_gains[, 2005, ] <- setYears(emi_gains[, 2010, ])

    x <- emi_gains
    unit <- "Mt"
    description <- "Emissions from GAINS for all CEDS setors execpt aviation and shipping"
    weight <- NULL
    aggregationArguments <- NULL

  } else if (subtype == "emission_factors") {
    ef_gains_ext_in  <- calcOutput("GAINS", subtype = "emission_factors", sectoral_resolution = "extended", aggregate = FALSE) # in Tg/TWa
    ef_gains_agg_in  <- calcOutput("GAINS", subtype = "emission_factors", sectoral_resolution = "aggregated", aggregate = FALSE) # in Tg/TWa
    emi_gains_ext_in <- calcOutput("GAINS", subtype = "emissions", sectoral_resolution = "extended", aggregate = FALSE) # in Mt
    emi_gains_agg_in <- calcOutput("GAINS", subtype = "emissions", sectoral_resolution = "aggregated", aggregate = FALSE) # in Mt

    # mainly: combine extended and aggregated GAINS data by appending
    # waste sectors from extended to aggregated and then using
    # aggregated only

    # construct mappings: add waste from extended mapping to aggregated mapping
    map_mix <- toolGetMapping(type = "sectoral", name = "mappingGAINSmixedtoREMIND17activities.csv",
                              where = "mappingfolder")
    map_waste <- map_mix[map_mix$GAINS %in% c("Waste_Solid_Industrial", "Waste_Solid_Municipal", "Waste_Water_Industrial", "Waste_Water_Municipal"), ]

    # append waste ef and emi from extended to aggregated
    ef_gains  <- mbind(ef_gains_agg_in,  ef_gains_ext_in[, , map_waste$GAINS])
    emi_gains <- mbind(emi_gains_agg_in, emi_gains_ext_in[, , map_waste$GAINS])

    # many of the 2005 values seem to be outliers
    ef_gains[, 2005, ]  <- setYears(ef_gains[, 2010, ])
    emi_gains[, 2005, ] <- setYears(emi_gains[, 2010, ])

    x <- ef_gains

    unit <- "Mt/TWa"
    description <- "Emission factors from GAINS for all CEDS setors execpt aviation and shipping"

    weight <- emi_gains

    # do not throw a warning for zero weights, as they seem to be intended
    aggregationArguments <- list(zeroWeight = "allow")
  } else if (subtype == "emissions_starting_values") {

    start_value_REMIND_regions <- readSource(type = "REMIND_11Regi", subtype = "AP_starting_values", convert = TRUE)

    x <- start_value_REMIND_regions
    unit <- "Mt"
    description <- "AP starting values for the first iteration of REMIND"
    weight <- NULL
    aggregationArguments <- NULL
  }

  return(list(x = x,
              weight = weight,
              unit = unit,
              description = description,
              aggregationArguments = aggregationArguments))

}
