#' Calculate FETaxes
#'
#' Reads in the data of the source IIASA_subs_taxes, by country. and
#' calculate taxes at the final energy delivery level to the end-use sectors
#' (industry, buildings and transport). Regional aggregation is done via the
#' respective energy quantities as weights.
#'
#' @param subtype choose between tax rates ("taxes") or subsidies rate ("subsidies") output
#'
#' @return MAgPIE object
#' @author Christoph Bertram and Renato Rodrigues
#' @seealso \code{\link{readIIASA_subs_taxes}}, \code{\link{convertIIASA_subs_taxes}}
#' @examples
#' \dontrun{
#' calcOutput("FETaxes")
#' }
#'
calcFETaxes <- function(subtype = "taxes") {
  # read in taxes/subsidies values
  if (subtype == "taxes") {
    tax <- readSource("IIASA_subs_taxes", subtype = "tax_rate")
    desc <- "Aggregated final energy tax data from country level data provided by IIASA (Jessica Jewell) \
extrapolated into the future for various tax scenarios"
  } else if (subtype == "subsidies") {
    tax <- -readSource("IIASA_subs_taxes", subtype = "subsidies_bulk")
    desc <- "Aggregated final energy subsidy data from country level data provided by IIASA (Jessica Jewell)"
  } else {
    stop("the subtype must be either 'taxes' or 'subsidies'")
  }

  # read in energy values
  energy <- readSource("IIASA_subs_taxes", subtype = "energy")
  # energy = 0 for regions/carriers with no information on subsidies, so that they are not considered in the weighting
  energy[is.na(tax)] <- 0
  # taxes without value are considered to be zero
  tax[is.na(tax)] <- 0
  # energy without value is considered to be zero
  energy[is.na(energy)] <- 0

  tax_map <- list(
    "indst" = c(
      "fegas" = "IN-Naturalgas",
      "feh2s" = "IN-Naturalgas",
      "fehos" = "IN-Oil",
      "fesos" = "IN-Coal",
      "feels" = "IN-Electricity",
      "fehes" = "TRP-Oilproducts"
    ),
    "build" = c(
      "fegas" = "RC-Naturalgas",
      "feh2s" = "RC-Naturalgas",
      "fehos" = "RC-Heatingoil",
      "fesos" = "RC-Coal",
      "feels" = "RC-Electricity",
      "fehes" = "TRP-Oilproducts"
    ),
    "trans" = c(
      "fepet" = "TRP-Oilproducts",
      "fedie" = "TRP-Oilproducts",
      "fegat" = "TRP-Oilproducts",
      "feh2t" = "TRP-Oilproducts",
      "feelt" = "RC-Electricity"
    )
  )


  Rtax <- Renergy <- NULL
  for (sector in c("indst", "build", "trans")) {
    Rtax <- mbind(Rtax,
                  add_dimension(setNames(tax[, , tax_map[[sector]]], names(tax_map[[sector]])),
                                dim = 3.1, add = "sector", nm = sector))
    Renergy <- mbind(Renergy,
                     add_dimension(setNames(energy[, , tax_map[[sector]]], names(tax_map[[sector]])),
                                   dim = 3.1, add = "sector", nm = sector))
  }

  # convert original data from bulk values to subsidies rates for the case of subsidies
  if (subtype == "subsidies") {
    Rtax <- Rtax / Renergy * 1e9 # converting from billion$/GJ to $/GJ
    Rtax[is.na(Rtax)] <- 0
  }

  # Ex-post tax rate adjustments
  # feelt tax rate as weighted average on industry and buildings
  Rtax[, , "trans.feelt"] <- (Rtax[, , "indst.feels"] * Renergy[, , "indst.feels"] + Rtax[, , "build.feels"] *
                                Renergy[, , "build.feels"]) / (Renergy[, , "indst.feels"] + Renergy[, , "build.feels"])
  Rtax[, , "trans.feelt"][is.na(Rtax[, , "trans.feelt"])] <- 0
  ## feh2t tax rate as same as feelt
  Rtax[, , "trans.feh2t"] <- Rtax[, , "trans.feelt"]
  # disabling tax for fehes and feh2t
  Rtax[, , c("fehes")] <- 0
  # do not apply gas subsidies to H2
  if (subtype == "subsidies") {
    Rtax[, , c("feh2s")] <- 0
  }

  # Ex-post energy weight adjustments
  Renergy[, , "build.feh2s"] <- Renergy[, , "build.fegas"]
  Renergy[, , "indst.feh2s"] <- Renergy[, , "indst.fegas"]
  Renergy[, , "trans.feelt"] <- Renergy[, , "build.feels"]
  Renergy[, , "trans.feh2t"] <- Renergy[, , "build.feels"]
  Renergy[, , c("fehes")] <- 0

  # cdr sector taxes equal to industry taxes
  cdrTax <- Rtax[, , "indst"]
  getNames(cdrTax, dim = 1) <- "cdr"
  Rtax <- mbind(Rtax, cdrTax)
  cdrEnergy <- Renergy[, , "indst"]
  getNames(cdrEnergy, dim = 1) <- "cdr"
  Renergy <- mbind(Renergy, cdrEnergy)

  # bunkers sector taxes equal to transport taxes
  bunkersTax <- Rtax[, , "trans"]
  getNames(bunkersTax, dim = 1) <- "bunkers"
  Rtax <- mbind(Rtax, bunkersTax)
  bunkersEnergy <- Renergy[, , "trans"]
  getNames(bunkersEnergy, dim = 1) <- "bunkers"
  Renergy <- mbind(Renergy, bunkersEnergy)

  # set base year
  getYears(Rtax) <- "2005"
  getYears(Renergy) <- "2005"

  # introduce upper bounds for subsidies
  if (subtype == "subsidies") {

    mapREMINDH12 <- toolGetMapping("regionmappingH12.csv", "regional", where = "mappingfolder")

    MEA <- mapREMINDH12 %>%
      filter(.data$RegionCode == "MEA") %>%
      pull("CountryCode")

    LAM <- mapREMINDH12 %>%
      filter(.data$RegionCode == "LAM") %>%
      pull("CountryCode")

    REF <- mapREMINDH12 %>%
      filter(.data$RegionCode == "REF") %>%
      pull("CountryCode")

    SSA <- mapREMINDH12 %>%
      filter(.data$RegionCode == "SSA") %>%
      pull("CountryCode")

    Rtax[LAM, , "fegas"] <- pmax(Rtax[LAM, , "fegas"], -7)
    Rtax[MEA, , "fegas"] <- pmax(Rtax[MEA, , "fegas"], -5)
    Rtax[REF, , "fegas"] <- pmax(Rtax[REF, , "fegas"], -3)
    Rtax["IND", , "fegas"] <- pmax(Rtax["IND", , "fegas"], -7)

    Rtax[REF, , "fesos"] <- pmax(Rtax[REF, , "fesos"], -1.5)

    Rtax[MEA, , "fedie"] <- pmax(Rtax[MEA, , "fedie"], -8)
    Rtax[MEA, , "fepet"] <- pmax(Rtax[MEA, , "fepet"], -8)

    Rtax[SSA, , "fehos"] <- pmax(Rtax[SSA, , "fehos"], -3)
    Rtax[MEA, , "fehos"] <- pmax(Rtax[MEA, , "fehos"], -0.1)
    Rtax["IND", , "fehos"] <- pmax(Rtax["IND", , "fehos"], -6)

    # Subsidy proportional cap to avoid liquids increasing dramatically
    # These factors are derived from previous REMIND versions
    Rtax[REF, , "fehos"] <- Rtax[REF, , "fehos"] * 0.5

  }

  if (subtype == "taxes") {
    years <- sort(unique(quitte::remind_timesteps$period))
    # all emi_sectors present in remind
    sectors <- c("power", "refining", "solids", "extraction", "build", "indst", "trans", "agriculture", "waste", "cdr",
                 "lulucf", "bunkers", "other", "indirect")
    # set all taxes to 0 that will be weighted 0
    Rtax[Renergy == 0] <- 0
    # extend base year value to all time steps
    Rtax <- time_interpolate(Rtax, years, integrate_interpolated_years = TRUE, extrapolation_type = "constant")

    # See documentation of cm_fetaxscen in Remind
    # scenario 0 has no FE taxes
    scen0 <- Rtax * 0
    # scenario 1,3,4 use the default taxes
    scen1 <- scen3 <- scen4 <- Rtax

    # scenario 2 converges some FE taxes to expert-guessed values in 2050
    convergence <- readSource("ExpertGuess", subtype = "taxConvergence", convert = TRUE)
    convergence <- add_dimension(convergence, dim = 3.1, add = "sector", nm = sectors, expand = TRUE)
    stopifnot(getYears(convergence) == "y2050")
    # converge taxes from now onward
    scen2 <- add_columns(Rtax[, c(2025, 2050), ], addnm = setdiff(getNames(convergence), getNames(Rtax)), fill = 0,
                         dim = 3)
    scen2[getItems(convergence, dim = 1), getYears(convergence), getNames(convergence)] <- convergence
    # linear interpolation between now and 2050
    scen2 <- time_interpolate(scen2, years, integrate_interpolated_years = TRUE, extrapolation_type = "constant")

    # scenario 5 rolls back some FE taxes to expert-guessed values in 2035
    rollback <- readSource("ExpertGuess", subtype = "taxConvergenceRollback", convert = TRUE)
    rollback <- add_dimension(rollback, dim = 3.1, add = "sector", nm = sectors, expand = TRUE)
    stopifnot(getYears(rollback) == "y2035")
    # converge taxes from now onward
    scen5 <- add_columns(Rtax[, c(2025, 2035), ], addnm = setdiff(getNames(rollback), getNames(Rtax)), fill = 0,
                         dim = 3)
    scen5[getItems(rollback, dim = 1), getYears(rollback), getNames(rollback)] <- rollback
    # linear interpolation between now and 2035
    scen5 <- time_interpolate(scen5, years, integrate_interpolated_years = TRUE, extrapolation_type = "constant")

    curves <- list(scen0, scen1, scen2, scen3, scen4, scen5)
    result <- NULL
    for (i in seq_along(curves)) {
      result <- mbind(result, add_dimension(curves[[i]], dim = 3.1, add = "scenario", nm = i - 1))
    }


    non_2_trillion <- 1e-12
    GJ_2_TWa <- 31.71e-12
    epsilon <- 1e-12
    result <- result * (non_2_trillion / GJ_2_TWa)
    # adapt Renergy to match Rtax (except the scenario subdimension)
    # use epsilon so that regions with 0 weight still have taxes later in the convergence scenarios
    Renergy <- add_columns(Renergy, addnm = setdiff(getNames(convergence), getNames(Renergy)), fill = epsilon, dim = 3)
    Renergy <- add_columns(Renergy, addnm = setdiff(getNames(rollback), getNames(Renergy)), fill = epsilon, dim = 3)
    Renergy[Renergy == 0] <- epsilon
    # do not throw a warning for zero weights, as they seem to be intended
    return(list(x = result, weight = Renergy, unit = "trillion US$2017/TWa", description = desc,
                aggregationArguments = list(zeroWeight = "allow")))
  }

  # Weights do not take into account the differentiation by services. So if
  # the tax in a Cooling country is very high and the tax in a country in the
  # same region using a lot of electricity for cooking is low, the tax for
  # cooling and cooking with electricity will be equal where it should be high
  # for cooling and low for cooking
  # So, we can assume that countries are app. similar in a given region

  # do not throw a warning for zero weights, as they seem to be intended
  list(x = Rtax, weight = Renergy, unit = "US$2017/GJ", description = desc,
    aggregationArguments = list(zeroWeight = "allow")
  )
}
