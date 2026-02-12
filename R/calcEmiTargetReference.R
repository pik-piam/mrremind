#' Calculate Emission Targets reference from UNFCCC and CEDS to be used when
#' calculating Emission Targets.
#'
#' Uses historical emissions from 1990-2022.
#' CO2 (excl LU), CH4, N2O (so far no F-Gas historic time series)
#' When available, UNFCCC data is used, otherwise CEDS data.
#'
#' @author Rahel Mandaroux, Falk Benke
#' @seealso [calcEmiTarget()], [convertUNFCCC_NDC()]

calcEmiTargetReference <- function() {
  # Global Warming Potentials of CH4 and N20, AR5 WG1 CH08 Table 8.7
  gwpCH4 <- 28
  gwpN2O <- 265

  ceds <- calcOutput("Emissions", datasource = "CEDS2025", years = 1990:2022, aggregate = FALSE)
  unfccc <- collapseDim(calcOutput("UNFCCC", years = 1990:2022, aggregate = FALSE, warnNA = FALSE))

  # calculate CEDS values ----

  # calculate GHG total of CO2, CH4 and N2O [unit Mt CO2eq] without landuse
  GHGwoLULUCF <- dimSums(ceds[, , c(
    "Emi|CO2|w/o Bunkers|Energy and Industrial Processes (Mt CO2/yr)",
    "Emi|CO2|Agriculture (Mt CO2/yr)",
    "Emi|CO2|Waste (Mt CO2/yr)"
  )], dim = 3) +
    gwpN2O / 1000 * dimSums(ceds[, , c(
      "Emi|N2O|w/o Bunkers|Energy and Industrial Processes (kt N2O/yr)",
      "Emi|N2O|Agriculture (kt N2O/yr)",
      "Emi|N2O|Waste (kt N2O/yr)"
    )], dim = 3) +
    gwpCH4 * dimSums(ceds[, , c(
      "Emi|CH4|w/o Bunkers|Energy and Industrial Processes (Mt CH4/yr)",
      "Emi|CH4|Agriculture (Mt CH4/yr)",
      "Emi|CH4|Waste (Mt CH4/yr)"
    )], dim = 3)

  getNames(GHGwoLULUCF) <- "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"

  ghgCEDS <- mbind(GHGwoLULUCF, unfccc[, , "Emi|GHG|Land-Use Change|LULUCF national accounting (Mt CO2eq/yr)"])
  ghgCEDS <- add_columns(ghgCEDS, "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)", dim = 3.1)

  # calculate Emi with LULUCF from UNFCCC LULUCF, is NA for countries not in UNFCCC
  ghgCEDS[, , "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)"] <-
    ghgCEDS[, , "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"] +
    ghgCEDS[, , "Emi|GHG|Land-Use Change|LULUCF national accounting (Mt CO2eq/yr)"]

  # get UNFCCC values ----

  ghgUNFCCC <- unfccc[, , c(
    "Emi|GHG|Land-Use Change|LULUCF national accounting (Mt CO2eq/yr)",
    "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)"
  )]

  ghgUNFCCC <- add_columns(ghgUNFCCC, "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)", dim = 3.1)

  ghgUNFCCC[, , "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"] <-
    ghgUNFCCC[, , "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)"] -
    ghgUNFCCC[, , "Emi|GHG|Land-Use Change|LULUCF national accounting (Mt CO2eq/yr)"]

  # merge CEDS and UNFCCCC values ----

  unfcccReg <- intersect(
    getItems(unfccc, dim = 1),
    getItems(readSource("UNFCCC", convert = FALSE), dim = 1)
  )

  out <- ghgCEDS
  out[unfcccReg, , ] <- ghgUNFCCC[unfcccReg, , ]

  # fill gaps for LULUCF national accounting with IIASA data ----

  # for historical values, replace all NAs with IIASA data
  iiasaHist <- readSource("IIASALanduse", subtype = "historical")
  tmp <- out[, 2022, , invert = TRUE][, , "Emi|GHG|Land-Use Change|LULUCF national accounting (Mt CO2eq/yr)"]

  # this does not overwrite existing data of better quality, but only uses IIASA where
  # no better source is available
  tmp[is.na(tmp)] <- iiasaHist[is.na(tmp)]
  out[, getYears(tmp), getNames(tmp)] <- tmp[, , ]

  out <- add_columns(out, addnm = c("y2030", "y2035"), dim = 2)

  # add forecast for 2030 from IIASA
  fc2030 <- readSource("IIASALanduse", subtype = "forecast2030")
  out[, 2030, "Emi|GHG|Land-Use Change|LULUCF national accounting (Mt CO2eq/yr)"] <- fc2030

  # add forecast fo 2035 from PBL
  fc2035 <- readSource("IIASALanduse", subtype = "forecast2035")
  out[, 2035, "Emi|GHG|Land-Use Change|LULUCF national accounting (Mt CO2eq/yr)"] <- fc2035

  # fill gaps for Emissions incl. LULUCF national accounting with IIASA data ----

  # after filling gaps in "Emi|GHG|Land-Use Change|LULUCF national accounting" with IIASA data,
  # we can now also redo the calculation of "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)"
  # to fill more gaps (either gaps are filled, or previously existing values are recalculated below)

  out[,,"Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)"] <-
    out[,,"Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"] +
    out[,,"Emi|GHG|Land-Use Change|LULUCF national accounting (Mt CO2eq/yr)"]

  # fill all remaining NAs with 0 ----

  out[is.na(out)] <- 0

  return(list(
    x = out,
    unit = "Mt CO2eq",
    description = glue::glue("historical GHG emissions with and without LULUCF \\
    from 1990 to 2022 according to UNFCCC, CEDS and IIASA")
  ))
}
