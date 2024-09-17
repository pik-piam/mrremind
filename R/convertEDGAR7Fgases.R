#' @title convertEDGAR7Fgases
#' @author Gabriel Abrahao
#' @param x  magpie object to be converted
#'
#' @export
convertEDGAR7Fgases <- function(x) {
  # Aggregate regions, but not species yet
  x[is.na(x)] <- 0 # set NA to 0
  x <- toolCountryFill(x, fill = 0, verbosity = 2) # fill missing countries

  fgaskt <- x

  # Converting to CO2 equivalent

  # Read AR6 GWP table
  dfgwp <- readSource("AR6GWP")

  # Rename some gases in the AR6 table to be compatible with the names in EDGAR
  dfgwp$Gas[dfgwp$Gas == "PFC-116"] <- "C2F6"
  dfgwp$Gas[dfgwp$Gas == "PFC-14"] <- "CF4"
  dfgwp$Gas[dfgwp$Gas == "Sulfur hexafluoride"] <- "SF6"
  dfgwp$Gas[dfgwp$Gas == "PFC-218"] <- "C3F8"
  dfgwp$Gas[dfgwp$Gas == "PFC-31-10"] <- "C4F10"
  dfgwp$Gas[dfgwp$Gas == "Octafluorooxolane"] <- "c-C4F8" # Formula is actually c-C4F8O, but couldn't find anything on a gas without the O so assuming a typo in EDGAR
  dfgwp$Gas[dfgwp$Gas == "HFC-43-10mee"] <- "HFC-43-10-mee"
  dfgwp$Gas[dfgwp$Gas == "Nitrogen trifluoride"] <- "NF3"
  dfgwp$Gas[dfgwp$Gas == "PFC-41-12"] <- "C5F12"
  dfgwp$Gas[dfgwp$Gas == "PFC-51-14"] <- "C6F14"

  # Gas species in EDGAR data
  gases_edgar <- getItems(fgaskt, "Gas")

  # Gas species in AR6 data
  gases_ar6 <- dfgwp[["Gas"]]

  # Check if there are gases in EDGAR without a GWP in AR6 and throw a warning
  unknown_gases <- setdiff(gases_edgar, intersect(gases_ar6, gases_edgar))
  if (length(unknown_gases) > 0) {
    warning(paste0("calcEDGAR7Fgases couldn't find GWPs for the following gases: ",
                   paste(unknown_gases, collapse = ",")))
  }

  # Keep only used gases
  dfgwp <- dfgwp[dfgwp$Gas %in% gases_edgar, ]

  # Convert to MAgPIE object
  gwp <- as.magpie(dfgwp, spatial = 0, temporal = 0)

  # Multiply to get emissions per species in kt CO2eq, convert to Mt
  co2eq_gas <- fgaskt * gwp * 1e-3

  # To keep with the IMAGE standard, we have to report total F-gases (in Mt CO2eq) plus
  # individual emissions of the following gases (in kt of themselves):
  # HFC, which is the total of HFC125, HFC134a, HFC143a, HFC227ea, HFC23, HFC245fa, HFC32, HFC43-10
  # HFC125, HFC134a, HFC143a, HFC227ea, HFC23, HFC245fa, HFC32, HFC43-10, PFC, SF6, C2F6, C6F14, CF4

  # Total CO2eq
  co2eq_tot <- setNames(dimSums(co2eq_gas, 3.1), "emiFgasTotal")

  # PFC and HFC gases
  gases_pfc <- c("CF4", "C2F6", "C3F8", "C4F10", "C5F12", "C6F14")
  gases_hfc <- c(
    "HFC-125", "HFC-134a", "HFC-143a", "HFC-152a", "HFC-227ea", "HFC-245fa",
    "HFC-32", "HFC-365mfc", "HFC-23", "HFC-134",
    "HFC-143", "HFC-236fa", "HFC-41", "HFC-43-10-mee"
  )

  pfc_tot <- setNames(dimSums(fgaskt[, , gases_pfc], 3.1), "emiFgasPFC")
  hfc_tot <- setNames(dimSums(fgaskt[, , gases_hfc], 3.1), "emiFgasHFC")

  # Individual gases
  # First rename the individual gases
  eminame_mapping <- c(
    "HFC-125" = "emiFgasHFC125",
    "HFC-134a" = "emiFgasHFC134a",
    "HFC-143a" = "emiFgasHFC143a",
    "HFC-227ea" = "emiFgasHFC227ea",
    "HFC-245fa" = "emiFgasHFC245fa",
    "HFC-43-10-mee" = "emiFgasHFC43-10",
    "HFC-32" = "emiFgasHFC32",
    "C2F6" = "emiFgasC2F6",
    "CF4" = "emiFgasCF4",
    "SF6" = "emiFgasSF6",
    "HFC-23" = "emiFgasHFC23",
    "C6F14" = "emiFgasC6F14"
  )

  fgaskt_ind <- fgaskt[, , names(eminame_mapping)]
  fgaskt_ind <- setNames(fgaskt_ind, eminame_mapping)

  # Bind all gases in the format already used by REMIND (legacy of IMAGE's reported species)
  xout <- mbind(co2eq_tot, hfc_tot, pfc_tot, fgaskt_ind)

  return(xout)
}
