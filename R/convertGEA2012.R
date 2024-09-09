#' @title convertGEA2012
#' @description Converts oil, gas and coal data from the Global Energy Assessment 2012 to country-level aggregation
#' @param x MAgPIE object to be disaggregated
#' @param subtype Type of fossil fuel (oil, coal or gas)
#' @return MAgPIE object containing country-level disaggregation of GEA 2012 data
#' @author Stephen Bi
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource("GEA2012")
#' }
#'
convertGEA2012 <- function(x, subtype) {
  if (subtype == "coal") {

    # Load mapping file for GEA regions to country level
    mapping <- toolGetMapping("regionmappingREMIND.csv", "regional", where = "mappingfolder")

    # Load country-level BGR data on coal combined reserve & resource distribution to serve as a disaggregation weight
    w <- readSource("BGR", subtype = "coal", convert = FALSE)[, , "Remaining_Potential"]
    getItems(w, dim = 1) <- toolCountry2isocode(getRegions(w))
    w <- toolNAreplace(toolCountryFill(w, fill = 0, verbosity = 2))[[1]]

    # Disaggregate GEA coal data to country level based on the BGR weights
    out <- toolAggregate(x[, , "xi3"], mapping, w)

    # Cost data xi1 and xi2 kept constant across regions
    out <- mbind(out, toolAggregate(x[, , c("xi1", "xi2")], mapping, weight = NULL))

  } else if (subtype %in% c("oil", "gas")) {

    # Load mapping file for GEA regions to country level
    mapping <- toolGetMapping("regionmappingGEA2012.csv", "regional", where = "mappingfolder")
    mapping$RegionCode[which(mapping$RegionCode == "ARC")] <- "WEU"
    mapping$RegionCode[which(mapping$RegionCode == "SOO")] <- "LAC"
    # Divide ARC fuels equally among EUR (WEU), USA, RUS (FSU), CAN
    # UNCLOS not ratified by USA, and territorial dispute would be uncertain even if it were
    for (reg in c("WEU", "USA", "FSU", "CAN")) {
      x[reg, , "xi3"] <- x[reg, , "xi3"] + 0.25 * x["ARC", , "xi3"]
    }
    x <- x["ARC", , invert = TRUE] # Remove ARC region

    # Antarctic Treaty banned resource extraction until at least 2048
    x <- x["SOO", , invert = TRUE] # Remove SOO region

    # Read country-level BGR data, distinguished between reserves and resources
    w <- readSource("BGR", subtype = subtype, convert = FALSE)[, , c("Reserves", "Resources")]
    getItems(w, dim = 1) <- toolCountry2isocode(getRegions(w))
    w <- toolNAreplace(toolCountryFill(w, fill = 0, verbosity = 2))[[1]]

    # Disaggregate the GEA data according to the BGR data on country-level oil/gas combined reserves + resources
    w <- dimSums(w, dim = 3)
    out <- toolAggregate(x[, , getNames(x[, , "xi3"])], mapping, weight = w)

    # Cost data xi1 and xi2 kept constant across regions
    out <- mbind(out, toolAggregate(x[, , c("xi1", "xi2", "dec")], mapping, weight = NULL))
  }

  if (subtype %in% c("oil", "coal", "gas")) {

    # a hack: we must reformat the subdimension 'grade' so that its values are not
    # integers, which causes the internal conversion back to maglass to wrongly guess
    # the year dimension

    getNames(out, dim = 4) <- paste0(getNames(out, dim = 4), "-grade")

    # convert T US$2005/TWa -> T US$2017/TWa for cost factors xi1 and xi2
    tmp <- GDPuc::convertGDP(
      gdp = out[, , c("xi1", "xi2")],
      unit_in = "constant 2005 US$MER",
      unit_out = mrdrivers::toolGetUnitDollar(),
      replace_NAs = "with_USA"
    )

    out <- mbind(tmp, out[, , c("xi1", "xi2"), invert = TRUE])

    # undo the hack after conversion
    getNames(out, dim = 4) <- gsub("-grade", "", getNames(x, dim = 4))

  } else {
    out <- x
  }

  return(out)
}
