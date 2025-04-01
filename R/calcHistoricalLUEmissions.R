#' Calculate historical landuse emissions
#'
#' @author David Klein, Falk Benke
calcHistoricalLUEmissions <- function() {

  # Historical Emissions from PRIMAPhist data base ----
  primap <- readSource("PRIMAPhist", "hist")[, , "CAT0"]
  primap <- primap[, , c("co2_c", "kyotoghgar4_co2eq_c")] / 12 * 44
  primap <- collapseNames(primap)
  getNames(primap) <- c("Emi|CO2 (Mt CO2/yr)", "Emi|GHG (Mt CO2eq/yr)")
  primap <- add_dimension(primap, dim = 3.1, add = "scenario", nm = "historical")
  primap <- add_dimension(primap, dim = 3.2, add = "model", nm = "PRIMAPhist")

  # Historical Emissions from CDIAC data base ----
  cdiac <- calcOutput("Emissions", datasource = "CDIAC", aggregate = FALSE, warnNA = FALSE)
  getNames(cdiac) <- gsub("Emissions", "Emi", getNames(cdiac))
  getNames(cdiac) <- gsub("Mt/yr", "Mt CO2/yr", getNames(cdiac))
  cdiac <- add_dimension(cdiac, dim = 3.1, add = "scenario", nm = "historical")
  cdiac <- add_dimension(cdiac, dim = 3.2, add = "model", nm = "CDIAC")

  # Historical Land Use Emissions (taken from "mrvalidation/R/fullVALIDATION.R") ----
  luEDGAR <- calcOutput(type = "LandEmissions", datasource = "EDGAR_LU", aggregate = FALSE, warnNA = FALSE)
  luCEDS <- calcOutput(type = "LandEmissions", datasource = "CEDS", aggregate = FALSE, warnNA = FALSE)
  # give CEDS emissions from calcValidEmissions (magpie) a name that is different
  # from CEDS emissions from calcEmissions (remind)
  getNames(luCEDS, dim = 2) <- "CEDS Landuse"

  luFAOEmisLUC <- calcOutput(type = "LandEmissions", datasource = "FAO_EmisLUC", aggregate = FALSE, warnNA = FALSE)
  luFAOEmisAg <- calcOutput(type = "LandEmissions", datasource = "FAO_EmisAg", aggregate = FALSE, warnNA = FALSE)
  getNames(luFAOEmisAg) <- gsub("projection", "historical", getNames(luFAOEmisAg), fixed = TRUE)

  luPRIMAPhist <- calcOutput(type = "LandEmissions", datasource = "PRIMAPhist", aggregate = FALSE, warnNA = FALSE)

  # find all existing years (y) and variable names (n)

  varlist <- list(primap, cdiac, luEDGAR, luCEDS, luFAOEmisLUC, luFAOEmisAg, luPRIMAPhist)
  y <- Reduce(union, lapply(varlist, getYears))
  n <- Reduce(c, lapply(varlist, getNames))
  y <- sort(y)

  # create empty object with full temporal, regional and data dimensionality
  data <- new.magpie(getISOlist(), y, n, fill = NA)
  getSets(data)[3] <- "scenario"
  getSets(data)[4] <- "model"
  getSets(data)[5] <- "variable"

  # transfer data of existing years
  for (i in varlist) {
    data[, getYears(i), getNames(i)] <- i
  }

  # rename emission variables to match REMIND variable names
  # note: spelling for the same gas might be different across historical sources
  getNames(data) <- gsub("Emissions|CO2|Land|+|Land-use Change", "Emi|CO2|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|CH4|Land|Agriculture", "Emi|CH4|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|CH4|Land|+|Agriculture", "Emi|CH4|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|N2O|Land|Agriculture", "Emi|N2O|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|N2O|Land|+|Agriculture", "Emi|N2O|Land Use", getNames(data), fixed = TRUE)

  # change unit from Mt to kt for N2O
  vars_with_unit_Mt <- getNames(data[, , "(Mt N2O/yr)", pmatch = TRUE])
  data[, , vars_with_unit_Mt] <- data[, , vars_with_unit_Mt] * 1000
  getNames(data) <- gsub("(Mt N2O/yr)", "(kt N2O/yr)", getNames(data), fixed = TRUE)

  return(list(x = data, weight = NULL, unit = "various",
              description = "historical landuse emissions"))
}
