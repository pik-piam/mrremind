#' Gather reference data from various sources.
#' @importFrom dplyr filter group_by mutate select ungroup
calcHistorical <- function() {

  # Historical Emissions from PRIMAPhist data base ----
  primap <- readSource("PRIMAPhist", "hist")[, , "CAT0"]
  primap <- primap[, , c("co2_c", "kyotoghgar4_co2eq_c")] / 12 * 44
  primap <- collapseNames(primap)
  getNames(primap) <- c("Emi|CO2 (Mt CO2/yr)", "Emi|GHG (Mt CO2eq/yr)")
  primap <- add_dimension(primap, dim = 3.1, add = "model", nm = "PRIMAPhist")

  # Historical Emissions from CDIAC data base ----
  cdiac <- calcOutput("Emissions", datasource = "CDIAC", aggregate = FALSE, warnNA = FALSE)
  getNames(cdiac) <- gsub("Emissions", "Emi", getNames(cdiac))
  getNames(cdiac) <- gsub("Mt/yr", "Mt CO2/yr", getNames(cdiac))
  cdiac <- add_dimension(cdiac, dim = 3.1, add = "model", nm = "CDIAC")

  # Historical Land Use Emissions (taken from "mrvalidation/R/fullVALIDATION.R") ----
  luEDGAR <- calcOutput(type = "LandEmissions", datasource = "EDGAR_LU", aggregate = FALSE, warnNA = FALSE)
  luCEDS <- calcOutput(type = "LandEmissions", datasource = "CEDS", aggregate = FALSE, warnNA = FALSE)
  # give CEDS emissions from calcValidEmissions (magpie) a name that is different
  # from CEDS emissions from calcEmissions (remind)
  getNames(luCEDS, dim = 2) <- "CEDS Landuse"

  luFAOEmisLUC <- calcOutput(type = "LandEmissions", datasource = "FAO_EmisLUC", aggregate = FALSE, warnNA = FALSE)
  luFAOEmisAg <- calcOutput(type = "LandEmissions", datasource = "FAO_EmisAg", aggregate = FALSE, warnNA = FALSE)
  luPRIMAPhist <- calcOutput(type = "LandEmissions", datasource = "PRIMAPhist", aggregate = FALSE, warnNA = FALSE)

  # Cement Production ----
  USGS_cement <- readSource(
    type = "USGS", subtype = "cement",
    convert = FALSE
  ) %>%
    quitte::madrat_mule() %>%
    group_by(!!!syms(c("iso3c", "year"))) %>%
    filter(max(.data$reporting.year) == .data$reporting.year) %>%
    ungroup() %>%
    select(-"reporting.year") %>%
    # t/year * 1e-6 Mt/t = Mt/year
    mutate(
      value = .data$value * 1e-6,
      model = "USGS",
      variable = "Production|Industry|Cement (Mt/yr)"
    ) %>%
    select("iso3c", "year", "model", "variable", "value") %>%
    tidyr::complete(
      iso3c = unname(getISOlist()),
      year = unique(.data$year),
      fill = list(
        model = "USGS",
        variable = "Production|Industry|Cement (Mt/yr)",
        value = 0
      )
    ) %>%
    as.magpie(spatial = 1, temporal = 2, tidy = TRUE)

  # Steel Production ----
  worldsteel <- readSource("worldsteel", convert = FALSE) %>%
    quitte::madrat_mule() %>%
    filter(
      .data$name %in% c(
        "Production in Oxygen-Blown Converters",
        "Production in Open Hearth Furnaces",
        "DRI Production",
        "Production in Electric Arc Furnaces"
      ),
      .data$iso3c %in% (toolGetMapping(
        name = getConfig("regionmapping"),
        type = "regional", where = "mappingfolder"
      ) %>%
        pull("CountryCode"))
    ) %>%
    # kt/year * 1e-3 Mt/kt = Mt/year
    mutate(value = .data$value * 1e-3) %>%
    tidyr::pivot_wider(values_fill = 0) %>%
    mutate(
      `Production|Industry|Steel (Mt/yr)` = .data$`Production in Oxygen-Blown Converters`
      + .data$`Production in Open Hearth Furnaces`
      + .data$`Production in Electric Arc Furnaces`,
      `Production|Industry|Steel|Secondary (Mt/yr)` =
        # Secondary steel production is production from EAF that does not use
        # inputs from DRI.  If mostly DRI is used for EAF, the difference might
        # be negative (different mass bases due to e.g. carbon content), so
        # limit to zero.
        pmax(
          0,
          .data$`Production in Electric Arc Furnaces`
          - .data$`DRI Production`
        ),
      `Production|Industry|Steel|Primary (Mt/yr)` = (.data$`Production|Industry|Steel (Mt/yr)`
        - .data$`Production|Industry|Steel|Secondary (Mt/yr)`
      ),
      source = "Worldsteel"
    ) %>%
    select(
      "iso3c", "year", "source", "Production|Industry|Steel (Mt/yr)",
      "Production|Industry|Steel|Primary (Mt/yr)",
      "Production|Industry|Steel|Secondary (Mt/yr)"
    ) %>%
    tidyr::pivot_longer(c(
      "Production|Industry|Steel (Mt/yr)",
      "Production|Industry|Steel|Primary (Mt/yr)",
      "Production|Industry|Steel|Secondary (Mt/yr)"
    )) %>%
    tidyr::complete(tidyr::nesting(!!!syms(c("year", "source", "name"))),
      iso3c = toolGetMapping(
        name = getConfig("regionmapping"),
        type = "regional", where = "mappingfolder"
      ) %>%
        pull("CountryCode"),
      fill = list(value = 0)
    ) %>%
    as.magpie(spatial = 4, temporal = 1, datacol = ncol(.data))

  # blow up to union of years ====
  # find all existing years (y) and variable names (n)

  varlist <- list(
    primap, cdiac, luEDGAR, luCEDS,
    luFAOEmisLUC, luFAOEmisAg, luPRIMAPhist,
    worldsteel, USGS_cement
  )

  y <- Reduce(union, lapply(varlist, getYears))
  n <- Reduce(c, lapply(varlist, getNames))
  y <- sort(y)

  # create empty object with full temporal, regional and data dimensionality
  data <- new.magpie(getISOlist(), y, n, fill = NA)
  getSets(data)[3] <- "model"
  getSets(data)[4] <- "variable"

  # transfer data of existing years
  for (i in varlist) {
    data[, getYears(i), getNames(i)] <- i
  }

  # add scenario dimension ====
  data <- add_dimension(data, dim = 3.1, add = "scenario", nm = "historical")
  # rename dimension "data" into "variable"
  getSets(data)[5] <- "variable"

  # rename emission variables generated by calcValidEmissions (magpie) to the names generated by calcEmissions (remind)
  # note: spelling for the same gas might be different across historical sources
  getNames(data) <- gsub("Emissions|CO2|Land|+|Land Use Change", "Emi|CO2|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|CO2|Land|+|Land-use Change", "Emi|CO2|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|CH4|Land|Agriculture", "Emi|CH4|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|CH4|Land|+|Agriculture", "Emi|CH4|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|N2O|Land|Agriculture", "Emi|N2O|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|N2O|Land|+|Agriculture", "Emi|N2O|Land Use", getNames(data), fixed = TRUE)

  # change unit from Mt to kt for N2O from calcValidEmissions (magpie)
  vars_with_unit_Mt <- getNames(data[, , "(Mt N2O/yr)", pmatch = TRUE])
  data[, , vars_with_unit_Mt] <- data[, , vars_with_unit_Mt] * 1000
  getNames(data) <- gsub("(Mt N2O/yr)", "(kt N2O/yr)", getNames(data), fixed = TRUE)

  return(list(x = data, weight = NULL, unit = "Various", description = "Historical Data"))
}
