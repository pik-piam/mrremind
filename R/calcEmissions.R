#' @title calcEmissions
#'
#' @return magpie object with historical emissions
#' @param datasource "CEDS2REMIND", "CEDS2025", "EDGAR6",
#'                   "EDGARghg", "CDIAC", "ClimateTrace"
#'
#' @author Steve Smith, Pascal Weigmann
#'
calcEmissions <- function(datasource = "CEDS16") {

    ## ---- CEDS2REMIND ----
  if (datasource == "CEDS2REMIND") {
    # read CEDS emissions data from sources (in kt)
    bc    <- readSource("CEDS", subtype = "BC")
    ch4   <- readSource("CEDS", subtype = "CH4")
    co    <- readSource("CEDS", subtype = "CO")
    co2   <- readSource("CEDS", subtype = "CO2")
    n2o   <- readSource("CEDS", subtype = "N2O")
    nh3   <- readSource("CEDS", subtype = "NH3")
    nox   <- readSource("CEDS", subtype = "NOx")
    nmvoc <- readSource("CEDS", subtype = "NMVOC")
    oc    <- readSource("CEDS", subtype = "OC")
    so2   <- readSource("CEDS", subtype = "SO2")

    y <- Reduce(
      intersect,
      list(
        getYears(bc),
        getYears(ch4),
        getYears(co),
        getYears(co2),
        getYears(n2o),
        getYears(nh3),
        getYears(nox),
        getYears(nmvoc),
        getYears(oc),
        getYears(so2)
      )
    )

    emi <-
      mbind(bc[, y, ], ch4[, y, ], co[, y, ], co2[, y, ],
            n2o[, y, ], nh3[, y, ], nox[, y, ], nmvoc[, y, ],
            oc[, y, ], so2[, y, ]) / 1000 # kt to Mt
    rm(bc, ch4, co, co2, n2o, nh3, nox, nmvoc, oc, so2)

    # remove 6B_Other-not-in-total but warn in case it contains data
    if (any(!emi[, , "6B_Other-not-in-total"] == 0)) {
      cat(
        "CEDS59 sector 6B_Other-not-in-total was removed
            although it contains data! Please check CEDS source files.\n"
      )
    }
    emi <- emi[, , "6B_Other-not-in-total", invert = TRUE]

    # aggregate and rename CEDS59 sectors to REMIND variables
    map_CEDS59_to_REMIND <-
      toolGetMapping(type = "sectoral",
                     name = "mappingCEDS59toREMINDreporting.csv",
                     where = "mappingfolder")
    emi <-
      toolAggregate(
        x = emi,
        weight = NULL,
        dim = 3.1,
        rel = map_CEDS59_to_REMIND,
        from = "CEDS59",
        to = "REMIND"
      )

    # rename emissions according to map (currently only relevant for VOC)
    map <-
      c(
        BC = "BC",
        CH4 = "CH4",
        CO = "CO",
        CO2 = "CO2",
        N2O = "N2O",
        NH3 = "NH3",
        NOx = "NOx",
        NMVOC = "VOC",
        OC = "OC",
        SO2 = "SO2"
      )
    getNames(emi, dim = 2) <- map[getNames(emi, dim = 2)]

    # remove third entry "kt" in data dimension
    emi <- collapseNames(emi, collapsedim = 3)

    # sectoral sums, probably should be "Transport"?
    emi <-
      add_columns(emi, "Energy|Demand|Transportation", dim = 3.1)
    emi[, , "Energy|Demand|Transportation"] <-
      emi[, , "Energy|Demand|Transportation|Aviation"] +
      emi[, , "Energy|Demand|Transportation|Ground Transportation"] +
      emi[, , "Energy|Demand|Transportation|International Shipping"]

    emi <- add_columns(emi, "Energy|Supply", dim = 3.1)
    emi[, , "Energy|Supply"] <- emi[, , "Energy|Supply|Electricity"] +
      emi[, , "Energy|Supply|Heat"] +
      emi[, , "Energy|Supply|Fuel Production"]

    emi <- add_columns(emi, "Energy|Demand", dim = 3.1)
    emi[, , "Energy|Demand"] <-
      emi[, , "Energy|Demand|Transportation"] +
      emi[, , "Energy|Demand|Residential and Commercial"] +
      emi[, , "Energy|Demand|Industry"]

    emi <- add_columns(emi, "Energy", dim = 3.1)
    emi[, , "Energy"] <- emi[, , "Energy|Demand"] +
      emi[, , "Energy|Supply"]

    emi <- add_columns(emi, "Energy Supply and Demand", dim = 3.1)
    emi[, , "Energy Supply and Demand"] <- emi[, , "Energy"]

    emi <-
      add_columns(emi, "Energy and Industrial Processes", dim = 3.1)
    emi[, , "Energy and Industrial Processes"] <- emi[, , "Energy"] +
      emi[, , "Industrial Processes"]

    emi <- add_columns(emi, "Land Use", dim = 3.1)
    emi[, , "Land Use"] <-
      emi[, , "Land Use|Agriculture and Biomass Burning"] +
      emi[, , "Land Use|Forest Burning"] +
      emi[, , "Land Use|Grassland Burning"]

    emi <- add_columns(emi, "Total", dim = 3.1)
    emi[, , "Total"] <- emi[, , "Energy"] +
      emi[, , "Industrial Processes"] +
      emi[, , "Land Use"] +
      emi[, , "Solvents"] +
      emi[, , "Waste"]

    # get variables names right
    emi[, , "N2O"] <- emi[, , "N2O"] * 1000 # Mt to kt
    # change order, add "Emissions|", and reduce to a single dimension by
    # replacing dots: Waste.SO2.harm -> Emi|SO2|Waste|harm
    tmp <-
      gsub("^([^\\.]*)\\.(.*$)",
           "Emi|\\2|\\1 (Mt \\2/yr)",
           getNames(emi))
    # remove "Total" from variable name
    tmp <- gsub("\\|Total", "", tmp)
    tmp <- gsub("Mt N2O", "kt N2O", tmp)
    tmp <- gsub("\\|SO2\\|", "\\|Sulfur\\|", tmp)

    # Add full scenario name
    getNames(emi) <- tmp
    getSets(emi) <- c("region", "year", "variable")
    tmp <- emi

    description <- "historic emissions from 1970-2015"

    ## ---- CEDS 2025 ----
  } else if (datasource %in% c("CEDS2025", "CEDS2025_IAMC")) {
    # read CEDS emissions data from source (in Mt)
    # opposed to older version, doesn't contain Land-Use Change and thus no
    # aggregation to highest level is performed
    emi <- readSource("CEDS2025")

    # remove 6B_Other-not-in-total (no data in there anyway)
    emi <- emi[, , "6B_Other-not-in-total", invert = TRUE]

    # load mapping file
    map <- toolGetMapping(type = "sectoral",
                          name = "mappingCEDS2025toREMIND.csv",
                          where = "mrremind")

    # load sectoral mapping, depending on the selected output sectoral
    # resolution
    if (datasource == "CEDS2025") {
      map_to <- "REMIND"
    } else if (datasource == "CEDS2025_IAMC") {
      map_to <- "IAMC"
    }
    # aggregate and rename CEDS sectors to REMIND or IAMC sectors
    emi <-
      toolAggregate(
        x = emi,
        weight = NULL,
        dim = 3.1,
        rel = map,
        from = "CEDS2025",
        to = map_to
      )

    # undo unnecessary conversion from convertCEDS2025
    emi[, , "n2o_n"] <- emi[, , "n2o_n"] * 44 / 28
    emi[, , "nh3_n"] <- emi[, , "nh3_n"] * 17 / 14
    emi[, , "no2_n"] <- emi[, , "no2_n"] * 46 / 14
    emi[, , "co2_c"] <- emi[, , "co2_c"] * 44 / 12

    # rename emissions according to map
    emi_map <-
      c(bc_c = "BC",
        ch4 = "CH4",
        co = "CO",
        co2_c = "CO2",
        n2o_n = "N2O",
        nh3_n = "NH3",
        no2_n = "NOX",
        nmvoc = "VOC",
        oc_c = "OC",
        so2 = "SO2")
    getNames(emi, dim = 2) <- emi_map[getNames(emi, dim = 2)]

    # sectoral sums, only needed if mapped to REMIND sectors
    if (datasource == "CEDS2025") {
      emi <-
        add_columns(emi,
                    "Energy|Demand|Transport|International Bunkers",
                    dim = 3.1
        )
      emi[, , "Energy|Demand|Transport|International Bunkers"] <-
        emi[, , "Transport|Freight|International Shipping|Demand"] +
        emi[, , "Transport|Pass|Aviation|International|Demand"]

      emi <- add_columns(emi, "Energy|Supply", dim = 3.1)
      emi[, , "Energy|Supply"] <-
        emi[, , "Energy|Supply|Electricity"] +
        emi[, , "Energy|Supply|Heat"] +
        emi[, , "Energy|Supply|Fuels"]

      # Addition of new items for industry subsectors
      emi <- add_columns(emi, "Energy|Demand|Industry", dim = 3.1)
      emi[, , "Energy|Demand|Industry"] <-
        emi[, , "Energy|Demand|Industry|Chemicals"] +
        emi[, , "Energy|Demand|Industry|Steel"] +
        emi[, , "Energy|Demand|Industry|Non-Metallic Minerals"] +
        emi[, , "Energy|Demand|Industry|Other"]

      emi <- add_columns(emi, "Industrial Processes", dim = 3.1)
      emi[, , "Industrial Processes"] <-
        emi[, , "Industrial Processes|Chemicals"] +
        emi[, , "Industrial Processes|Steel"] +
        emi[, , "Industrial Processes|Non-Metallic Minerals"] +
        emi[, , "Industrial Processes|Other"]

      emi <-
        add_columns(emi, "Industry and Industrial Processes|Chemicals", dim = 3.1)
      emi[, , "Industry and Industrial Processes|Chemicals"] <-
        emi[, , "Energy|Demand|Industry|Chemicals"] +
        emi[, , "Industrial Processes|Chemicals"]

      emi <-
        add_columns(emi, "Industry and Industrial Processes|Steel", dim = 3.1)
      emi[, , "Industry and Industrial Processes|Steel"] <-
        emi[, , "Energy|Demand|Industry|Steel"] +
        emi[, , "Industrial Processes|Steel"]

      emi <-
        add_columns(emi,
                    "Industry and Industrial Processes|Non-Metallic Minerals",
                    dim = 3.1
        )
      emi[, , "Industry and Industrial Processes|Non-Metallic Minerals"] <-
        emi[, , "Energy|Demand|Industry|Non-Metallic Minerals"] +
        emi[, , "Industrial Processes|Non-Metallic Minerals"]

      emi <-
        add_columns(emi, "Industry and Industrial Processes|Other", dim = 3.1)
      emi[, , "Industry and Industrial Processes|Other"] <-
        emi[, , "Energy|Demand|Industry|Other"] +
        emi[, , "Industrial Processes|Other"]

      emi <- add_columns(emi, "w/o Bunkers|Energy|Demand", dim = 3.1)
      emi[, , "w/o Bunkers|Energy|Demand"] <-
        emi[, , "w/o Bunkers|Energy|Demand|Transport"] +
        emi[, , "Energy|Demand|Buildings"] +
        emi[, , "Energy|Demand|Industry"]

      emi <- add_columns(emi, "w/o Bunkers|Energy", dim = 3.1)
      emi[, , "w/o Bunkers|Energy"] <-
        emi[, , "w/o Bunkers|Energy|Demand"] +
        emi[, , "Energy|Supply"]

      emi <- add_columns(emi, "w/o Bunkers|Energy and Industrial Processes", dim = 3.1)
      emi[, , "w/o Bunkers|Energy and Industrial Processes"] <-
        emi[, , "w/o Bunkers|Energy"] +
        emi[, , "Industrial Processes"]

      # variables with bunker emissions
      emi <- add_columns(emi, "w/ Bunkers|Energy", dim = 3.1)
      emi[, , "w/ Bunkers|Energy"] <-
        emi[, , "w/o Bunkers|Energy"] +
        emi[, , "Energy|Demand|Transport|International Bunkers"]

      emi <-
        add_columns(emi, "w/ Bunkers|Energy and Industrial Processes", dim = 3.1)
      emi[, , "w/ Bunkers|Energy and Industrial Processes"] <-
        emi[, , "w/o Bunkers|Energy and Industrial Processes"] +
        emi[, , "Energy|Demand|Transport|International Bunkers"]

      emi <- add_columns(emi, "w/ Bunkers|Energy|Demand", dim = 3.1)
      emi[, , "w/ Bunkers|Energy|Demand"] <-
        emi[, , "w/o Bunkers|Energy|Demand"] +
        emi[, , "Energy|Demand|Transport|International Bunkers"]

      emi <- add_columns(emi, "w/ Bunkers|Energy|Demand|Transport", dim = 3.1)
      emi[, , "w/ Bunkers|Energy|Demand|Transport"] <-
        emi[, , "w/o Bunkers|Energy|Demand|Transport"] +
        emi[, , "Energy|Demand|Transport|International Bunkers"]

      # add default variables corresponding to w/ bunkers
      emi <- add_columns(emi, "Energy", dim = 3.1)
      emi[, , "Energy"] <-
        emi[, , "w/ Bunkers|Energy"]

      emi <-
        add_columns(emi, "Energy and Industrial Processes", dim = 3.1)
      emi[, , "Energy and Industrial Processes"] <-
        emi[, , "w/ Bunkers|Energy and Industrial Processes"]

      emi <- add_columns(emi, "Energy|Demand", dim = 3.1)
      emi[, , "Energy|Demand"] <-
        emi[, , "w/ Bunkers|Energy|Demand"]

      emi <- add_columns(emi, "Energy|Demand|Transport", dim = 3.1)
      emi[, , "Energy|Demand|Transport"] <-
        emi[, , "w/ Bunkers|Energy|Demand|Transport"]
    }


    # convert N20 to correct unit Mt to kt
    emi[, , "N2O"] <- emi[, , "N2O"] * 1000

    # change order, add "Emi|", and reduce to a single dimension by
    # replacing dots: Waste.SO2.harm -> Emi|SO2|Waste|harm
    tmp <-
      gsub("^([^\\.]*)\\.(.*$)",
           "Emi|\\2|\\1 (Mt \\2/yr)",
           getNames(emi))

    # remove "Total" from variable name, correct unit and rename SO2
    tmp <- gsub("Mt N2O", "kt N2O", tmp)
    tmp <- gsub("\\|SO2\\|", "\\|Sulfur\\|", tmp)

    # Add full scenario name
    getNames(emi) <- tmp
    getSets(emi) <- c("region", "year", "variable")
    tmp <- emi

    # Add total GHG as CO2 equivalents for sectors, only fits REMIND sectors
    if (datasource == "CEDS2025") {
      tmp <-
        add_columns(tmp, "Emi|GHG|Energy (Mt CO2eq/yr)", dim = 3.1)
      tmp[, , "Emi|GHG|Energy (Mt CO2eq/yr)"] <-
        tmp[, , "Emi|CO2|Energy (Mt CO2/yr)"] +
        tmp[, , "Emi|CH4|Energy (Mt CH4/yr)"] * 28 +
        tmp[, , "Emi|N2O|Energy (kt N2O/yr)"] / 1000 * 265

      tmp <-
        add_columns(tmp, "Emi|GHG|w/ Bunkers|Energy (Mt CO2eq/yr)", dim = 3.1)
      tmp[, , "Emi|GHG|w/ Bunkers|Energy (Mt CO2eq/yr)"] <-
        tmp[, , "Emi|CO2|w/ Bunkers|Energy (Mt CO2/yr)"] +
        tmp[, , "Emi|CH4|w/ Bunkers|Energy (Mt CH4/yr)"] * 28 +
        tmp[, , "Emi|N2O|w/ Bunkers|Energy (kt N2O/yr)"] / 1000 * 265

      tmp <-
        add_columns(tmp, "Emi|GHG|w/o Bunkers|Energy (Mt CO2eq/yr)", dim = 3.1)
      tmp[, , "Emi|GHG|w/o Bunkers|Energy (Mt CO2eq/yr)"] <-
        tmp[, , "Emi|CO2|w/o Bunkers|Energy (Mt CO2/yr)"] +
        tmp[, , "Emi|CH4|w/o Bunkers|Energy (Mt CH4/yr)"] * 28 +
        tmp[, , "Emi|N2O|w/o Bunkers|Energy (kt N2O/yr)"] / 1000 * 265

      tmp <-
        add_columns(tmp, "Emi|GHG|Industrial Processes (Mt CO2eq/yr)", dim = 3.1)
      tmp[, , "Emi|GHG|Industrial Processes (Mt CO2eq/yr)"] <-
        tmp[, , "Emi|CO2|Industrial Processes (Mt CO2/yr)"] +
        tmp[, , "Emi|CH4|Industrial Processes (Mt CH4/yr)"] * 28 +
        tmp[, , "Emi|N2O|Industrial Processes (kt N2O/yr)"] / 1000 * 265

      tmp <-
        add_columns(tmp, "Emi|GHG|Agriculture (Mt CO2eq/yr)", dim = 3.1)
      tmp[, , "Emi|GHG|Agriculture (Mt CO2eq/yr)"] <-
        tmp[, , "Emi|CO2|Agriculture (Mt CO2/yr)"] +
        tmp[, , "Emi|CH4|Agriculture (Mt CH4/yr)"] * 28 +
        tmp[, , "Emi|N2O|Agriculture (kt N2O/yr)"] / 1000 * 265

      tmp <-
        add_columns(tmp, "Emi|GHG|Energy|Demand|Industry (Mt CO2eq/yr)",
                    dim = 3.1)
      tmp[, , "Emi|GHG|Energy|Demand|Industry (Mt CO2eq/yr)"] <-
        tmp[, , "Emi|CO2|Energy|Demand|Industry (Mt CO2/yr)"] +
        tmp[, , "Emi|CH4|Energy|Demand|Industry (Mt CH4/yr)"] * 28 +
        tmp[, , "Emi|N2O|Energy|Demand|Industry (kt N2O/yr)"] / 1000 * 265

      tmp <-
        add_columns(tmp, "Emi|GHG|Waste (Mt CO2eq/yr)", dim = 3.1)
      tmp[, , "Emi|GHG|Waste (Mt CO2eq/yr)"] <-
        tmp[, , "Emi|CO2|Waste (Mt CO2/yr)"] +
        tmp[, , "Emi|CH4|Waste (Mt CH4/yr)"] * 28 +
        tmp[, , "Emi|N2O|Waste (kt N2O/yr)"] / 1000 * 265

      # industry including process emissions
      tmp <-
        add_columns(tmp, "Emi|CO2|Industry (Mt CO2/yr)", dim = 3.1)
      tmp[, , "Emi|CO2|Industry (Mt CO2/yr)"] <-
        tmp[, , "Emi|CO2|Industrial Processes (Mt CO2/yr)"] +
        tmp[, , "Emi|CO2|Energy|Demand|Industry (Mt CO2/yr)"]

      tmp <-
        add_columns(tmp, "Emi|GHG|Industry (Mt CO2eq/yr)", dim = 3.1)
      tmp[, , "Emi|GHG|Industry (Mt CO2eq/yr)"] <-
        tmp[, , "Emi|GHG|Industrial Processes (Mt CO2eq/yr)"] +
        tmp[, , "Emi|GHG|Energy|Demand|Industry (Mt CO2eq/yr)"]

      description <- "historic emissions from 1750-2023"
    } else if (datasource == "CEDS2025_IAMC") {
      description <- "historic emissions from 1750-2023, IAMC sectors"
    }


    ## ---- EDGAR 6 ----
  } else if (datasource == "EDGAR6") {
    # read EDGAR v6.0 and v5.0 emissions from sources
    pollutants <-
      c(
        "n2o",
        "ch4",
        "co2_excl_short",
        # from EDGAR v6.0
        "nh3",
        "no2",
        "bc",
        "co",
        "oc",
        "nmvoc",
        "pm10",
        "pm25",
        "so2"
      )  # from EDGAR v5.0
    emi <- NULL
    for (p in pollutants) {
      x <- readSource("EDGAR6", subtype = p)
      if (max(getYears(x)) == "y2015") {
        # for v5 data: add years 2016-18
        x <-
          add_columns(x,
                      addnm = c("y2016", "y2017", "y2018"),
                      dim = 2)
      } else if (ndim(x, dim = 3) == 3) {
        # for v6 data: drop "bio" variables, keep "fossil" ones
        x <- collapseDim(x["fossil", dim = 3], keepdim = "pollutant")
      }
      emi <- mbind(x, emi)
    }
    # remove 5A as it is currently not represented in REMIND
    emi <-
      emi[, , "5_A Indirect N2O emissions from the atmospheric deposition of nitrogen in NOx and NH3",
          invert = TRUE]
    # convert Units from kt -> Mt except for N2O
    emi <- emi / 1000
    emi[, , "n2o"] <- emi[, , "n2o"] * 1000

    # map sectors and pollutants to REMIND nomenclature
    map_sec <-
      toolGetMapping("mappingEDGAR6toREMIND.csv",
                     type = "sectoral",
                     where = "mappingfolder")
    map_pol <- c(
      n2o = "N2O",
      ch4 = "CH4",
      co2_excl_short = "CO2",
      nh3 = "NH3",
      no2 = "NOX",
      bc = "BC",
      co = "CO",
      oc = "OC",
      nmvoc = "VOC",
      pm10 = "PM10",
      pm25 = "PM25",
      so2 = "Sulfur"
    )

    # insert "0" instead of "NA" to avoid data-loss when aggregating.
    emi[is.na(emi)] <- 0

    emi <-
      toolAggregate(
        emi,
        dim = 3.2,
        rel = map_sec,
        from = "EDGAR6",
        to = "REMIND",
        partrel = TRUE
      )
    getNames(emi, dim = 1) <- map_pol[getNames(emi, dim = 1)]

    # sectoral sums
    emi <- add_columns(emi, "Energy|Supply", dim = 3.2)
    emi[, , "Energy|Supply"] <-
      emi[, , "Energy|Supply|Electricity and Heat"] +
      emi[, , "Energy|Supply|Fuel Production"]

    emi <- add_columns(emi, "Energy|Demand", dim = 3.2)
    emi[, , "Energy|Demand"] <- emi[, , "Energy|Demand|Transport"] +
      emi[, , "Energy|Demand|Buildings"] +
      emi[, , "Energy|Demand|Industry"]

    emi <- add_columns(emi, "Energy", dim = 3.2)
    emi[, , "Energy"] <- emi[, , "Energy|Demand"] +
      emi[, , "Energy|Supply"]

    emi <-
      add_columns(emi, "Energy and Industrial Processes", dim = 3.2)
    emi[, , "Energy and Industrial Processes"] <- emi[, , "Energy"] +
      emi[, , "Industrial Processes"]

    # Add "Emi" and replace "." by "|" hereby reducing name dimension, add units
    getNames(emi) <-
      gsub("^([^\\.]*)\\.(.*$)",
           "Emi|\\1|\\2 (Mt \\1/yr)",
           getNames(emi))
    getNames(emi) <- gsub("Mt N2O", "kt N2O", getNames(emi))
    getNames(emi) <- gsub("Mt Sulfur", "Mt SO2", getNames(emi))

    # Add total GHG as CO2 equivalents for sectors
    emi <- add_columns(emi, "Emi|GHG|Energy (Mt CO2eq/yr)", dim = 3.1)
    emi[, , "Emi|GHG|Energy (Mt CO2eq/yr)"] <-
      emi[, , "Emi|CO2|Energy (Mt CO2/yr)"] +
      emi[, , "Emi|CH4|Energy (Mt CH4/yr)"] * 28 +
      emi[, , "Emi|N2O|Energy (kt N2O/yr)"] / 1000 * 265

    emi <-
      add_columns(emi, "Emi|GHG|Industrial Processes (Mt CO2eq/yr)", dim = 3.1)
    emi[, , "Emi|GHG|Industrial Processes (Mt CO2eq/yr)"] <-
      emi[, , "Emi|CO2|Industrial Processes (Mt CO2/yr)"] +
      emi[, , "Emi|CH4|Industrial Processes (Mt CH4/yr)"] * 28 +
      emi[, , "Emi|N2O|Industrial Processes (kt N2O/yr)"] / 1000 * 265

    emi <-
      add_columns(emi, "Emi|GHG|Agriculture (Mt CO2eq/yr)", dim = 3.1)
    emi[, , "Emi|GHG|Agriculture (Mt CO2eq/yr)"] <-
      emi[, , "Emi|CO2|Agriculture (Mt CO2/yr)"] +
      emi[, , "Emi|CH4|Agriculture (Mt CH4/yr)"] * 28 +
      emi[, , "Emi|N2O|Agriculture (kt N2O/yr)"] / 1000 * 265

    emi <- add_columns(emi, "Emi|GHG|Waste (Mt CO2eq/yr)", dim = 3.1)
    emi[, , "Emi|GHG|Waste (Mt CO2eq/yr)"] <-
      emi[, , "Emi|CO2|Waste (Mt CO2/yr)"] +
      emi[, , "Emi|CH4|Waste (Mt CH4/yr)"] * 28 +
      emi[, , "Emi|N2O|Waste (kt N2O/yr)"] / 1000 * 265

    emi <-
      add_columns(emi, "Emi|CO2|Industry (Mt CO2/yr)", dim = 3.1)
    emi[, , "Emi|CO2|Industry (Mt CO2/yr)"] <-
      emi[, , "Emi|CO2|Industrial Processes (Mt CO2/yr)"] +
      emi[, , "Emi|CO2|Energy|Demand|Industry (Mt CO2/yr)"]

    emi <-
      add_columns(emi, "Emi|GHG|Energy|Demand|Industry (Mt CO2eq/yr)",
                  dim = 3.1)
    emi[, , "Emi|GHG|Energy|Demand|Industry (Mt CO2eq/yr)"] <-
      emi[, , "Emi|CO2|Energy|Demand|Industry (Mt CO2/yr)"] +
      emi[, , "Emi|CH4|Energy|Demand|Industry (Mt CH4/yr)"] * 28 +
      emi[, , "Emi|N2O|Energy|Demand|Industry (kt N2O/yr)"] / 1000 * 265

    emi <-
      add_columns(emi, "Emi|GHG|Industry (Mt CO2eq/yr)", dim = 3.1)
    emi[, , "Emi|GHG|Industry (Mt CO2eq/yr)"] <-
      emi[, , "Emi|GHG|Industrial Processes (Mt CO2eq/yr)"] +
      emi[, , "Emi|GHG|Energy|Demand|Industry (Mt CO2eq/yr)"]

    tmp <- emi
    description <- "historic emissions from 1970-2018"

    ## ---- EDGAR GHG ----
    # previous version: "EDGAR8"
  } else if (datasource == "EDGARghg") {
    emi <- readSource("EDGARghg")
    emi[is.na(emi)] <- 0

    # map variables
    map_sec <-
      toolGetMapping("mappingEDGAR8toREMIND.csv",
                     type = "sectoral",
                     where = "mrremind")
    emi <- toolAggregate(
      emi,
      dim = 3.2,
      rel = map_sec,
      from = "EDGAR8",
      to = "REMIND",
      partrel = TRUE
    )


    # aggregate pollutants to GHG (they already are given as GWPs)
    emi <- add_columns(emi,
                       addnm = "GHG",
                       dim = 3.1,
                       fill = 0)
    emi[, , "GHG"] <- dimSums(emi, dim = 3.1)

    # convert units, rename pollutants (F-Gases stay as GWPs)
    map_pol <-
      c(
        GWP_100_AR5_N2O = "N2O",
        GWP_100_AR5_CH4 = "CH4",
        CO2 = "CO2",
        `GWP_100_AR5_F-gases` = "F-Gases",
        GHG = "GHG"
      )
    getNames(emi, dim = 1) <- map_pol[getNames(emi, dim = 1)]

    # convert from Mt CO2eq/yr to Mt CH4/yr (AR5 GWP100)
    emi[, , "CH4"] <- emi[, , "CH4"] / 28

    # convert from Mt CO2eq/yr to kt N2O/yr (AR5 GWP100)
    emi[, , "N2O"] <- emi[, , "N2O"] * 1000 / 265


    # sectoral sums
    emi <- add_columns(emi, "Energy|Supply", dim = 3.2)
    emi[, , "Energy|Supply"] <-
      emi[, , "Energy|Supply|Electricity and Heat"] +
      emi[, , "Energy|Supply|Fuels"]

    emi <- add_columns(emi, "w/o Bunkers|Energy|Demand", dim = 3.2)
    emi[, , "w/o Bunkers|Energy|Demand"] <-
      emi[, , "w/o Bunkers|Energy|Demand|Transport"] +
      emi[, , "Energy|Demand|Buildings"] +
      emi[, , "Energy|Demand|Industry"]

    emi <- add_columns(emi, "w/o Bunkers|Energy", dim = 3.2)
    emi[, , "w/o Bunkers|Energy"] <-
      emi[, , "w/o Bunkers|Energy|Demand"] +
      emi[, , "Energy|Supply"]

    emi <-
      add_columns(emi, "w/o Bunkers|Energy and Industrial Processes", dim = 3.2)
    emi[, , "w/o Bunkers|Energy and Industrial Processes"] <-
      emi[, , "w/o Bunkers|Energy"] +
      emi[, , "Industrial Processes"]

    # bunkers
    emi <-
      add_columns(emi, "Energy|Demand|Transport|International Bunkers", dim = 3.2)
    emi[, , "Energy|Demand|Transport|International Bunkers"] <-
      emi[, , "Transport|Freight|International Shipping|Demand"] +
      emi[, , "Transport|Pass|Aviation|International|Demand"]


    # variables with bunker emissions
    emi <- add_columns(emi, "w/ Bunkers|Energy", dim = 3.2)
    emi[, , "w/ Bunkers|Energy"] <-
      emi[, , "w/o Bunkers|Energy"] +
      emi[, , "Energy|Demand|Transport|International Bunkers"]

    emi <-
      add_columns(emi, "w/ Bunkers|Energy and Industrial Processes", dim = 3.2)
    emi[, , "w/ Bunkers|Energy and Industrial Processes"] <-
      emi[, , "w/o Bunkers|Energy and Industrial Processes"] +
      emi[, , "Energy|Demand|Transport|International Bunkers"]

    emi <- add_columns(emi, "w/ Bunkers|Energy|Demand", dim = 3.2)
    emi[, , "w/ Bunkers|Energy|Demand"] <-
      emi[, , "w/o Bunkers|Energy|Demand"] +
      emi[, , "Energy|Demand|Transport|International Bunkers"]

    emi <- add_columns(emi, "w/ Bunkers|Energy|Demand|Transport", dim = 3.2)
    emi[, , "w/ Bunkers|Energy|Demand|Transport"] <-
      emi[, , "w/o Bunkers|Energy|Demand|Transport"] +
      emi[, , "Energy|Demand|Transport|International Bunkers"]

    # add default variables corresponding to w/ bunkers
    emi <- add_columns(emi, "Energy", dim = 3.2)
    emi[, , "Energy"] <-
      emi[, , "w/ Bunkers|Energy"]

    emi <-
      add_columns(emi, "Energy and Industrial Processes", dim = 3.2)
    emi[, , "Energy and Industrial Processes"] <-
      emi[, , "w/ Bunkers|Energy and Industrial Processes"]

    emi <- add_columns(emi, "Energy|Demand", dim = 3.2)
    emi[, , "Energy|Demand"] <-
      emi[, , "w/ Bunkers|Energy|Demand"]

    emi <- add_columns(emi, "Energy|Demand|Transport", dim = 3.2)
    emi[, , "Energy|Demand|Transport"] <-
      emi[, , "w/ Bunkers|Energy|Demand|Transport"]


    # Add "Emi" and replace "." by "|" hereby reducing name dimension, add units
    getNames(emi) <-
      gsub("^([^\\.]*)\\.(.*$)",
           "Emi|\\1|\\2 (Mt \\1/yr)",
           getNames(emi))
    getNames(emi) <- gsub("Mt N2O", "kt N2O", getNames(emi))
    getNames(emi) <- gsub("Mt F-Gases", "Mt CO2eq", getNames(emi))
    getNames(emi) <- gsub("Mt GHG", "Mt CO2eq", getNames(emi))

    getSets(emi) <- c("region", "period", "variable")

    tmp <- emi

    description <- "historic emissions from 1970-2023"

    ## ---- CDIAC ----
  } else if (datasource == "CDIAC") {
    data <- readSource("CDIAC")

    # omitting "PerCap"
    map <-
      c(
        FFIC    = "Fossil Fuels and Industry",
        # <--- old name. New name will be: "Energy and Industrial Processes",
        Solids  = "Energy|Solids",
        Liquids = "Energy|Liquids",
        Gases   = "Energy|Gases",
        Cement  = "Industrial Processes|Cement",
        Flaring = "Energy|Flaring",
        Bunker  = "Energy|Bunkers"
      )

    tmp <- NULL
    for (i in names(map)) {
      tmp <-
        mbind(tmp, setNames(data[, , i],
                            paste0("Emissions|CO2|", map[i], " (Mt/yr)")))
    }

    tmp <- tmp * 44 / 12 / 1000 # from ktC -> MtCO2
    tmp <- tmp[, seq(1970, 2013), ]

    description <- "historic emissions in 1970-2013"

    ## ---- ClimateTrace ----
  } else if (datasource == "ClimateTrace") {
    emi <- readSource("ClimateTrace")
    # map variables
    mapping <- toolGetMapping("mappingClimateTrace.csv",
                              type = "sectoral",
                              where = "mrremind")
    emi <- toolAggregate(
      emi,
      dim = 3.2,
      rel = mapping,
      from = "ClimateTrace",
      to = "REMIND",
      partrel = TRUE
    )

    # rename pollutants
    map_pol <-
      c(co2e_100yr = "GHG",
        n2o = "N2O",
        ch4 = "CH4",
        co2 = "CO2"
      )
    getNames(emi, dim = 1) <- map_pol[getNames(emi, dim = 1)]

    # at this point all variables exist for GHG, but only F-Gases have values
    # F-Gases also exist for other gases, but values are zero.
    emi[, , "GHG"] <-
      emi[, , "CO2"] +
      emi[, , "CH4"] * 28 +  # (AR5 GWP100)
      emi[, , "N2O"] * 265 + # (AR5 GWP100)
      emi[, , "GHG"]

    # convert from t to Mt: GHG, CO2, CH4
    emi[, , c("GHG", "CO2", "CH4")] <- emi[, , c("GHG", "CO2", "CH4")] / 1e6

    # convert from t to kt: N2O
    emi[, , "N2O"] <- emi[, , "N2O"] / 1000


    ## Additional Variables
    # sectoral sums
    emi <- add_columns(emi, "Energy|Supply|Electricity and Heat", dim = 3.2)
    emi[, , "Energy|Supply|Electricity and Heat"] <-
      emi[, , "Energy|Supply|Electricity"] +
      emi[, , "Energy|Supply|Heat"]

    emi <- add_columns(emi, "Energy|Supply", dim = 3.2)
    emi[, , "Energy|Supply"] <-
      emi[, , "Energy|Supply|Electricity and Heat"] +
      emi[, , "Energy|Supply|Fuels"]

    emi <-
      add_columns(emi, "w/o Bunkers|Energy and Industrial Processes", dim = 3.2)
    emi[, , "w/o Bunkers|Energy and Industrial Processes"] <-
      emi[, , "Energy|Supply"] +
      emi[, , "w/o Bunkers|Energy|Demand|Transport"] +
      emi[, , "Energy|Demand|Buildings"] +
      emi[, , "Industry"]  # = Energy|Demand|Industry + Industrial Processes

    # bunkers
    emi <-
      add_columns(emi, "Energy|Demand|Transport|International Bunkers", dim = 3.2)
    emi[, , "Energy|Demand|Transport|International Bunkers"] <-
      emi[, , "Transport|Freight|International Shipping|Demand"] +
      emi[, , "Transport|Pass|Aviation|International|Demand"]


    # variables with bunker emissions
    emi <-
      add_columns(emi, "w/ Bunkers|Energy and Industrial Processes", dim = 3.2)
    emi[, , "w/ Bunkers|Energy and Industrial Processes"] <-
      emi[, , "w/o Bunkers|Energy and Industrial Processes"] +
      emi[, , "Energy|Demand|Transport|International Bunkers"]

    emi <- add_columns(emi, "w/ Bunkers|Energy|Demand|Transport", dim = 3.2)
    emi[, , "w/ Bunkers|Energy|Demand|Transport"] <-
      emi[, , "w/o Bunkers|Energy|Demand|Transport"] +
      emi[, , "Energy|Demand|Transport|International Bunkers"]

    # add default variables corresponding to w/ bunkers
    emi <-
      add_columns(emi, "Energy and Industrial Processes", dim = 3.2)
    emi[, , "Energy and Industrial Processes"] <-
      emi[, , "w/ Bunkers|Energy and Industrial Processes"]

    emi <- add_columns(emi, "Energy|Demand|Transport", dim = 3.2)
    emi[, , "Energy|Demand|Transport"] <-
      emi[, , "w/ Bunkers|Energy|Demand|Transport"]


    # Add "Emi" and replace "." by "|" hereby reducing name dimension, add units
    getNames(emi) <-
      gsub("^([^\\.]*)\\.(.*$)",
           "Emi|\\1|\\2 (Mt \\1/yr)",
           getNames(emi))
    getNames(emi) <- gsub("Mt N2O", "kt N2O", getNames(emi))
    getNames(emi) <- gsub("Mt GHG", "Mt CO2eq", getNames(emi))

    getSets(emi) <- c("region", "period", "variable")

    # add totals per pollutant
    emi <- add_columns(emi, "Emi|CO2|w/o Bunkers (Mt CO2/yr)", dim = 3.1)
    emi[, , "Emi|CO2|w/o Bunkers (Mt CO2/yr)"] <-
      emi[, , "Emi|CO2|Agriculture (Mt CO2/yr)"] +
      emi[, , "Emi|CO2|w/o Bunkers|Energy and Industrial Processes (Mt CO2/yr)"] +
      emi[, , "Emi|CO2|Extraction (Mt CO2/yr)"] +
      emi[, , "Emi|CO2|Waste (Mt CO2/yr)"] +
      emi[, , "Emi|CO2|Land-Use Change (Mt CO2/yr)"] +
      emi[, , "Emi|CO2|non-ES CDR (Mt CO2/yr)"]

    emi <- add_columns(emi, "Emi|CH4|w/o Bunkers (Mt CH4/yr)", dim = 3.1)
    emi[, , "Emi|CH4|w/o Bunkers (Mt CH4/yr)"] <-
      emi[, , "Emi|CH4|Agriculture (Mt CH4/yr)"] +
      emi[, , "Emi|CH4|w/o Bunkers|Energy and Industrial Processes (Mt CH4/yr)"] +
      emi[, , "Emi|CH4|Extraction (Mt CH4/yr)"] +
      emi[, , "Emi|CH4|Waste (Mt CH4/yr)"] +
      emi[, , "Emi|CH4|Land-Use Change (Mt CH4/yr)"] +
      emi[, , "Emi|CH4|non-ES CDR (Mt CH4/yr)"]

    emi <- add_columns(emi, "Emi|N2O|w/o Bunkers (kt N2O/yr)", dim = 3.1)
    emi[, , "Emi|N2O|w/o Bunkers (kt N2O/yr)"] <-
      emi[, , "Emi|N2O|Agriculture (kt N2O/yr)"] +
      emi[, , "Emi|N2O|w/o Bunkers|Energy and Industrial Processes (kt N2O/yr)"] +
      emi[, , "Emi|N2O|Extraction (kt N2O/yr)"] +
      emi[, , "Emi|N2O|Waste (kt N2O/yr)"] +
      emi[, , "Emi|N2O|Land-Use Change (kt N2O/yr)"] +
      emi[, , "Emi|N2O|non-ES CDR (kt N2O/yr)"]

    emi <- add_columns(emi, "Emi|GHG|w/o Bunkers (Mt CO2eq/yr)", dim = 3.1)
    emi[, , "Emi|GHG|w/o Bunkers (Mt CO2eq/yr)"] <-
      emi[, , "Emi|GHG|Agriculture (Mt CO2eq/yr)"] +
      emi[, , "Emi|GHG|w/o Bunkers|Energy and Industrial Processes (Mt CO2eq/yr)"] +
      emi[, , "Emi|GHG|Extraction (Mt CO2eq/yr)"] +
      emi[, , "Emi|GHG|Waste (Mt CO2eq/yr)"] +
      emi[, , "Emi|GHG|Land-Use Change (Mt CO2eq/yr)"] +
      emi[, , "Emi|GHG|non-ES CDR (Mt CO2eq/yr)"] +
      emi[, , "Emi|GHG|F-Gases (Mt CO2eq/yr)"]

    # totals with Bunkers
    emi <- add_columns(emi, "Emi|CO2|w/ Bunkers (Mt CO2/yr)", dim = 3.1)
    emi[, , "Emi|CO2|w/ Bunkers (Mt CO2/yr)"] <-
      emi[, , "Emi|CO2|w/o Bunkers (Mt CO2/yr)"] +
      emi[, , "Emi|CO2|Energy|Demand|Transport|International Bunkers (Mt CO2/yr)"]

    emi <- add_columns(emi, "Emi|CH4|w/ Bunkers (Mt CH4/yr)", dim = 3.1)
    emi[, , "Emi|CH4|w/ Bunkers (Mt CH4/yr)"] <-
      emi[, , "Emi|CH4|w/o Bunkers (Mt CH4/yr)"] +
      emi[, , "Emi|CH4|Energy|Demand|Transport|International Bunkers (Mt CH4/yr)"]

    emi <- add_columns(emi, "Emi|N2O|w/ Bunkers (kt N2O/yr)", dim = 3.1)
    emi[, , "Emi|N2O|w/ Bunkers (kt N2O/yr)"] <-
      emi[, , "Emi|N2O|w/o Bunkers (kt N2O/yr)"] +
      emi[, , "Emi|N2O|Energy|Demand|Transport|International Bunkers (kt N2O/yr)"]

    emi <- add_columns(emi, "Emi|GHG|w/ Bunkers (Mt CO2eq/yr)", dim = 3.1)
    emi[, , "Emi|GHG|w/ Bunkers (Mt CO2eq/yr)"] <-
      emi[, , "Emi|GHG|w/o Bunkers (Mt CO2eq/yr)"] +
      emi[, , "Emi|GHG|Energy|Demand|Transport|International Bunkers (Mt CO2eq/yr)"]

    # add default totals corresponding to w/ bunkers
    emi <- add_columns(emi, "Emi|CO2 (Mt CO2/yr)", dim = 3.1)
    emi[, , "Emi|CO2 (Mt CO2/yr)"] <-
      emi[, , "Emi|CO2|w/ Bunkers (Mt CO2/yr)"]

    emi <- add_columns(emi, "Emi|CH4 (Mt CH4/yr)", dim = 3.1)
    emi[, , "Emi|CH4 (Mt CH4/yr)"] <-
      emi[, , "Emi|CH4|w/ Bunkers (Mt CH4/yr)"]

    emi <- add_columns(emi, "Emi|N2O (kt N2O/yr)", dim = 3.1)
    emi[, , "Emi|N2O (kt N2O/yr)"] <-
      emi[, , "Emi|N2O|w/ Bunkers (kt N2O/yr)"]

    emi <- add_columns(emi, "Emi|GHG (Mt CO2eq/yr)", dim = 3.1)
    emi[, , "Emi|GHG (Mt CO2eq/yr)"] <-
      emi[, , "Emi|GHG|w/ Bunkers (Mt CO2eq/yr)"]


    tmp <- emi

    description <- "historic emissions from 2015-2024"
    }

  return(list(
    x = tmp,
    weight = NULL,
    unit = "Mt",
    description = description
  ))
}
