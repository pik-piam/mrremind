#' @title calc Capacity
#' @description provides historical capacity values in TW
#'
#' @param subtype data subtype. Either "capacityByTech" or "capacityByPE"
#' @return magpie object of  capacity data
#' @author Renato Rodrigues, Stephen Bi
#' @examples
#' \dontrun{
#' calcOutput("Capacity", subtype = "capacityByTech")
#' }
calcCapacity <- function(subtype) {

  if ((subtype == "capacityByTech_windoff") || (subtype == "capacityByTech")) {
    description <- "Historical capacity by technology." # now always includes offshore wind

    # Use IRENA data for world renewables capacity.
    # Year: 2000-2017
    # Technologies: "csp", "geohdr", "hydro", "spv", "windon", "windoff"
    mapping_IRENA <- tribble(
      ~IRENA_techs,               ~REMIND_techs,
      "Concentrated solar power", "csp",
      "Geothermal",               "geohdr",
      "Renewable hydropower",     "hydro",
      "Solar photovoltaic",       "spv",
      "Onshore wind energy",      "windon",
      "Offshore wind energy",     "windoff"
    )

    if (subtype == "capacityByTech_windoff") {
      description <- "Historical capacity by technology including offshore wind."
      mapping_IRENA$REMIND_techs[mapping_IRENA$REMIND_techs == "windon"] <- "wind"
    }

    IRENAcap <- readSource(type = "IRENA", subtype = "Capacity") # Read IRENA renewables capacity data
    IRENAcap <- IRENAcap[, , mapping_IRENA$IRENA_techs] # selecting data used on REMIND
    # renaming technologies to REMIND naming convention
    IRENAcap <- madrat::toolAggregate(IRENAcap, dim = 3, rel = mapping_IRENA, from = "IRENA_techs", to = "REMIND_techs")
    IRENAcap <- IRENAcap * 1E-06 # converting MW to TW

    # Use Openmod capacity values updated by the LIMES team for the European countries.
    # Year: 2015
    # Technologies: "tnrs","ngcc","ngt","dot"
    mapping_Openmod <- tribble(
      ~Openmod_techs, ~REMIND_techs,
      "tnr",          "tnrs",
      "ngcc",         "ngcc",
      "ngt",          "ngt",
      "oil",          "dot"
    )

    Openmodcap <- readSource(type = "Openmod") # Read Openmod capacities
    # selecting data used on REMIND "BAL"
    Openmodcap <- Openmodcap[c("FIN", "NOR", "SWE", "EST", "LVA", "LTU", "DNK", "GBR", "IRL", "NLD", "POL",
                               "DEU", "BEL", "LUX", "CZE", "SVK", "AUT", "CHE", "HUN", "ROU", "SVN", "FRA",
                               "HRV", "BGR", "ITA", "ESP", "PRT", "GRC"), , mapping_Openmod$Openmod_techs]
    # renaming technologies to REMIND naming convention
    Openmodcap <- madrat::toolAggregate(Openmodcap, dim = 3, rel = mapping_Openmod, from = "Openmod_techs", to = "REMIND_techs")
    Openmodcap <- Openmodcap * 1E-03 # converting GW to TW

    # Use WEO 2017 data to additional countries: "USA","BRA","RUS","CHN","IND","JPN"
    # Year: 2015
    # Technologies: "tnrs","dot"
    mapping_WEO <- tribble(
      ~WEO_techs, ~REMIND_techs,
      "Nuclear",  "tnrs",
      "Oil",      "dot"
    )
    WEOcap <- readSource(type = "IEA_WEO", subtype = "Capacity") # Read IEA WEO capacities
    WEOcap <- WEOcap[c("USA", "BRA", "RUS", "CHN", "IND", "JPN"), 2015, mapping_WEO$WEO_techs] # selecting data used on REMIND
    # renaming technologies to REMIND naming convention
    WEOcap <- madrat::toolAggregate(WEOcap, dim = 3, rel = mapping_WEO, from = "WEO_techs", to = "REMIND_techs")
    WEOcap <- WEOcap * 1E-03 # converting GW to TW

    #    ***CG: fix CHA gas power capacities: 97 GW by September 2020 (Oxford Institute for Energy Studies:
    #    Natural gas in China’s power sector: Challenges and the road ahead
    #    (https://www.oxfordenergy.org/wpcms/wp-content/uploads/2020/12/Insight-80-Natural-gas-in-Chinas-power-sector.pdf)
    #    ~50% is peaking (= ngt), the other 50 is called cogeneration but contains ngcc and gaschp
    #    *** for 2018-2022, take 90GW, 90GW*0.5=50GW ngt, the rest is split between ngcc and gaschp 70:30 (from IEA EB energy output)

    CHA.2020.GasData <- as.magpie(
      tibble::tribble(
        ~region,   ~year,   ~data,      ~value,
        "CHN",     2010,    "gaschp",   0.004,
        "CHN",     2015,    "gaschp",   0.011,
        "CHN",     2020,    "gaschp",   0.014,
        "CHN",     2010,    "ngcc",     0.009,
        "CHN",     2015,    "ngcc",     0.025,
        "CHN",     2020,    "ngcc",     0.032,
        "CHN",     2010,    "ngt",      0.013,
        "CHN",     2015,    "ngt",      0.036,
        "CHN",     2020,    "ngt",      0.045))

    # RP: add upper bound for USA PV in 2025, as current forecast by Wood Mackenzie Solar Market Insight Report 2022 sees ~ 265 GW DC in 2025 in
    # bullish scenario. So it would be less in GW_AC, but REMIND corrects for lower model CF than real world (in USA) by upscaling capacity
    # so it should be roughly ok as upper bound. (don't use as lower bound!)

    USA.2025.PVData <- as.magpie(
      tibble::tribble(
        ~region,   ~year,   ~data,      ~value,
        "USA",     2025,    "spv",      0.265))

    # merge IRENA, Openmod and WEO capacities data
    datasets <- list(IRENAcap, Openmodcap, WEOcap, CHA.2020.GasData, USA.2025.PVData)
    output <- new.magpie(
      cells_and_regions = unique(unlist(lapply(datasets, getRegions))),
      years = unique(unlist(lapply(datasets, getYears))),
      names = unique(unlist(lapply(datasets, getNames))),
      fill = 0
    )

    for (dataset in datasets) {
      output[getRegions(dataset), getYears(dataset), getNames(dataset)] <- dataset[getRegions(dataset), getYears(dataset), getNames(dataset)]
    }

    output[is.na(output)] <- 0 # set NA to 0
    output  <- toolCountryFill(output, fill = 0, verbosity = 2) # fill missing countries

  } else if (grepl("capacityByPE", subtype)) {
    # Pe -> peoil, pegas, pecoal, peur, pegeo, pehyd, pewin, pesol, pebiolc, pebios, pebioil
    description <- "Historical capacity by primary energy."

    # Secondary Energy Electricity capacities by primary energy source
    # Data for non-RE techs from Ember
    # Except coal, which comes from Global Coal Plant Tracker

    ## Primary Energies: "peur", "pecoal", "pecoal", "pegas", "pegas", "pehyd",
    ## "pewin", "pewin", "pesol", "pehyd", "pebiolc", "pesol", "peoil"
    # mapping <- data.frame( Openmod_techs=c("tnr", "pc", "lpc", "ngcc", "ngt", "hydro", "windon", "windoff", "spv",
    #                                        "psp", "biolcigcc", "csp", "oil"), #, "waste", "others"
    # REMIND_PE=c("peur", "pecoal", "pecoal", "pegas", "pegas", "pehyd", "pewin", "pewin",
    #             "pesol", "pehyd", "pebiolc", "pesol", "peoil"), stringsAsFactors = FALSE)
    mapping_ember <- tribble(
      ~ember_techs, ~REMIND_PE,
      "Biomass",    "pebiolc",
      "Coal",       "pecoal",
      "Gas",        "pegas",
      "Oil",        "peoil",
      "Hydro",      "pehyd",
      "Nuclear",    "peur",
      "Solar",      "pesol",
      "Wind",       "pewin"
    )

    embercap <- calcOutput("Ember", subtype = "capacity", aggregate = FALSE)
    embercap <- setNames(embercap,
                         nm = gsub("Cap|Electricity|", "",
                                   gsub(" (GW)", "",
                                        getNames(embercap), fixed = TRUE), fixed = TRUE))

    # aggregating primary energies to REMIND naming convention
    embercap <- toolAggregate(embercap[, , mapping_ember$ember_techs], rel = mapping_ember, from = "ember_techs",
                              to = "REMIND_PE", dim = 3.1)
    embercap <- embercap * 1E-03 # converting GW to TW

    embercap <- embercap[, , c("peur", "pegas", "pebiolc", "pehyd")] # pegas is handled at technology level

    # estimating lower bound coal capacity to remaining countries assuming
    # (1) capacity factors are given by REMIND pc capacity factor in 2015,
    # (2) generation is given by IEA 2015 generation values,
    # (3) all 2015 coal capacity is provided by the pc technology.
    # SB Use coal capacity data from Global Coal Plant Tracker (GCPT)

    # historical coal capacity data
    coal_hist <- readSource("GCPT", subtype = "historical") * 1e-03
    coal_hist <- setNames(coal_hist, nm = "pecoal")

    if (grepl("annual", subtype)) {
      output <- new.magpie(cells_and_regions = c(getRegions(embercap)),
                           years = c(min(c(getYears(embercap, as.integer = TRUE), getYears(coal_hist, as.integer = TRUE)))
                                     :max(c(getYears(embercap, as.integer = TRUE), getYears(coal_hist, as.integer = TRUE)))),
                           names = c("pecoal", "pegas", "pebiolc", "pehyd", "peur"),
                           fill = 0)

      output[, intersect(getYears(coal_hist), getYears(output)), "pecoal"] <- coal_hist[, intersect(getYears(coal_hist), getYears(output)), ]

      output[, intersect(getYears(embercap), getYears(output)), getItems(output, dim = 3) != "pecoal"] <- embercap[, intersect(getYears(embercap), getYears(output)), ]

    } else {
      last_ts <- max(intersect(getYears(coal_hist, as.integer = TRUE), seq(2010, 2050, 5)))

      coal_hist <- setNames(coal_hist[, getYears(coal_hist) >= "y2007", ], nm = "pecoal")

      output <- new.magpie(cells_and_regions = c(getRegions(embercap)), years = seq(2010, last_ts, 5),
                           names = c("pecoal", "pegas", "pebiolc", "pehyd", "peur"), fill = 0)

      # Fill in output with GCPT and Ember data, averaging across each 5 (or 3 or 4) year period
      ts_coal <- getYears(coal_hist, as.integer = TRUE)
      ts_ember <- getYears(embercap, as.integer = TRUE)

      for (yr in getYears(output, as.integer = TRUE)) {
        if ((yr + 2) %in% ts_coal) {                      ## Fill in coal separately because data is more recent
          output[, yr, "pecoal"] <- dimSums(coal_hist[, (yr - 2):(yr + 2), ], dim = 2) / 5
        } else if ((yr + 1) %in% ts_coal) {
          output[, yr, "pecoal"] <- dimSums(coal_hist[, (yr - 2):(yr + 1), ], dim = 2) / 4
        } else {
          output[, yr, "pecoal"] <- dimSums(coal_hist[, (yr - 2):yr, ], dim = 2) / 3
        }
        if ((yr + 2) %in% ts_ember) {
          output[, yr, getItems(output, dim = 3) != "pecoal"] <- dimSums(embercap[, (yr - 2):(yr + 2), ], dim = 2) / 5
        } else if ((yr + 1) %in% ts_ember) {
          output[, yr, getItems(output, dim = 3) != "pecoal"] <- dimSums(embercap[, (yr - 2):(yr + 1), ], dim = 2) / 4
        } else {
          output[, yr, getItems(output, dim = 3) != "pecoal"] <- dimSums(embercap[, (yr - 2):yr, ], dim = 2) / 3
        }
      }
    }

    output  <- toolCountryFill(output, fill = 0, verbosity = 2) # fill missing countries

    output <- magclass::add_dimension(output, dim = 3.2, add = "enty", nm = "seel") # add secondary energy dimension

  } else {
    stop("Not a valid subtype!")
  }

  # Returning capacity values
  return(list(x = output, weight = NULL,
              unit = "TW",
              description = description
  ))
}
