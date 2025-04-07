#' @title calc Capacity
#' @description provides historical capacity values in TW
#'
#' @param subtype data subtype. Either "capacityByTech" or "capacityByPE"
#' @return magpie object of  capacity data
#' @author Renato Rodrigues, Stephen Bi, Fabrice Lécuyer
#' @examples
#' \dontrun{
#' calcOutput("Capacity", subtype = "capacityByTech")
#' }
calcCapacity <- function(subtype) {

  if (subtype == "capacityByTech") {
    description <- "Historical capacity by technology."


    ###### Use IRENA data for world renewables capacity
    # Year: 2000-2023
    mappingIRENA <- tibble::tribble(
      ~irena,                     ~remind,
      "Geothermal",               "geohdr",
      "Renewable hydropower",     "hydro",
      "Onshore wind energy",      "windon",
      "Offshore wind energy",     "windoff",
      "Solar photovoltaic",       "spv",
      "Concentrated solar power", "csp"
    )

    capIRENA <- readSource(type = "IRENA", subtype = "Capacity")[, , mappingIRENA$irena] %>% # selecting relevant variables
      toolAggregate(dim = 3, rel = mappingIRENA, from = "irena", to = "remind") * # renaming to remind names
      1e-6 # converting MW to TW


    ###### Use Openmod capacity values updated by the LIMES team for the European countries
    # Year: 2015
    mappingOpenmod <- tibble::tribble(
      ~openmod, ~remind,
      "tnr",    "tnrs",
      "ngcc",   "ngcc",
      "ngt",    "ngt",
      "oil",    "dot"
    )

    capOpenmod <- readSource(type = "Openmod")
    capOpenmod <- capOpenmod[c("FIN", "NOR", "SWE", "EST", "LVA", "LTU", "DNK", "GBR", "IRL", "NLD", "POL",
          "DEU", "BEL", "LUX", "CZE", "SVK", "AUT", "CHE", "HUN", "ROU", "SVN", "FRA", "HRV", "BGR", "ITA",
          "ESP", "PRT", "GRC"), , mappingOpenmod$openmod] %>% # selecting countries and variables
      toolAggregate(dim = 3, rel = mappingOpenmod, from = "openmod", to = "remind") * # renaming to remind names
      1e-3 # converting GW to TW


    ###### Use WEO 2017 data for countries: "USA","BRA","RUS","CHN","IND","JPN"
    # Year: 2015
    mappingWEO <- tibble::tribble(
      ~weo,      ~remind,
      "Nuclear", "tnrs",
      "Oil",     "dot"
    )

    capWEO <- readSource(type = "IEA_WEO", subtype = "Capacity")
    capWEO <- capWEO[c("USA", "BRA", "RUS", "CHN", "IND", "JPN"), 2015, mappingWEO$weo] %>% # selecting relevant data
      toolAggregate(dim = 3, rel = mappingWEO, from = "weo", to = "remind") * # renaming to remind names
      1e-3 # converting GW to TW


    ###### Use GlobalEnergyMonitor to project renewable capacities into 2025
    # Note: replace with consolidated data when available
    # Year: 2025
    mappingGEM <- tibble::tribble(
      ~gem,                            ~remind,
      "Cap|Electricity|Geothermal",    "geohdr",
      "Cap|Electricity|Hydro",         "hydro",
      "Cap|Electricity|Wind|Onshore",  "windon",
      "Cap|Electricity|Wind|Offshore", "windoff",
      "Cap|Electricity|Solar|PV",      "spv",
      "Cap|Electricity|Solar|CSP",     "csp"
    )

    capGEM <- readSource("GlobalEnergyMonitor")[, , mappingGEM$gem] %>% # selecting relevant variables
      toolAggregate(dim = 3.2, rel = mappingGEM, from = "gem", to = "remind") %>% # renaming to remind names
      collapseDim(keepdim = "status") * # removing useless dimensions
      1e-3 # converting GW to TW

    # as GEM only includes big plants, rescale to IRENA values
    scalingFactor <- collapseDim(capIRENA[, 2020,] / capGEM[, 2020, "operating"], dim = "status")
    scalingFactor[is.na(scalingFactor) | scalingFactor > 10] <- 1 # set errors to 1
    capGEM[, 2025,] <- capGEM[, 2025,] * scalingFactor
    capGEM <- capGEM[, 2025, , drop = FALSE]

    # estimate probability of success depending on project status
    capGEM[, , "operating"] <-  capGEM[, , "operating"] +
                         0.75 * capGEM[, , "construction"] +
                         0.5  * capGEM[, , "pre-construction"] +
                         0.15 * capGEM[, , "announced"]

    capGEM <- capGEM[, , "operating", drop = FALSE] # remove status
    capGEM <- dimSums(capGEM, dim = "status", na.rm = TRUE)  # remove NAs
    #capGEM[capGEM < 20e-6] <- 0  # remove values < 20MW as this is the GEM threshold


    ###### Use manual data with expert judgement

    # CG: fix CHA gas power capacities: 97 GW by September 2020 (Oxford Institute for Energy Studies:
    # Natural gas in China’s power sector: Challenges and the road ahead
    # (https://www.oxfordenergy.org/wpcms/wp-content/uploads/2020/12/Insight-80-Natural-gas-in-Chinas-power-sector.pdf)
    # ~50% is peaking (= ngt), the other 50 is called cogeneration but contains ngcc and gaschp
    # for 2018-2022, take 90GW, 90GW*0.5=50GW ngt, the rest is split between ngcc and gaschp 70:30 (from IEA EB energy output)
    capManual <- as.magpie(tibble::tribble(
      ~region,   ~year,   ~data,      ~value,
      "CHN",     2010,    "gaschp",   0.004,
      "CHN",     2015,    "gaschp",   0.011,
      "CHN",     2020,    "gaschp",   0.014,
      "CHN",     2010,    "ngcc",     0.009,
      "CHN",     2015,    "ngcc",     0.025,
      "CHN",     2020,    "ngcc",     0.032,
      "CHN",     2010,    "ngt",      0.013,
      "CHN",     2015,    "ngt",      0.036,
      "CHN",     2020,    "ngt",      0.045
    ))


    ###### merge capacity data from different sources
    # the order matters as latter will overwrite former
    datasets <- list(capIRENA, capOpenmod, capWEO, capGEM, capManual)
    output <- new.magpie(
      cells_and_regions = unique(unlist(lapply(datasets, getRegions))),
      years = sort(unique(unlist(lapply(datasets, getYears)))),
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

    ## Primary Energies
    mappingEmber <- tibble::tribble(
      ~ember,       ~remind,
      "Biomass",    "pebiolc",
      "Coal",       "pecoal",
      "Gas",        "pegas",
      "Oil",        "peoil",
      "Hydro",      "pehyd",
      "Nuclear",    "peur",
      "Solar",      "pesol",
      "Wind",       "pewin"
    )

    capEmber <- calcOutput("Ember", subtype = "capacity", aggregate = FALSE)
    capEmber <- setNames(capEmber,
                         nm = gsub("Cap|Electricity|", "",
                                   gsub(" (GW)", "",
                                        getNames(capEmber), fixed = TRUE), fixed = TRUE))

    # aggregating primary energies to REMIND naming convention
    capEmber <- toolAggregate(capEmber[, , mappingEmber$ember], rel = mappingEmber, from = "ember",
                              to = "remind", dim = 3.1)
    capEmber <- capEmber * 1E-03 # converting GW to TW

    capEmber <- capEmber[, , c("peur", "pegas", "pebiolc", "pehyd")] # pegas is handled at technology level

    # estimating lower bound coal capacity to remaining countries assuming
    # (1) capacity factors are given by REMIND pc capacity factor in 2015,
    # (2) generation is given by IEA 2015 generation values,
    # (3) all 2015 coal capacity is provided by the pc technology.
    # SB Use coal capacity data from Global Coal Plant Tracker (GCPT)

    # historical coal capacity data
    coalHist <- readSource("GCPT", subtype = "historical") * 1e-03
    coalHist <- setNames(coalHist, nm = "pecoal")

    if (grepl("annual", subtype)) {
      output <- new.magpie(cells_and_regions = c(getRegions(capEmber)),
                           years = c(min(c(getYears(capEmber, as.integer = TRUE), getYears(coalHist, as.integer = TRUE)))
                                     :max(c(getYears(capEmber, as.integer = TRUE), getYears(coalHist, as.integer = TRUE)))),
                           names = c("pecoal", "pegas", "pebiolc", "pehyd", "peur"),
                           fill = 0)

      output[, intersect(getYears(coalHist), getYears(output)), "pecoal"] <- coalHist[, intersect(getYears(coalHist), getYears(output)), ]

      output[, intersect(getYears(capEmber), getYears(output)), getItems(output, dim = 3) != "pecoal"] <- capEmber[, intersect(getYears(capEmber), getYears(output)), ]

    } else {
      yearLast <- max(intersect(getYears(coalHist, as.integer = TRUE), seq(2010, 2050, 5)))

      coalHist <- setNames(coalHist[, getYears(coalHist) >= "y2007", ], nm = "pecoal")

      output <- new.magpie(cells_and_regions = c(getRegions(capEmber)), years = seq(2010, yearLast, 5),
                           names = c("pecoal", "pegas", "pebiolc", "pehyd", "peur"), fill = 0)

      # Fill in output with GCPT and Ember data, averaging across each 5 (or 3 or 4) year period
      yearsCoal <- getYears(coalHist, as.integer = TRUE)
      yearsEmber <- getYears(capEmber, as.integer = TRUE)

      for (yr in getYears(output, as.integer = TRUE)) {
        if ((yr + 2) %in% yearsCoal) {                      ## Fill in coal separately because data is more recent
          output[, yr, "pecoal"] <- dimSums(coalHist[, (yr - 2):(yr + 2), ], dim = 2) / 5
        } else if ((yr + 1) %in% yearsCoal) {
          output[, yr, "pecoal"] <- dimSums(coalHist[, (yr - 2):(yr + 1), ], dim = 2) / 4
        } else {
          output[, yr, "pecoal"] <- dimSums(coalHist[, (yr - 2):yr, ], dim = 2) / 3
        }
        if ((yr + 2) %in% yearsEmber) {
          output[, yr, getItems(output, dim = 3) != "pecoal"] <- dimSums(capEmber[, (yr - 2):(yr + 2), ], dim = 2) / 5
        } else if ((yr + 1) %in% yearsEmber) {
          output[, yr, getItems(output, dim = 3) != "pecoal"] <- dimSums(capEmber[, (yr - 2):(yr + 1), ], dim = 2) / 4
        } else {
          output[, yr, getItems(output, dim = 3) != "pecoal"] <- dimSums(capEmber[, (yr - 2):yr, ], dim = 2) / 3
        }
      }
    }

    output  <- toolCountryFill(output, fill = 0, verbosity = 2) # fill missing countries

    output <- magclass::add_dimension(output, dim = 3.2, add = "enty", nm = "seel") # add secondary energy dimension

  } else {
    stop("Not a valid subtype!")
  }

  # Returning capacity values
  return(list(
    x = output,
    weight = NULL,
    unit = "TW",
    description = description
  ))
}
