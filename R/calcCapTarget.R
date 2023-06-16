#' Capacity targets from two sources
#' @description The capacity targets (GW)  at regional level are produced from two different databases-
#' UNFCCC_NDC database, an update of the Rogelj 2017 paper (see readme in inputdata), and REN21 Global Renewables report
#' The UNFCCC_NDC capacity targets are further broken down to conditional and unconditional targets.
#' @author Aman Malik, Oliver Richters
#' @param sources Database source
#' @importFrom dplyr %>% filter


calcCapTarget <- function(sources) {

  convertNAto0 <- function(x) {
    x[is.na(x)] <- 0
    return(x)
  }

  REN21data <- readSource("REN21", subtype = "Capacity")

  if (sources == "REN21") { # only REN21
    description <- "Capacity targets from REN 21(2017) database"
    return(list(x = REN21data, weight = NULL, unit = "GW", description = description))
    # end REN21

  } else { # import NDC capacity target
    listCapacitiesNDC <- list(
      "2018_cond"   = readSource("UNFCCC_NDC", subtype = "Capacity_2018_cond"),
      "2018_uncond" = readSource("UNFCCC_NDC", subtype = "Capacity_2018_uncond"),
      "2021_cond"   = readSource("UNFCCC_NDC", subtype = "Capacity_2021_cond"),
      "2021_uncond" = readSource("UNFCCC_NDC", subtype = "Capacity_2021_uncond"),
      "2022_cond"   = readSource("UNFCCC_NDC", subtype = "Capacity_2022_cond"),
      "2022_uncond" = readSource("UNFCCC_NDC", subtype = "Capacity_2022_uncond"),
      "2023_cond"   = readSource("UNFCCC_NDC", subtype = "Capacity_2023_cond"),
      "2023_uncond" = readSource("UNFCCC_NDC", subtype = "Capacity_2023_uncond")
    )

    listYears   <- lapply(listCapacitiesNDC, getItems, dim = "year") %>% unlist() %>% unique() %>% sort()
    listRegions <- lapply(listCapacitiesNDC, getItems, dim = "region") %>% unlist() %>% unique() %>% sort()

    # expand all magpies to listYears
    expandMagpieYears <- function(x) {
      y <- new.magpie(cells_and_regions = listRegions, years = listYears, names = getNames(x))
      for (year in getItems(x, dim = "year")) {
        y[, year, ] <- x[, year, ]
      }
      return(y)
    }

    lapply(listCapacitiesNDC, expandMagpieYears) %>% mbind() %>% convertNAto0() -> NDC
  }

  if (sources == "UNFCCC_NDC") { # if no other source
    description <- "Capacity targets from Nationally Determined Contributions (NDC)"
    return(list(x = NDC, weight = NULL, unit = "GW", description = description))
  }

  if (sources == "UNFCCC_NDC+REN21+CHN_NUC") { # Additional CHN nuclear target and EV target

    # used to extend non NDC-data to data structure of NDC targets
    extend2Dim <- function(x, dimNames) {
      listx <- rep(list(x), length(dimNames))
      for (i in seq(length(dimNames))) {
        getNames(listx[[i]]) <- paste(dimNames[[i]], getNames(x), sep = ".")
      }
      return(mbind(listx))
    }

    REN21 <- extend2Dim(REN21data, names(listCapacitiesNDC))

    # names of all technologies in REN21 and NDC database + apCarElT
    c(getNames(NDC), getNames(REN21), paste(names(listCapacitiesNDC), "apCarElT", sep = ".")) %>%
      unique() %>% sort() -> techNames

    x <- new.magpie(getItems(REN21, dim = "region"), getYears(REN21), techNames)
    # China's nuclear target
    common_tech <- intersect(
      getNames(REN21) %>% unlist() %>% unique(),
      getNames(NDC)   %>% unlist() %>% unique())
    # for common technologies, take bigger value
    x[, listYears, common_tech] <- pmax(REN21[, listYears, common_tech], NDC[, , common_tech])
    # for tech. in REN21 but not in NDC, take REN21 values
    x[, , setdiff(getNames(REN21), common_tech)]           <- REN21[, , setdiff(getNames(REN21), common_tech)]
    # for tech. in NDC but not in REN21, take NDC values
    x[, getYears(NDC), setdiff(getNames(NDC), common_tech)] <- NDC[, , setdiff(getNames(NDC), common_tech)]
    # additional nuclear policy for CHN. The target is actually 2020 in 58 GW in 2020, but setting this leads to an
    # unfeasible solution in REMIND, therefore setting from 2025 onwards
    x["CHN", seq(2025, 2040, 5), "tnrs"] <- 58 # in GW
    p40_conv_cap_2_MioLDV <-  650 # Conversion factor from capacity of ApCarxxx to Mio Light duty vehicles
    x["CHN", 2020, "apCarElT"] <- 5 / p40_conv_cap_2_MioLDV # China's EV target of 5 Million EVs by 2020.
    x["CHN", seq(2025, 2040, 5), "apCarElT"] <- 15 / p40_conv_cap_2_MioLDV

    # making 2040 targets as good as 2035 targets.
    for (t in seq_len(length(as.vector(x[, 2040, ])))) {
      if (is.na(as.vector(x[, 2040, ])[t])) {
        x[, 2040, ][t] <- as.vector(x[, 2035, ])[t]
      }
    }

    ### Hydrogen Target Start
    # hydrogen capacity targets from national/EU hydrogen strategies

    # Region targets
    reg.map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder") # get H12 regionmapping
    H2Target.reg <- new.magpie(unique(reg.map$RegionCode), getYears(x), "elh2", fill = 0)
    # Electrolyzer capacity target from the EU Hydrogen Strategy
    # https://ec.europa.eu/energy/sites/ener/files/hydrogen_strategy.pdf
    H2Target.reg["EUR", "y2025", ] <-  6
    H2Target.reg["EUR", "y2030", ] <- 40

    # Country Targets
    H2Target.country <- new.magpie(getItems(x, dim = "region"), getYears(x), "elh2", fill = 0)
    # iso countries with a country target that belong to the EU
    country.target.regs <- c("DEU")
    # Germany Target: https://www.bmbf.de/files/die-nationale-wasserstoffstrategie.pdf
    H2Target.country["DEU", "y2030", "elh2"] <- 5

    # aggregate Country Targets to EU
    H2Target.CountryAgg <- toolAggregate(H2Target.country, reg.map, dim = 1)
    # reduce EU target by aggregated country targets
    H2Target <- new.magpie(unique(reg.map$RegionCode), getYears(x), "elh2", fill = 0)
    H2Target["EUR", , "elh2"] <- H2Target.reg["EUR", , "elh2"] - H2Target.CountryAgg["EUR", , "elh2"]

    # # SE VRE Production in 2015 to be used as weight for disaggregation EU target to iso countries
    # SEHistVRE <- dimSums(calcOutput("FE", aggregate = FALSE)[,"y2015",c("SE|Electricity|Solar (EJ/yr)",
    #                                                                 "SE|Electricity|Wind (EJ/yr)")],
    #                      dim = 3)

    # GDP 2015 to be used as weight for disaggregation of EU target to iso coutries
    GDP2015 <- calcOutput("GDPPast", aggregate = FALSE)[, "y2015", ]

    # regionmapping without countries that already have a country target
    CountryCode <- NULL
    reg.map.reduced <- reg.map %>%
                        filter(!CountryCode %in% country.target.regs)
    # disaggregate EU target to iso-countries
    H2Target.disagg <- toolAggregate(H2Target, reg.map.reduced,
                                  from = "RegionCode", to = "CountryCode", dim = 1,
                                  weight = GDP2015[country.target.regs, , , invert = TRUE])
    # bind country target together with disaggregation of EU targets to other countries
    H2Target.out <- magpiesort(mbind(H2Target.country[country.target.regs, , ], H2Target.disagg))
    x <- mbind(x, extend2Dim(H2Target.out, names(listCapacitiesNDC)))
    ### Hydrogen Target End

    description <- "Capacity targets combined from REN 21(2017), NDC database, special case for China nuclear and EV"
    return(list(x = convertNAto0(x), weight = NULL, unit = "GW", description = description))
  } # end sources = UNFCCC_NDC+REN21+CHN_NUC
}
