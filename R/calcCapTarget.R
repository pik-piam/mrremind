#' Capacity targets from two sources
#'
#' @description The capacity targets (GW)  at regional level are produced from two different databases-
#' UNFCCC_NDC database, an update of the Rogelj 2017 paper (see readme in inputdata), and REN21 Global Renewables
#' report. The UNFCCC_NDC capacity targets are further broken down to conditional and unconditional targets.
#'
#' @param sources Database source
#' @author Aman Malik, Oliver Richters
#'
calcCapTarget <- function(sources) {

  if (! sources %in% c("REN21", "UNFCCC_NDC", "UNFCCC_NDC+REN21+CHN_NUC", "NewClimate")) {
    stop("Unknown 'sources' argument.")
  }

  if (sources == "REN21") {
    return(list(x = readSource("REN21", subtype = "Capacity"),
                weight = NULL,
                unit = "GW",
                description = "Capacity targets from REN 21(2017) database"))
  }

  if (sources == "NewClimate") {
    capCond <- readSource("NewClimate", subtype = "Capacity_2025_cond")
    capUncond <- readSource("NewClimate", subtype = "Capacity_2025_uncond")
    x <- mbind(capCond, capUncond)
    return(list(x = x,
                weight = NULL,
                unit = "GW",
                description = "Capacity targets combined from NewClimate Database for Current Policy Scenarios")
    )
  }

  listCapacitiesNDC <- list(
    "2018_cond"   = readSource("UNFCCC_NDC", subtype = "Capacity_2018_cond"),
    "2018_uncond" = readSource("UNFCCC_NDC", subtype = "Capacity_2018_uncond"),
    "2021_cond"   = readSource("UNFCCC_NDC", subtype = "Capacity_2021_cond"),
    "2021_uncond" = readSource("UNFCCC_NDC", subtype = "Capacity_2021_uncond"),
    "2022_cond"   = readSource("UNFCCC_NDC", subtype = "Capacity_2022_cond"),
    "2022_uncond" = readSource("UNFCCC_NDC", subtype = "Capacity_2022_uncond"),
    "2023_cond"   = readSource("UNFCCC_NDC", subtype = "Capacity_2023_cond"),
    "2023_uncond" = readSource("UNFCCC_NDC", subtype = "Capacity_2023_uncond"),
    "2024_cond"   = readSource("UNFCCC_NDC", subtype = "Capacity_2024_cond"),
    "2024_uncond" = readSource("UNFCCC_NDC", subtype = "Capacity_2024_uncond")
  )
  listYears <- lapply(listCapacitiesNDC, getItems, dim = "year") %>% unlist() %>% unique() %>% sort()
  NDC <- purrr::map(listCapacitiesNDC,
                    ~ add_columns(.x, listYears[!listYears %in% getItems(.x, dim = "year")], 2)) %>%
    mbind()
  NDC <- NDC[, sort(getYears(NDC)), ]
  NDC[is.na(NDC)] <- 0

  if (sources == "UNFCCC_NDC") {
    return(list(x = NDC,
                weight = NULL,
                unit = "GW",
                description = "Capacity targets from Nationally Determined Contributions (NDC)"))
  }

  if (sources == "UNFCCC_NDC+REN21+CHN_NUC") {
    # used to extend non NDC-data to data structure of NDC targets
    extend2Dim <- function(x, dimNames) {
      listx <- rep(list(x), length(dimNames))
      for (i in seq_along(dimNames)) {
        getNames(listx[[i]]) <- paste(dimNames[[i]], getNames(x), sep = ".")
      }
      return(mbind(listx))
    }

    REN21data <- readSource("REN21", subtype = "Capacity")
    REN21 <- extend2Dim(REN21data, names(listCapacitiesNDC))

    # names of all technologies in REN21 and NDC database + apCarElT
    techNames <- c(getNames(NDC), getNames(REN21), paste(names(listCapacitiesNDC), "apCarElT", sep = ".")) %>%
      unique() %>%
      sort()

    x <- new.magpie(getItems(REN21, dim = "region"), getYears(REN21), techNames)
    # China's nuclear target
    common_tech <- intersect(getNames(REN21) %>% unlist() %>% unique(),
                             getNames(NDC)   %>% unlist() %>% unique())
    # for common technologies, take bigger value
    x[, listYears, common_tech] <- pmax(REN21[, listYears, common_tech], NDC[, , common_tech])
    # for tech. in REN21 but not in NDC, take REN21 values
    x[, , setdiff(getNames(REN21), common_tech)] <- REN21[, , setdiff(getNames(REN21), common_tech)]
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
    reg.map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")
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

    # GDP 2015 to be used as weight for disaggregation of EU target to iso coutries
    GDP2015 <- calcOutput("GDPPast", aggregate = FALSE)[, "y2015", ]

    # regionmapping without countries that already have a country target
    reg.map.reduced <- reg.map %>% dplyr::filter(!.data$CountryCode %in% country.target.regs)
    # disaggregate EU target to iso-countries
    H2Target.disagg <- toolAggregate(H2Target,
                                     reg.map.reduced,
                                     from = "RegionCode",
                                     to = "CountryCode",
                                     dim = 1,
                                     weight = GDP2015[country.target.regs, , , invert = TRUE])
    # bind country target together with disaggregation of EU targets to other countries
    H2Target.out <- magpiesort(mbind(H2Target.country[country.target.regs, , ], H2Target.disagg))
    x <- mbind(x, extend2Dim(H2Target.out, names(listCapacitiesNDC)))

    x[is.na(x)] <- 0

    return(list(x = x,
                weight = NULL,
                unit = "GW",
                description = glue::glue("Capacity targets combined from REN 21(2017), NDC database, special case \\
                                         for China nuclear and EV")))
  }
}
