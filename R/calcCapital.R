#' Calculate macroeconomic capital stock
#'
#' Compute macroeconomic capital stock based on capital intensities from PWT and GDP scenarios from mrdrivers.
#' The PWT capital intensities are used up until 2010. After that, the capital intensities converge towards
#' that of Japan in 2010, at speeds that vary across scenarios. The final capital stocks are the product
#' of the capital intensities and the gdp scenarios from mrdrivers.
#'
#' @param scenario GDP and pop scenarios. Passed to [mrdrivers::calcGDP()].
#'
#' @seealso \itemize{
#'   \item See the vignette \code{vignette("scenarios", "mrdrivers")} for information on the GDP scenarios.
#'   \item [readPWT()] for information on the PWT version used.
#' }
#' @inherit madrat::calcOutput return
calcCapital <- function(scenario) {
  # Get capital intensities (capital over GDP) from PWT
  kPWT <- readSource("PWT")[, , "rkna"]
  gdpPWT <- readSource("PWT")[, , "rgdpna"]
  kIntPWT <- kPWT / setNames(gdpPWT, NULL)

  # Get GDP from mrdrivers (which differs from GDP in PWT). Make sure SSP2 is always returned.
  gdp <- calcOutput("GDP", scenario = unique(c(scenario, "SSP2")), aggregate = FALSE, years = seq(1995, 2150, 5))

  # Define reference capital intensity, and the convergence time in years, of the countries capital intensities towards
  # that reference, for the different GDP scenarios. The convergence assumptions should follow the SSP narratives.
  # Convergence starts after 2010.
  kIntRef <- kIntPWT["JPN", 2010, ] %>% as.numeric()
  convTime <- c("SSP1" = 150, "SSP2" = 250, "SSP3" = 150, "SSP4" = 300, "SSP5" = 150)

  # If required, add any assumption on convergence times for non SSP scenarios. By default use the SSP2 convTime.
  if (any(! getNames(gdp) %in% names(convTime))) {
    scens <- getNames(gdp)[! getNames(gdp) %in% names(convTime)]
    message(glue::glue("Adding {paste(scens, collapse = ', ')} assumptions as copies of SSP2."))
    addConvTime <- purrr::map(scens, ~`names<-`(convTime["SSP2"], .x)) %>% purrr::list_c()
    convTime <- c(convTime, addConvTime)
  }


  # Create kInt magpie object with the same dimension as gdp, and assign the PWT capital intensities for the
  # historic years until 2010.
  kInt <- gdp * NaN
  hy <- c(1995, 2000, 2005, 2010)
  kInt[, hy, ] <- kIntPWT[, hy, ]
  # For future years (after 2010), linearly converge towards kIntRef
  ## Special convergence for SSP3: use a 25% higher reference value, and let JPN reach this value by 2060.
  fy <- setdiff(getYears(gdp, as.integer = TRUE), hy)
  for (t in fy) {
    for (s in getNames(kInt)) {
      # Special case for SSP3
      if (s == "SSP3") {
        kInt[, t, s] <-  kInt[, 2010, s] + (kIntRef * 1.25 - kInt[, 2010, s]) * (t - 2010) / convTime[s]
        if (t <= 2060) {
          kInt["JPN", t, s] <-  kInt["JPN", 2010, s] + (kIntRef * 1.25 - kInt["JPN", 2010, s]) * (t - 2010) / 50
        } else {
          kInt["JPN", t, s] <-  kInt["JPN", 2060, s]
        }
      # For all other sceanarios
      } else {
        kInt[, t, s] <-  kInt[, 2010, s] + (kIntRef - kInt[, 2010, s]) * (t - 2010) / convTime[s]
      }

    }
  }

  # Use regional average for countries missing data
  ## To get regional aggregate values for all countries: first aggregate, using only non-missing countries and
  ## partrel = TRUE, then disaggregate again. (Faster than toolFillWithRegionAvg).
  nmc <- getItems(kInt, dim = 1)[!is.nan(kInt[, 2010, "SSP2"])]
  map <- toolGetMapping("regionmappingH12.csv", where = "mappingfolder", type = "regional")
  kIntReg <- toolAggregate(kInt[nmc, , ], rel = map, weight = gdp[nmc, , ], partrel = TRUE) %>%
    toolAggregate(rel = map, from = "RegionCode", to = "CountryCode")
  mc <- setdiff(getItems(kInt, dim = 1), nmc)
  kInt[mc, , ] <- kIntReg[mc, , ]

  # Compute capital stock
  k <- kInt * gdp * 1e-6

  # Add industry energy efficiency capital stocks (passing 2015 SSP2 capital stock as "kap")
  kap <- k[, 2015, "SSP2"] %>% tibble::as_tibble() %>% dplyr::select("iso3c", "kap" = "value")
  EEK <- calcOutput("Industry_EEK", kap = kap, scenarios = scenario, aggregate = FALSE, years = getYears(k))

  # Modify names to differentiate the macroeconomic capital "kap" from EEK capital stocks.
  getNames(k) <- paste0(getNames(k), ".kap")
  getSets(k) <- c("iso3c", "year", "ssp", "variable")
  x <- mbind(k, EEK)

  list(x = x,
       weight = NULL,
       unit = glue::glue("trillion US${mrdrivers::toolGetUnitDollar(returnOnlyBase = TRUE)}"),
       description = "Capital stock computed using the capital/GDP ratio from PWT, and GDP scenarios from mrdrivers.")
}
