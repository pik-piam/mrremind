#' Calculate macroeconomic capital stock
#'
#' Compute macroeconomic capital stock based on capital intensities from PWT and GDP scenarios from [mrdrivers].
#' The PWT capital intensities are used up until 2010. After that, the capital intensities converge towards
#' that of Japan in 2010, at speeds that vary across scenarios. The final capital stocks are the product
#' of the capital intensities and the gdp scenarios from [mrdrivers].
#'
#' @export
#' @seealso \itemize{
#'   \item See the vignette \code{vignette("scenarios", "mrdrivers")} for information on the GDP scenarios.
#'   \item [readPWT()] for information on the PWT version used.
#' }
#' @inherit madrat::calcOutput return
calcCapital <- function() {

  # Get capital intensities (capital over GDP) from PWT
  kPWT <- readSource("PWT")[, , "rkna"]
  gdpPWT <- readSource("PWT")[, , "rgdpna"]
  kIntPWT <- kPWT / setNames(gdpPWT, NULL)

  # Get GDP from mrdrivers (which differs from GDP in PWT)
  gdp <- calcOutput("GDP", naming = "scenario", aggregate = FALSE, years = seq(1995, 2150, 5))

  # Define reference capital intensity, and the convergence time in years, of the countries capital intensities towards
  # that reference, for the different GDP scenarios. The convergence assumptions should follow the SSP narratives.
  # Convergence starts after 2010.
  kIntRef <- kIntPWT["JPN", 2010, ] %>% as.numeric()
  convTime <- c("SSP1" = 150, "SSP2" = 250, "SSP3" = 500, "SSP4" = 300, "SSP5" = 150,
                "SDP" = 150, "SDP_EI" = 150, "SDP_RC" = 150, "SDP_MC" = 150, "SSP2EU" = 250)

  # Create kInt magpie object with the same dimension as gdp, and assign the PWT capital intensities for the
  # historic years until 2010.
  kInt <- gdp * NaN
  hy <- c(1995, 2000, 2005, 2010)
  kInt[, hy, ] <- kIntPWT[, hy, ]
  # For future years (after 2010), linearly converge towards kIntRef
  fy <- setdiff(getYears(gdp, as.integer = TRUE), hy)
  for (t in fy) {
    for (s in getNames(kInt)) {
      kInt[, t, s] <-  kInt[, 2010, s] + (kIntRef - kInt[, 2010, s]) * (t - 2010) / convTime[s]
    }
  }

  # Use regional average for countries missing data
  ## To get regional aggregate values for all countries: first aggregate, using only non-missing countries and
  ## partrel = TRUE, then disaggregate again.
  nmc <- getItems(kInt, dim = 1)[!is.nan(kInt[, 2010, "SSP2"])]
  map <- toolGetMapping("regionmappingH12.csv")
  kIntReg <- toolAggregate(kInt[nmc, , ], rel = map, weight = gdp[nmc, , ], partrel = TRUE) %>%
    toolAggregate(rel = map, from = "RegionCode", to = "CountryCode")
  mc <- setdiff(getItems(kInt, dim = 1), nmc)
  kInt[mc, , ] <- kIntReg[mc, , ]

  # Compute capital stock
  k <- kInt * gdp * 1e-6

  # Add industry energy efficiency capital stocks (passing 2015 SSP2 capital stock as "kap")
  kap <- k[, 2015, "SSP2"] %>% tibble::as_tibble() %>% dplyr::select("iso3c", "kap" = "value")
  EEK <- calcOutput("Industry_EEK", kap = kap, aggregate = FALSE, years = getYears(k))

  # Modify names to match "all_demScen" in remind ("gdp_" prefix), and differentiate the macroeconomic capital "kap"
  # from EEK capital stocks.
  getNames(k) <- paste0("gdp_", getNames(k), ".kap")
  getSets(k) <- c("iso3c", "year", "ssp", "variable")
  x <- mbind(k, EEK)

  list(x = x,
       weight = NULL,
       unit = "trillion US$2017",
       description = "Capital stock computed using the capital/GDP ratio from PWT, and GDP scenarios from mrdrivers.")
}
