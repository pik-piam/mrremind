#' Historical nuclear capacities and near-term capacity addition bounds for REMIND
#'
#' @description use historical nuclear electricity generation capacity and
#' calculate near-term estimates based on current nuclear power project status
#' per country.
#' @author Robert Pietzcker, Christoph Bertram, Aman Malik, Pascal Weigmann

calcCapacityNuclear <- function() {

  # additional assumption: gross-net losses vary between 3 (Shin-Kori Unit 3)
  # and 12 (Fuqing Unit 5) %, so 5% seems good assumption:
  grossnet <- 1.05

  x <- readSource("IAEA")
  out <- new.magpie(getRegions(x), c(2015, 2020, 2025, 2030, 2035), "tnrs")

  # Historical data ####
  # allocate data and convert from MW into TW
  # total capacity in 2015: snapshot of operable reactors in early 2016
  out[, 2015, ] <- x[, 2016, "REACTORS OPERABLE (MWe net)"] / 10^6

  # total capacity in 2020: snapshot of operable reactors in April 2020
  out[, 2020, ] <- x[, 2020, "REACTORS OPERABLE (MWe net)"] / 10^6

  # total capacity in 2025: snapshot of operable reactors in November 2025
  out[, 2025, ] <- x[, 2025, "REACTORS OPERABLE (MWe net)"] / 10^6


  # Near-Term data ####
  # maximum capacity addition for the 5 years of the 2030 period based on 2025:
  out[, 2030, ] <- (
    0.3 * x[, 2025, "REACTORS UNDER CONSTRUCTION (MWe gross)"] / grossnet
  + 0.4 * x[, 2025, "REACTORS PLANNED (MWe gross)"] / grossnet
  + 0.3 * x[, 2025, "REACTORS PROPOSED (MWe gross)"] / grossnet
  + 0.1 * x[, 2025, "REACTORS OPERABLE (MWe net)"]  # represents extensions
  ) / 10^6  # convert to TW

  # maximum capacity addition for 5 years in 2035 period:
  out[, 2035, ] <- (
      0.6 * x[, 2025, "REACTORS PLANNED (MWe gross)"] / grossnet
    + 0.7 * x[, 2025, "REACTORS PROPOSED (MWe gross)"] / grossnet
    + 0.1 * x[, 2025, "REACTORS OPERABLE (MWe net)"]  # represents extensions
    ) / 10^6  # convert to TW


  # Additional estimates ####

  # Emerging Nuclear Energy Countries, Updated Tuesday, 18 November 2025
  # http://www.world-nuclear.org/information-library/country-profiles/others/emerging-nuclear-energy-countries.aspx
  # special treatment to avoid infeasibility and open nuclear potential for Africa
  # these countries can have additions of 500MW in 2030 and 2GW in 2035 periods
  # commented out if calculations above result in equal or higher values

  # Power reactors under construction: Bangladesh, Egypt, Turkey
  #out["BGD", 2030, ]
  #out["EGY", 2030, ]
  #out["TUR", 2030, ]

  out["BGD", 2035, ] <- 0.002
  out["EGY", 2035, ] <- 0.002
  #out["TUR", 2035, ]

  # Power reactors planned: Poland
  #out["POL", 2030, ]
  #out["POL", 2035, ]

  # Power reactors proposed: Ghana, Kazakhstan, Saudi Arabia, Uzbekistan.
  out["GHA", 2030, ] <- 0.0005
  #out["KAZ", 2030, ]
  #out["SAU", 2030, ]
  #out["UZB", 2030, ]

  out["GHA", 2035, ] <- 0.002
  #out["KAZ", 2035, ]
  out["SAU", 2035, ] <- 0.002
  out["UZB", 2035, ] <- 0.002

  # Provisional plans, commitment pending or deferred: Algeria, Azerbaijan,
  # El Salvador, Estonia, Ethiopia, Ghana (dupl), Kenya, Laos, Morocco, Nigeria,
  # Philippines, Rwanda, Sri Lanka, Sudan, Thailand, Indonesia,
  # Saudi Arabia (dupl), Vietnam
  out["DZA", 2030, ] <- 0.0005
  out["AZE", 2030, ] <- 0.0005
  out["SLV", 2030, ] <- 0.0005
  out["EST", 2030, ] <- 0.0005
  out["ETH", 2030, ] <- 0.0005
  out["KEN", 2030, ] <- 0.0005
  out["LAO", 2030, ] <- 0.0005
  out["MAR", 2030, ] <- 0.0005
  out["NGA", 2030, ] <- 0.0005
  out["PHL", 2030, ] <- 0.0005
  out["RWA", 2030, ] <- 0.0005
  out["LKA", 2030, ] <- 0.0005
  out["SDN", 2030, ] <- 0.0005
  out["THA", 2030, ] <- 0.0005
  out["IDN", 2030, ] <- 0.0005
  out["VNM", 2030, ] <- 0.0005

  out["DZA", 2035, ] <- 0.002
  out["AZE", 2035, ] <- 0.002
  out["SLV", 2035, ] <- 0.002
  out["EST", 2035, ] <- 0.002
  out["ETH", 2035, ] <- 0.002
  out["KEN", 2035, ] <- 0.002
  out["LAO", 2035, ] <- 0.002
  out["MAR", 2035, ] <- 0.002
  out["NGA", 2035, ] <- 0.002
  out["PHL", 2035, ] <- 0.002
  out["RWA", 2035, ] <- 0.002
  out["LKA", 2035, ] <- 0.002
  out["SDN", 2035, ] <- 0.002
  out["THA", 2035, ] <- 0.002
  out["IDN", 2035, ] <- 0.002
  out["VNM", 2035, ] <- 0.002

  return(list(x = out, weight = NULL, unit = "TW",
              description = "capacity of operating nuclear plants in 2015, 2020
              and 2025, upper limits of capacity additions for 2030 and 2035")
         )

}
