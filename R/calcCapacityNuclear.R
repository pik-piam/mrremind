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
  out <- new.magpie(getItems(x, dim = 1), seq(2020, 2040, 5), "tnrs")

  # Historical data ----
  # allocate data and convert from MW into TW

  # total capacity in 2020: snapshot of operable reactors in April 2020
  out[, 2020, ] <- x[, 2020, "REACTORS OPERABLE (MWe net)"] / 10^6

  # total capacity in 2025: snapshot of operable reactors in November 2025
  out[, 2025, ] <- x[, 2025, "REACTORS OPERABLE (MWe net)"] / 10^6

  # Near-Term data ----

  # We assume that until 2030, 80% of the reactors that are currently under
  # construction can be built.
  # Until 2035, the remaining 20% under construction can be built, as well as
  # 50% of the planned ones, and 30% of the proposed ones.
  # Until 2040, the remaining 50% of the planned ones as well as the remaining
  # 70% of the proposed ones could be built.
  # There also might be completely new plans to build nuclear plants, which is
  # likely easier if a country already has some nuclear - so we add 10% of the
  # 2025 operating capacity - or if they built some between 2025 and 2035 - so
  # we add 30% of the additions between 2025 and 2035.

  out[, 2030, ] <- 0.8 * x[, 2025, "REACTORS UNDER CONSTRUCTION (MWe gross)"] / grossnet / 10^6

  out[, 2035, ] <- (
    0.2 * x[, 2025, "REACTORS UNDER CONSTRUCTION (MWe gross)"] / grossnet +
      0.5 * x[, 2025, "REACTORS PLANNED (MWe gross)"] / grossnet +
      0.3 * x[, 2025, "REACTORS PROPOSED (MWe gross)"] / grossnet
  ) / 10^6 # convert to TW

  out[, 2040, ] <- (
    0.5 * x[, 2025, "REACTORS PLANNED (MWe gross)"] / grossnet +
      0.7 * x[, 2025, "REACTORS PROPOSED (MWe gross)"] / grossnet +
      # add 10% of operating 2025 capacities - this is to represent that
      # countries may develop new nuclear plants, and if you have already
      # nuclear power plants it is easier to build new ones.
      0.1 * x[, 2025, "REACTORS OPERABLE (MWe net)"] / grossnet
  ) / 10^6 + # convert to TW
    # a percentate of max capacity additions from 2025 to 2030 and from 2030 to 2035
    # is added to include countries that currently have little nuclear but scaling
    # it up until 2035 - they should also be able to continue their upscaling.
    0.3 * out[, 2030, ] +
    0.3 * out[, 2035, ]


  # Additional estimates ----

  # Emerging Nuclear Energy Countries, Updated Tuesday, 18 November 2025
  # http://www.world-nuclear.org/information-library/country-profiles/others/emerging-nuclear-energy-countries.aspx
  # special treatment to avoid infeasibility and open nuclear potential for Africa
  # these countries can have additions of 500MW in 2035 and 2GW in 2040 periods

  # Power reactors under construction: Bangladesh, Egypt, Turkey
  out[c("BGD", "EGY", "TUR"), 2035, ] <- pmax(out[c("BGD", "EGY", "TUR"), 2035, ], 0.0005)
  out[c("BGD", "EGY", "TUR"), 2040, ] <- pmax(out[c("BGD", "EGY", "TUR"), 2040, ], 0.002)

  # Power reactors planned: Poland
  out["POL", 2035, ] <- pmax(out["POL", 2035, ], 0.0005)
  out["POL", 2040, ] <- pmax(out["POL", 2040, ], 0.002)

  # Power reactors proposed: Ghana, Kazakhstan, Saudi Arabia, Uzbekistan.
  out[c("GHA", "KAZ", "SAU", "UZB"), 2035, ] <- pmax(out[c("GHA", "KAZ", "SAU", "UZB"), 2035, ], 0.0005)
  out[c("GHA", "KAZ", "SAU", "UZB"), 2040, ] <- pmax(out[c("GHA", "KAZ", "SAU", "UZB"), 2040, ], 0.002)

  # Provisional plans, commitment pending or deferred: Algeria, Azerbaijan,
  # El Salvador, Estonia, Ethiopia, Ghana (dupl), Kenya, Laos, Morocco, Nigeria,
  # Philippines, Rwanda, Sri Lanka, Sudan, Thailand, Indonesia,
  # Saudi Arabia (dupl), Vietnam

  ctry <- c("DZA", "AZE", "SLV", "EST", "ETH", "KEN", "LAO", "MAR", "NGA", "PHL",
            "RWA", "LKA", "SDN", "THA", "IDN", "VNM")

  out[ctry, 2035, ] <- pmax(out[ctry, 2035, ], 0.0005)
  out[ctry, 2040, ] <- pmax(out[ctry, 2040, ], 0.002)

  # Barakah reactors 1&2 were finished in 2020/2021
  out["ARE", 2020, ] <- 3000 / 10^6

  # provide some leeway for REMIND to keep the 1.85 GW running even though
  # REMIND requires technical depreciation, which would reduce the capacity
  out["ZAF", 2030, ] <- 400 / 10^6

  return(list(x = out, weight = NULL, unit = "TW",
              description = "capacity of operating nuclear plants in 2020 and 2025,
              upper limits of capacity additions for 2030, 2025 and 2040")
  )

}
