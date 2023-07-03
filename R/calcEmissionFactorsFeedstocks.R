#' Calculate emission factors for feedstocks in the chemicals industry
#' using emissions from UNFCCC and energy demands from IEA Energy Balances
#'
#'
#' @md
#' @return A list with a [`magpie`][magclass::magclass] object `x`, `weight`,
#'   `unit`, `description`.
#'
#' @author Falk Benke, Renato Rodrigues, Simón Moreno Leiva
#'
#' @seealso [`calcOutput()`]
#'
#' @importFrom dplyr filter pull select
#' @importFrom madrat getISOlist toolFillYears
#' @importFrom magclass new.magpie getItems
#' @importFrom rlang .data
#' @importFrom tibble as_tibble

#' @export
#'
calcEmissionFactorsFeedstocks <- function() {

  # read source data ----

  # read in FE data from IEA and convert from ktoe to ZJ
  iea <- readSource("IEA", subtype = "EnergyBalances", convert = T)[, , "NECHEM"] %>% collapseDim() * 4.1868e-5 * 1e-3
  iea[iea == 0] <- NA
  remove <- magpply(iea, function(y) all(is.na(y)), MARGIN = 1)
  iea <- iea[!remove, , ]

  # read in emissions from UNFCCC and convert from kt CO2 to GtC
  emi <- readSource("UNFCCC", convert = F)[, , "Table2(I)s1|Total industrial processes|Chemical industry|CO2.kt CO2"] * 1e-6 / 3.666666666667
  remove <- magpply(emi, function(y) all(is.na(y)), MARGIN = 1)
  emi <- emi[!remove, , ]

  # filter by common regions and years
  regions <- intersect(getItems(emi, dim = 1), getItems(iea, dim = 1))
  years <- intersect(getItems(emi, dim = 2), getItems(iea, dim = 2))
  iea <- iea[regions, years, ]
  emi <- emi[regions, years, ]

  # calculate FE demands in chemicals grouped by solids, liquids, gases (ZJ) ----
  fe.demand <- new.magpie(
    cells_and_regions = getItems(iea, dim = 1), years = getItems(iea, dim = 2),
    names = c("solids", "liquids", "gases")
  )

  product_mapping <- toolGetMapping(
    name = "structuremappingIO_outputs_NECHEM.csv",
    type = "sectoral"
  , where = "mappingfolder") %>%
    as_tibble() %>%
    right_join(
      tribble(
        ~remind, ~group,
        "pecoal", "solids",
        "peoil", "liquids",
        "pegas", "gases"
      ),
      "remind"
    ) %>%
    select("products", "group") %>%
    filter(!!sym("products") %in% getItems(iea, dim = 3.1))

  for (g in unique(product_mapping$group)) {
    products <- product_mapping %>%
      filter(g == .data$group) %>%
      select("products") %>%
      pull()

    fe.demand[, , g] <- dimSums(iea[, , products], dim = 3, na.rm = T)
  }

  fe.demand[fe.demand == 0] <- NA

  # calculate carbon used in chemicals (GtC) ----

  # carbon content, taken from REMIND (GtC/ZJ)
  carbon.content <- tribble(
    ~group,      ~emission.factor,
    "solids",    26.1,
    "liquids",   20,
    "gases",     15.3
  )

  carbon.use <- fe.demand

  for (g in getNames(fe.demand)) {
    carbon.use[, , g] <- carbon.content %>%
      filter(g == .data$group) %>%
      pull() * fe.demand[, , g]
  }

  # total carbon used in chemicals in GtC (GtC/ZJ * ZJ = GtC)
  total.carbon <- dimSums(carbon.use, dim = 3, na.rm = T)

  # calculate emission factors (GtC/ZJ) for chemical solids, liquids, gases ----
  # emission factors = carbon used * (chemicals process emissions / total carbon used in chemicals) / energy demand in chemicals
  # GtC * (GtC / GtC ) / ZJ = GtC / ZJ
  emission.factors <- fe.demand
  for (g in getNames(emission.factors)) {
    emission.factors[, , g] <- carbon.use[, , g] * (emi / total.carbon) / fe.demand[, , g]
  }

  # fill missing countries and years from 2025 - 2150 ----

  y <- c(seq(2005, 2060, 5), seq(2070, 2100, 10), seq(2110, 2150, 20))

  # convergence targets assumed for 2050 and missing emission factors based on 2015 USA and Russia chemicals emission factors
  # minimum emission factors assumed to be equal to JPN values to remove outliers from data
  emifact.ranges <- tribble(
    ~group, ~convergence, ~minimum,
    "solids", 5.3, 1.14,
    "liquids", 4, 0.87,
    "gases", 3.2, 0.67
  )

  # magclass with all countries that have at least some values for 2005 - 2020
  x.fill <- new.magpie(
    cells_and_regions = getItems(emission.factors, dim = 1),
    years = y,
    names = c("solids", "liquids", "gases")
  )

  x.fill[, c(2005, 2010, 2015, 2020), ] <- emission.factors[, c(2005, 2010, 2015, 2020), ]

  # magclass with countries without emission factors
  x.empty <- new.magpie(
    cells_and_regions = setdiff(getISOlist(), getItems(emission.factors, dim = 1)),
    years = y,
    names = c("solids", "liquids", "gases")
  )

  for (g in emifact.ranges$group) {
    conv <- emifact.ranges %>%
      filter(g == .data$group) %>%
      select("convergence") %>%
      pull()

    min <- emifact.ranges %>%
      filter(g == .data$group) %>%
      select("minimum") %>%
      pull()

    # maximum emission factors in chemicals assumed to be 2 times the convergence values to remove outliers from data
    max <- conv * 2

    # filter outliers
    clean <- x.fill[, c(2005, 2010, 2015, 2020), g]
    clean[clean < min] <- NA
    clean[clean > max] <- NA

    # set convergence value for all-NAs
    empty <- magpply(clean, function(y) all(is.na(y)), MARGIN = 1)
    clean[empty, , ] <- conv

    # for missing 2005 - 2020 values repeat nearest data
    # fill 2005 NAs, try 2010 first, then 2015, then 2020
    clean[, 2005, ][is.na(clean[, 2005, ])] <- clean[, 2010, ][is.na(clean[, 2005, ])]
    clean[, 2005, ][is.na(clean[, 2005, ])] <- clean[, 2015, ][is.na(clean[, 2005, ])]
    clean[, 2005, ][is.na(clean[, 2005, ])] <- clean[, 2020, ][is.na(clean[, 2005, ])]

    # fill 2010 NAs, try 2015 first, then 2020, then 2005
    clean[, 2010, ][is.na(clean[, 2010, ])] <- clean[, 2015, ][is.na(clean[, 2010, ])]
    clean[, 2010, ][is.na(clean[, 2010, ])] <- clean[, 2020, ][is.na(clean[, 2010, ])]
    clean[, 2010, ][is.na(clean[, 2010, ])] <- clean[, 2005, ][is.na(clean[, 2010, ])]

    # fill 2015 NAs, try 2020 first, then 2010
    clean[, 2015, ][is.na(clean[, 2015, ])] <- clean[, 2020, ][is.na(clean[, 2015, ])]
    clean[, 2015, ][is.na(clean[, 2015, ])] <- clean[, 2010, ][is.na(clean[, 2015, ])]

    # fill 2020 NAs with 2015
    clean[, 2020, ][is.na(clean[, 2020, ])] <- clean[, 2015, ][is.na(clean[, 2020, ])]

    x.fill[, c(2005, 2010, 2015, 2020), g] <- clean

    # set values from 2050 onwards to convergence values: either the fixed value "conv",
    # or the 2020 value if lower than "conv"
    x.conv <- x.fill[, 2020, g]
    x.conv[x.conv > conv] <- conv
    x.fill[, c(2050, 2055, 2060, seq(2070, 2100, 10), seq(2110, 2150, 20)), g] <- x.conv

    # interpolate values between 2020 and 2050
    x.fill[, seq(2005, 2050, 5), g] <- toolFillYears(x.fill[, c(2005, 2010, 2015, 2020, 2050), g], seq(2005, 2050, 5))

    # set emission factors for countries without values to convergence values
    x.empty[, , g] <- conv
  }

  x <- mbind(x.fill, x.empty)

  # create weights ----
  weights <- new.magpie(cells_and_regions = getItems(x, dim = 1), years = getItems(x, dim = 2))
  iea <- readSource("IEA", subtype = "EnergyBalances", convert = T)[, c(2005, 2010, 2015, 2020), "NECHEM"] %>%
    collapseDim() %>%
    dimSums(dim = 3, na.rm = T) * 4.1868e-5 * 1e-3
  weights[, c(2005, 2010, 2015, 2020), ] <- iea
  weights[, c(seq(2025, 2060, 5), seq(2070, 2100, 10), seq(2110, 2150, 20)), ] <- weights[, 2020, ]

  return(
    list(
      x = x,
      weight = weights,
      unit = "GtC/ZJ",
      description = "Emission factors for feedstocks in the chemicals industry"
    )
  )
}
