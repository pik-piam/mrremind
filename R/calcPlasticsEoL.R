#' calculates projections for the end of life fate of plastic waste
#' in particular, calculates the share that is incinerated
#'
#' @md
#' @return A list with a [`magpie`][magclass::magclass] object `x`, `weight`,
#'   `unit`, `description`.
#'
#' @author Falk Benke, Simón Moreno Leiva
#'
#' @seealso [`calcOutput()`]
#'
#' @importFrom dplyr filter pull select
#' @importFrom madrat getISOlist toolFillYears
#' @importFrom magclass new.magpie getItems
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @importFrom quitte calc_addVariable
#'
#' @export
#'
calcPlasticsEoL <- function() {
  # read source data ----

  # read in projections for plastics end-of-life flows EJ/yr
  x <- readSource("Stegmann2022")

  # select variables that we are going to need to calculate incineration shares
  selection <- c(
    "Plastics|Waste|Buildings & Construction",
    "Plastics|Waste|Consumer Products",
    "Plastics|Waste|Electrical & Electronic (products)",
    "Plastics|Waste|Industrial Machinery",
    "Plastics|Waste|Other",
    "Plastics|Waste|Packaging",
    "Plastics|Waste|Textiles",
    "Plastics|Waste|Transportation",
    "Plastics|End of Life|Waste to Energy"
  )

  x <- x[, , selection, pmatch = TRUE]

  y <- as.quitte(x)
  # calculate total plastic waste
  plasticsEoL <- y %>%
    # we could have switches to include a circular scenario but it would probably
    # require a different scenario to calibrate
    filter(!!sym("scenario") %in% c("SSP2")) %>%
    calc_addVariable(
      "Plastics|Waste|Total" = "
                        `Plastics|Waste|Buildings & Construction` +
                        `Plastics|Waste|Consumer Products` +
                        `Plastics|Waste|Electrical & Electronic (products)` +
                        `Plastics|Waste|Industrial Machinery`+
                        `Plastics|Waste|Other`+
                        `Plastics|Waste|Packaging`+
                        `Plastics|Waste|Textiles`+
                        `Plastics|Waste|Transportation`
                        ",
      units = "PJ/yr"
    )

  # calculate share (0 to 1) of waste that gets incinerated
  incinerationShares <- plasticsEoL %>%
    calc_addVariable(
      "Plastics|End of Life|Incineration share" = "
                        `Plastics|End of Life|Waste to Energy`/
                        `Plastics|Waste|Total`
                        ",
      units = "fraction", only.new = TRUE
    ) %>%
    # remove unused dimensions
    select(-"model", -"scenario", -"variable", -"unit") %>%
    suppressWarnings(interpolate_missing_periods(seq(2050, 2060, 5), method = "linear"))

  # as magpie
  x <- as.magpie(incinerationShares)

  # fill post 2100 years
  # post 2100 = 2100
  xNew <- new.magpie(
    cells_and_regions = getItems(x, dim = 1),
    years = c(seq(2110, 2150, 20)),
    names = getNames(x)
  )

  xNew[, c(seq(2110, 2150, 20)), ] <- x[, 2100, ]

  x <- mbind(x, xNew) %>%
    toolCountryFill(fill = 0, verbosity = 2) %>%
    collapseDim()

  # create weights ----

  fe <- calcOutput("FE", source = "IEA", aggregate = FALSE)[, 2016, "FE (EJ/yr)"]

  return(
    list(
      x = x,
      weight = fe,
      unit = "fraction",
      description = "share of plastic waste that gets incinerated"
    )
  )
}
