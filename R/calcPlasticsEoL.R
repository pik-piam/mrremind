#' calculates projections for the end of life fate of plastic waste
#' in particular, calculates the share that is incinerated
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
#' @importFrom quitte calc_addVariable
#'
#' @export
#'
calcPlasticsEoL <- function() {

  # read source data ----

  # read in projections for plastics end-of-life flows EJ/yr
  x <- readSource("Stegmann2022")
  y <- as.quitte(x)

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

  # calculate total plastic waste
  plasticsEoL <- y %>%
    #we could have switches to include a circular scenario but it would probably
    #require a different scenario to calibrate
    filter(scenario %in% c("SSP2")) %>%
    filter(variable %in% selection) %>%
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
      units = "fraction" ) %>%
    filter(variable %in% c("Plastics|End of Life|Incineration share"))

  # as magpie

  # create weights ----

  return(
    list(
      x = plasticsEoL,
      weight = weights,
      unit = "",
      description = ""
    )
  )
}
