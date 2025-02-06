#' Calculate 2005 macroeconomic capital investments
#'
#' Compute macroeconomic capital investments based on investments shares from the PWT and GDP scenarios from
#' [mrdrivers]. The final investments are the product of the two.
#'
#' @export
#' @seealso \itemize{
#'   \item See the vignette \code{vignette("scenarios", "mrdrivers")} for information on the GDP scenarios.
#'   \item [readPWT()] for information on the PWT version used.
#' }
#' @inherit madrat::calcOutput return
calcMacroInvestments <- function() {

  # Read-in 2005 investment share and SSP2 GDP
  shInv <- readSource("PWT")[, 2005, "csh_i"]
  gdp <- calcOutput("GDP", scenario = "SSP2", aggregate = FALSE)[, 2005, ]

  shInv_new <- toolFillWithRegionAvg(shInv, valueToReplace = 0, weight = gdp)

  # Calculate macro investments
  data <- shInv_new * gdp

  # Convert from million to trillion
  data <- data * 1e-6

  getYears(data) <- NULL
  getNames(data) <- NULL

  list(x = data,
       weight = NULL,
       unit = glue::glue("trillion US${mrdrivers::toolGetUnitDollar(returnOnlyBase = TRUE)}"),
       description = glue::glue("Investments in the macro-economic capital stock computed using the investment/GDP \\
                                ratio from the PWT, and GDP scenarios from mrdrivers."))
}
