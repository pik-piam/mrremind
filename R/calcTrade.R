#' Computes Trade variables based on latest IEA data available
#'
#' @return a magclass object
calcTrade <- function() {

  x <- calcOutput("IO", subtype = "trade", ieaVersion = "latest",
                  corrected = TRUE, aggregate = FALSE)

  # Xport - Mport
  mapping <- list(
    "peoil.Mport" = "Trade|Imports|Oil (EJ/yr)",
    "peoil.Xport" = "Trade|Exports|Oil (EJ/yr)",
    "pecoal.Mport" = "Trade|Imports|Coal (EJ/yr)",
    "pecoal.Xport" = "Trade|Exports|Coal (EJ/yr)",
    "pegas.Mport" = "Trade|Imports|Gas (EJ/yr)",
    "pegas.Xport" = "Trade|Exports|Gas (EJ/yr)"
  )

  # rename 3. dimension
  getNames(x) <- mapping[getNames(x)]

  x <- mbind(x, setNames(x[, , "Trade|Exports|Oil (EJ/yr)"] - x[, , "Trade|Imports|Oil (EJ/yr)"], "Trade|Oil (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "Trade|Exports|Coal (EJ/yr)"] - x[, , "Trade|Imports|Coal (EJ/yr)"], "Trade|Coal (EJ/yr)"))
  x <- mbind(x, setNames(x[, , "Trade|Exports|Gas (EJ/yr)"] - x[, , "Trade|Imports|Gas (EJ/yr)"], "Trade|Gas (EJ/yr)"))

  return(list(
    x = x, weight = NULL, unit = "EJ",
    description = "IEA Final Energy Data based on 2024 version of IEA Energy Balances"
  ))
}
