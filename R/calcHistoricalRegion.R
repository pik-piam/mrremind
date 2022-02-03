#' Gather global reegional data from various sources. Call this function with aggregate = "region".
#' @importFrom magclass setNames getNames getSets add_columns
#' @importFrom luscale rename_dimnames
#' @importFrom madrat getISOlist
#'

calcHistoricalRegion <- function() {
  
  IEA_ETP <- calcOutput("IEA_ETP", aggregate = F)

  # filter regional cement and steel production data from IEA ETP
  IEA_ETP <- IEA_ETP[, , c(
    "Production|Industry|Cement (Mt/yr)",
    "Production|Industry|Steel (Mt/yr"
  ), pmatch = T, invert = T]

  WEO_2021 <- calcOutput("IEA_WEO_2021", subtype = "regional", aggregate = F)

  # ====== start: blow up to union of years ===================
  # find all existing years (y) and variable names (n)

  varlist <- list(IEA_ETP, WEO_2021)

  y <- Reduce(union, lapply(varlist, getYears))
  n <- Reduce(c, lapply(varlist, getNames))
  y <- sort(y)

  # create empty object with full temporal, regional and data dimensionality
  data <- new.magpie(getISOlist(), y, n, fill = NA)

  getSets(data)[3] <- "model"
  getSets(data)[4] <- "variable"

  # transfer data of existing years
  for (i in varlist) {
    data[, getYears(i), getNames(i)] <- i
  }
  # ====== end: blow up to union of years ===================

  # add scenario dimension
  data <- add_dimension(data, dim = 3.1, add = "scenario", nm = "historical")

  # rename dimension "data" into "variable"
  getSets(data)[5] <- "variable"

  return(list(x = data, weight = NULL, unit = "Various", description = "Historical Data"))
}
