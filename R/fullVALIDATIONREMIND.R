#' fullVALIDATIONREMIND
#'
#' Function that generates the historical regional dataset against which the
#' REMIND model results can be compared.
#'
#' @param rev data revision which should be used as input (positive numeric).
#' @author David Klein
#' @seealso
#' \code{\link{fullREMIND}},\code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}}
#' @examples
#' \dontrun{
#' fullVALIDATIONREMIND()
#' }
#'
fullVALIDATIONREMIND <- function(rev = 0) {

  #-------------- historical data ---------------------------------------------------------------------
  
  valfile <- "historical.mif"
  
  calcOutput("Historical", round = 5,  file = valfile, aggregate = "region+global+missingH12",
             append = FALSE, na_warning = FALSE, try = TRUE)
  
  calcOutput(type = "ValidIEA_ETP", aggregate = "GLO", file = valfile,
             append = TRUE, na_warning = FALSE, try = TRUE, varSet = "all")
  
  calcOutput(type = "ValidIEA_ETP", aggregate = "region", file = valfile,
             append = TRUE, na_warning = FALSE, try = TRUE, varSet = "only_regi_meaningful")

  calcOutput(type = "ValidIEA_WEO_2021", aggregate = "GLO", file = valfile,
             append = TRUE, na_warning = FALSE, try = TRUE, subtype = "GLO")
  
  calcOutput(type = "ValidIEA_WEO_2021", aggregate = "region", file = valfile,
             append = TRUE, na_warning = FALSE, try = TRUE, subtype = "regional")

}
