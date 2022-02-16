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
  
  calcOutput(type = "IEA_ETP", aggregate = "global", file = valfile,
             append = TRUE, na_warning = FALSE, try = TRUE, isValidation = TRUE)
  
  calcOutput(type = "IEA_ETP", aggregate = "region", file = valfile,
             append = TRUE, na_warning = FALSE, try = TRUE, isValidation = TRUE)

  calcOutput(type = "IEA_WEO_2021", aggregate = "global", file = valfile,
             append = TRUE, na_warning = FALSE, try = TRUE, isValidation = TRUE)
  
  calcOutput(type = "IEA_WEO_2021", aggregate = "region", file = valfile,
             append = TRUE, na_warning = FALSE, try = TRUE, isValidation = TRUE)

}
