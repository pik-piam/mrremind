#' Read EDGAR7 emissions data for F-gases per species, in kt of each gas
#' @md
#'
#' @return A magpie object with F-gases emissions per gas species and per country
#'
#' @author Gabriel Abrahao
#'
#' @importFrom readxl read_xlsx
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr starts_with
#' @export
readEDGAR7Fgases <- function() {
  intable <- read_excel(
    "EDGAR_F-gases_1990-2021.xlsx",
    sheet = "TOTALS BY COUNTRY", skip = 10
  )

  intable <- intable[, -c(1, 2, 4)]
  colnames(intable)[1:2] <- c("CountryCode", "Gas")
  longtable <- tidyr::pivot_longer(intable,
    cols = starts_with("Y_"),
    names_prefix = "Y_",
    names_to = "period",
    values_to = "value"
  )

  x <- as.magpie(longtable)
  return(x)
}
