#' Read EDGAR7 emissions data for F-gases per species, in kt of each gas
#' @md
#'
#' @return A magpie object with F-gases emissions per gas species and per country
#' @author Gabriel Abrahao
#' @export
readEDGAR7Fgases <- function() {
  intable <- readxl::read_excel("EDGAR_F-gases_1990-2021.xlsx", sheet = "TOTALS BY COUNTRY", skip = 10)

  intable <- intable[, -c(1, 2, 4)]
  colnames(intable)[1:2] <- c("CountryCode", "Gas")
  longtable <- tidyr::pivot_longer(intable,
                                   cols = tidyselect::starts_with("Y_"),
                                   names_prefix = "Y_",
                                   names_to = "period",
                                   values_to = "value")

  as.magpie(longtable)
}
