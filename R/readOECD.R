#' Read OECD
#'
#' Read-in risk premium
#'
#' @order 1
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()]
#' @examples \dontrun{
#' readSource("OECD")
#' }
readOECD <- function() {
  readxl::read_excel("cre-crc-current-english.xlsx", na = "-", progress = FALSE) %>%
    dplyr::select("iso3c" = "Country Code\r\nISO Alpha 3", "Current Prevailing Classification") %>%
    dplyr::mutate("Current Prevailing Classification" = as.numeric(.data$`Current Prevailing Classification`)) %>%
    dplyr::filter(!is.na(.data$iso3c)) %>%
    as.magpie()
}

#' @rdname readOECD
#' @order 2
#' @param x MAgPIE object returned from readOECD
convertOECD <- function(x) {
  x[is.na(x)] <- 0
  x <- toolCountryFill(x, fill = 0)
  x
}
