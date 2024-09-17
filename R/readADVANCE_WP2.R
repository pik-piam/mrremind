#' Read ADVANCE WP2 Data
#'
#' @md
#' @param subtype One of
#'   - `clinker-to-cement-ratio` for the clinker-to-cement ratios from figure 21
#'     of Edelenbosch, O. _Enhancing the representation of energy demand
#'     developments in IAM models - A Modeling Guide for the Cement Industry_
#'     (2015) [zotero://select/items/JP8X2QFK](zotero://select/items/JP8X2QFK)
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Michaja Pehl
#'
#' @seealso [`readSource()`], [`convertADVANCE_WP2()`]
#'
#' @importFrom readr read_csv
#'
#' @export
readADVANCE_WP2 <- function(subtype) {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'clinker-to-cement-ratio' = function() {
      read_csv(file = './clinker-to-cement-ratio.csv',
               col_types = 'cd',
               comment = '#') %>%
        as.magpie()
    }
  )

  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               paste(names(switchboard), collapse = ', ')))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}
