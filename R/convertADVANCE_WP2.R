#' Convert ADVANCE WP2 Data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned by 
#'   [`readADVANCE_WP2()`].
#' @param subtype One of
#'   - `clinker-to-cement-ratio` for the clinker-to-cement ratios from figure 21
#'     of Edelenbosch, O. _Enhancing the representation of energy demand
#'     developments in IAM models - A Modeling Guide for the Cement Industry_
#'     (2015) [zotero://select/items/JP8X2QFK](zotero://select/items/JP8X2QFK),
#'     which is extended from H12 regions to country level.
#'
#' @return A [`magpie`][magclass::magclass] object.
#' 
#' @author Michaja Pehl
#' 
#' @seealso [`readSource()`], [`readADVANCE_WP2()`]
#' 
#' @importFrom assertr assert
#' @importFrom dplyr %>% full_join select
#' @importFrom madrat toolGetMapping
#' @importFrom magclass as.data.frame as.magpie
#' @importFrom rlang sym
#' @importFrom tibble as_tibble
#' @importFrom tidyselect everything
#' 
#' @export
convertADVANCE_WP2 <- function(x, subtype) {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'clinker-to-cement-ratio' = function(x) {
      x %>% 
        as.data.frame() %>% 
        as_tibble() %>% 
        select(region = 'Region', ratio = 'Value') %>% 
        full_join(
          toolGetMapping(name = 'regionmappingH12.csv', type = 'regional', where = "mappingfolder") %>% 
            as_tibble() %>% 
            select(iso3c = 'CountryCode', region= 'RegionCode'),
          
          'region'
        ) %>% 
        assert(not_na, everything()) %>% 
        select(-'region') %>% 
        as.magpie()
    }
  )
  
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste('Invalid subtype -- supported subtypes are:', 
               paste(names(switchboard), collapse = ', ')))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]](x))
  }
}
