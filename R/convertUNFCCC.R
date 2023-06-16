#' Convert UNFCCC data
#' 
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from 
#'          [`readUNFCCC()`].
#'
#' @return A [`magpie`][magclass::magclass] object.
#' 
#' @author Falk Benke
#' 
#' @importFrom dplyr %>% mutate select
#' @importFrom madrat getISOlist
#' @importFrom magclass as.data.frame as.magpie
#' @importFrom quitte character.data.frame
#' @importFrom rlang sym syms
#' @importFrom tibble as_tibble
#' @importFrom tidyr complete nesting
#' 
#' @export
convertUNFCCC <- function(x)
{
  x <- as.data.frame(x) %>% 
    as_tibble() %>% 
    select('region' = 'Region', 'variable' = 'Data1', 'unit' = 'Data2', 
           'year' = 'Year', 'value' = 'Value') %>% 
    character.data.frame() %>%
    filter(!!sym("region") %in% getISOlist()) %>%
    mutate(year = as.integer(!!sym('year'))) %>%
    complete(nesting(!!!syms(c('variable', 'unit', 'year'))),
             region = setNames(getISOlist(), NULL)) %>% 
    as.magpie(tidy = TRUE)
  
  # fill countries of selected regions with 0 to allow for region aggregation
  regions.fill <- c("EUR", "REF", "NEU", "CAZ")
  mapping <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder") %>% filter(
    !!sym("RegionCode") %in% regions.fill
  )
  tmp <- x[unique(mapping$CountryCode),,]
  tmp[is.na(tmp)] <- 0
  x[unique(mapping$CountryCode),,] <- tmp

  return(x)
}
