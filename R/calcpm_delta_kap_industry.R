#' Calculate depreciation rate of industry energy-efficiency capital (EEK).
#' 
#' @md
#' @return A list with a [`magpie`][magclass::magclass] object `x`, `unit`, and
#'   `description`.
#'   
#' @author Michaja Pehl
#'
#' @seealso [`calcOutput()`]
#' 
#' @importFrom magclass new.magpie
#' 

#' @export
calcpm_delta_kap_industry <- function() {
  lifetime <- 50
  
  pm_delta_kap_industry <- new.magpie(
    cells_and_regions = toolGetMapping(name = 'regionmapping_21_EU11.csv',
                                       type = 'regional', where = "mappingfolder")$CountryCode,
    years = NULL,
    names = 'pm_delta_kap',
    fill = log(4) / lifetime,
    sort = TRUE)
  return(
    list(x = pm_delta_kap_industry,
         weight = pm_delta_kap_industry,
         unit = 'fraction',
         description = paste('Depreciation rate of industry energy-efficiency',
                             'capital (EEK).')))
}
