#' Read Global Steel Plant Tracker Data
#'
#' Read data from https://globalenergymonitor.org/projects/global-steel-plant-tracker/.
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @importFrom magrittr %>%
#' @importFrom quitte madrat_mule
#' @importFrom readxl read_xlsx
#'
#' @export
readGlobal_Steel_Plant_Tracker <- function() {
  directory <- '~/PIK/swap/inputdata/sources/Global_Steel_Plant_Tracker/'
  directory <- '.'

  file <- 'Global-Steel-Plant-Tracker-April-2024-Standard-Copy-V1.xlsx'

  read_xlsx(path = file.path(directory, file), sheet = 'Steel Plants') %>%
    madrat_mule()
}
