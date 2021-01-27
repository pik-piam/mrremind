#' Read REMIND-EU Data
#' 
#' Read REMIND-EU data
#' 
#' @md
#' @param subtype One of
#'   * `fixed_shares_FE_calibration_data` for the projections of 
#'     industry FE demand tuned for REMIND-EU
#' 
#' @return A [`magpie`][magclass::magclass] object.
#' 
#' @author Michaja Pehl
#' 
#' @seealso [`readSource()`]
#' 
#' @importFrom dplyr %>% filter pull
#' @importFrom readr read_csv read_delim

#' @export
readREMIND_EU <- function(subtype) {
  
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    fixed_shares_FE_calibration_data = function() {
      read_csv(file = 'pm_fe_demand_REMIND-EU.csv',
               col_names = c('t', 'regi', 'SSP', 'pf', 'value'),
               col_types = 'icccd',
               comment = '*') %>% 
        # # add useless NA data to please the all-mighty madrat god
        # complete(nesting(t, SSP, pf), 
        #          regi = read_delim(
        #            file = system.file('extdata', 'iso_country.csv', 
        #                               package = 'madrat'),
        #            delim = ';',
        #            col_names = 'iso3c',
        #            col_types = '-c',
        #            skip = 1) %>% 
        #            pull('iso3c')) %>% 
        as.magpie(spatial = 4, temporal = 1) %>% 
        return()
    },
    
    NULL
  )
  
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste('Invalid subtype -- supported subtypes are:', 
               names(switchboard)))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}
