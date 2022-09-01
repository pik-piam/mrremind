#' Thermodynamic Limits for Industry Specific FE Demand
#'
#' Return `readExpertGuess('industry_specific_FE_limits')` in a format usable as
#' a REMIND input.
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Michaja Pehl
#'
#' @importFrom dplyr filter mutate select
#' @importFrom madrat readSource
#' @importFrom magclass as.magpie
#' @importFrom quitte madrat_mule

#' @export
calcindustry_specific_FE_limits <- function() {
  return(list(x = readSource(type = 'ExpertGuess',
                             subtype = 'industry_specific_FE_limits',
                             convert = FALSE) %>%
                madrat_mule() %>%
                filter('absolute' == .data$type) %>%
                select(-'type') %>%
                mutate(subsector = paste0('ue_', .data$subsector)) %>%
                as.magpie(spatial = 0, temporal = 0, data = 2),
              weight = NULL,
              unit = 'GJ/t',
              description = paste('Thermodynamic limits for industry subsector',
                                  'specific FE demand')))
}
