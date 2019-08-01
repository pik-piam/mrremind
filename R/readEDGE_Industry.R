#' Read EDGE Industry Data
#' 
#' Read EDGE Industry data
#' 
#' @return a data frame
#' @author Michaja Pehl
#' @seealso \code{\link{readSource}}
#' @param subtype for now only \code{projections_VA_iso3c}
#' 
#' @importFrom dplyr %>%
#' @importFrom readr read_rds   
#' @importFrom rlang is_empty

readEDGE_Industry <- function(subtype = NULL) {
  
  year <- NULL
  
  # list all available subtypes with functions doing all the work
  switchboard <- list(
    
    # this is just a dummy for getting SDP trajectories although the new 
    # industry subsector isn't done yet
    'projections_VA_iso3c' = function() {
      read_rds('projections_VA_iso3c.rds') %>% 
        # conversion to please the all-mighty god of magpie
        mutate(year = factor(year)) %>% 
        as.magpie() %>% 
        return()
    }
  
  )
  
  # check if the subtype called is available
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste('Invalid subtype -- supported subtypes are:', 
               names(switchboard)))
  } else {
    # load data and do whatever
    return(switchboard[[subtype]]())
  }
}
