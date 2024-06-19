#' Read Pauliuk et al. 2013 data
#'
#' Read data from Pauliuk et al. 2013
#' (https://dx.doi.org/10.1016/j.resconrec.2012.11.008).
#'
#' @md
#' @param subtype One of:
#'   - `lifetime`: Read estimated lifetime of overall steel stocks (approach b)
#'     in years.
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Michaja Pehl
#'
#' @seealso [`readSource()`]
#'
#' @importFrom dplyr mutate
#' @importFrom quitte madrat_mule
#' @importFrom readxl read_xlsx
#' @importFrom rlang .data

#' @export
readPauliuk <- function(subtype = 'lifetime')
{
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    lifetime = function()
    {
      read_xlsx(path = './Supplementary_Table_23.xlsx',
                sheet = 'Supplementray_Table_23',
                range = 'A5:J201',
                col_names = c('country', 'lifetime'),
                col_types = c('text', rep('skip', 8), 'numeric')) %>%
        mutate(lifetime = as.integer(.data$lifetime)) %>%
        madrat_mule() %>%
        return()
    })

  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               names(switchboard)))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}
