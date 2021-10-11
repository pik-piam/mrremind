#' read UNIDO data
#' 
#' Read data from United Nations Industrial Organisation.
#'
#' @md
#' @param subtype one of
#'                - `INDSTAT2`: read INDSTAT2 data
#'
#' @return A [`magpie`][magclass::magclass] object.
#' 
#' @author Michaja Pehl
#' 
#' @seealso [`readSource()`],
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr select
#' @importFrom magclass as.magpie
#' @importFrom quitte madrat_mule
#' @importFrom rlang .data
#' @importFrom tidyr unite

#' @export
readUNIDO <- function(subtype)
{
  # ---- define read functions for all subtypes ----
  switchboard <- list(
    `INDSTAT2` = function()
    {
      read_csv(file = './INDSTAT2/INDSTAT2_2017_ISIC_Rev_3.csv',
               col_names = c('ctable', 'country', 'year', 'isic', 'isiccomb', 
                             'value', 'utable', 'source', 'lastupdated', 
                             'unit'),
               col_types = 'iiiccdiddc',
               na = '...') %>% 
        select('ctable', 'country', 'year', 'isic', 'isiccomb', 'utable', 
               'source', 'lastupdated', 'unit', 'value') %>% 
        filter(!is.na(.data$value)) %>% 
        madrat_mule() %>% 
        return()
    }
  )
  
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard))))
  {
    stop(paste(
      'Invalid subtype -- supported subtypes are:',
      names(switchboard)
    ))
  }
  else
  {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}
