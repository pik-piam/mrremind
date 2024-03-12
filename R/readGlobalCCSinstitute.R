#' Read Global CCS Institute Project Database
#'
#' @md
#' @param subtype Project Database version to read, one of
#'     - `'08-09-2017'`: Data apparently from June 2017.
#'     - `'2023-11'`: Data from the
#'       [Global Status of CCS 2023](zotero://select/items/3_E5GNNPZ8) report.
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @importFrom dplyr %>% mutate select
#' @importFrom magclass as.magpie
#' @importFrom quitte madrat_mule
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#'
#' @export
readGlobalCCSinstitute <- function(subtype = '08-09-2017') {
   if ('08-09-2017' == subtype) { # 08-09-2017 ----
      . <- NULL
      read_excel("status-ccs-project-database-current-08-09-2017.xlsx",
                 skip = 3) %>%
         select('Country', 'State / district', 'CO2 capture capacity (Mtpa)',
                'Operation date', 'Industry') %>%
         # use approximated values, or upper values from ranges
         mutate(value = as.numeric(sub('^(Approx. |[0-9]+\\.[0-9]+-)', '',
                                       .data$`CO2 capture capacity (Mtpa)`)),
                .keep = 'unused') %>%
         as.magpie(spatial = 1, datacol = 5) %>%
         `[<-`(is.na(.), value = 0) %>%
         return()
   }
   else if ('2023-11' == subtype) { # 2023-11 ----
      read_csv(file = 'Global_Status_of_CCS_2023-11.csv', col_types = 'cccicnc',
               na = 'Under Evaluation', comment = '#', trim_ws = TRUE) %>%
         madrat_mule() %>%
         return()
   }
   else {
      stop('Unsupported version argument.')
   }
}


