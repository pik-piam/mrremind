library(tidyverse)
library(readxl)
library(quitte)


# - get investment data
# - divide by depreciation factor to get capital stock
# - spread capital stock on region countries using 

# - add OECDEUR and EU28 and calculate with the sum of the countries (giving
#   double weight to countries in both groups)

readIEA_WEIO_2014 <- function() {
  # define country groups ----
  # FIXME file
  country_groups <- read_csv(file = '~/PIK/swap/inputdata/sources/IEA_WEIO_2014/IEA_WEO_country_groups.csv',
                             comment = '#',
                             show_col_types = FALSE) %>% 
    select(-'country.name')
  
  # read data ----
  d <- lapply(unique(country_groups$country.group), function(sheet) {
    # FIXME path
    read_xls(path = '~/PIK/swap/inputdata/sources/IEA_WEIO_2014/WEIO2014AnnexA.xls',
             sheet = sheet,
             range = 'C39:F40',
             col_names = c('sector', 'value'),
             col_types = c('text', 'skip', 'skip', 'numeric')) %>% 
      mutate(country.group = sheet)
  }) %>% 
    bind_rows()
  
  # add additional country groups ----
  
  # 
}
