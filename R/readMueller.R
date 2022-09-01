#' Read Müller et al. 2013 data.
#'
#' Read data from Müller et al. 2013 (http://dx.doi.org/10.1021/es402618m).
#' 
#' @md
#' @param subtype One of:
#'   - `countries`: read table mapping country names use by Müller et al. 2013
#'     to ISO 3166-1 alpha-3 codes.
#'   - `stocks`: read low/medium/high estimates of per-capita steel stocks from
#'     Müller et al. 2013 SI2
#'
#' @return A [`magpie`][magclass::magclass] object.
#' 
#' @author Michaja Pehl
#' 
#' @importFrom assertr assert in_set
#' @importFrom dplyr distinct
#' 
#' @seealso [`readSource()`]


#' @export
readMueller <- function(subtype)
{
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    countries = function()
    {
      read_csv(file = './Mueller_countries.csv', 
               col_types = 'cc') %>%
        # check for duplicated ISO codes
        distinct(.data$country, .data$iso3c) %>%
        group_by(.data$iso3c) %>%
        mutate(iso3c.count = n()) %>%
        ungroup() %>%
        assert(in_set(1), .data$iso3c.count) %>%
        select(-.data$iso3c.count) %>% 
        madrat_mule()
    },
    
    stocks = function()
    {
      # read low, medium, and high estimates ----
      lapply(c('low', 'med', 'high'), function(estimate)
      {
        read_excel(path = paste0('./Mueller_2013_CarbonEmissions_',
                                 'InfrastructureDevelopment_SI2.xlsx'), 
                   sheet = paste('steel stock per cap', estimate),
                   range = 'A3:BH292') %>% 
          select(country = .data$Country, matches('^[0-9]{4}$')) %>% 
          pivot_longer(cols = matches('^[0-9]{4}$'), names_to = 'year', 
                       names_transform = list(year = as.integer)) %>% 
          filter(.data$value != 0) %>% 
          mutate(estimate = estimate)
      }) %>% 
        # combine all three sheets ----
        bind_rows() %>% 
        # combine USA and  "USA (before 1981)" ----
        mutate(country = ifelse('USA (before 1981)' == .data$country, 'USA',
                                .data$country)) %>%
        # add iso3c codes ----
        inner_join(
          readSource(type = 'Mueller', subtype = 'countries',
                     convert = FALSE) %>% 
            madrat_mule(), 
          
          'country'
        ) %>%
        select(.data$estimate, .data$iso3c, .data$year, 
               steel.stock.per.capita = .data$value) %>% 
        madrat_mule()
    }
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
