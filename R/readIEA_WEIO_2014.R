#' IEA World Energy Investment Outlook (2014)
#'
#' Read projected 2014-20 investments into industry energy efficiency from the
#' [IEA World Energy Investment Outlook (2014)](http://www.iea.org/publications/freepublications/publication/weo-2014-special-report---investment.html)
#'
#' @return A [madrat_mule()] with a list containing the [tibble] `data` with
#'   2014–20 average annual investments into `Energy intensive` and
#'   `Non-energy intensive` industry, in $bn 2012, and the [tibble]
#'   `country_groups` with `IEA region`s and corresponding `iso3c` country
#'   codes.
#'
#' @importFrom dplyr anti_join group_by left_join mutate pull select summarise
#' @importFrom purrr %>% map
#' @importFrom quitte madrat_mule
#' @importFrom readr read_csv
#' @importFrom readxl excel_sheets read_excel
#' @importFrom tidyr nest unnest

#' @export
readIEA_WEIO_2014 <- function() {
  # define file paths ----
  # (for easier debugging)
  # file_country_groups <- '~/PIK/swap/inputdata/sources/IEA_WEIO_2014/IEA_WEO_country_groups.csv'
  file_country_groups <- 'IEA_WEO_country_groups.csv'

  # file_data <- '~/PIK/swap/inputdata/sources/IEA_WEIO_2014/WEIO2014AnnexA.xls'
  file_data <- 'WEIO2014AnnexA.xls'

  # read country groups ----
  country_groups <- read_csv(file = file_country_groups,
                             show_col_types = FALSE) %>%
    nest(data = .data$Countries) %>%
    mutate(result = map(.data$data, function(x) {
      tibble(iso3c = unlist(strsplit(x$Countries, ', ', fixed = TRUE)))
    })) %>%
    select(-'data') %>%
    unnest(.data$result)

  # read data ----
  d <- tibble()
  for (sheet in setdiff(excel_sheets(file_data), 'Contents')) {
    d <- bind_rows(
      d,

      tibble(
        `IEA region` = read_excel(path = file_data, sheet = sheet, range = 'C2',
                                  col_names = 'IEA region',
                                  col_types = 'text') %>%
          pull('IEA region'),

        read_excel(path = file_data, sheet = sheet, range = 'C39:F40',
                   col_names = c('name', 'value'),
                   col_types = c('text', 'skip', 'skip', 'numeric'),
                   progress = FALSE)
      )
    )
  }

  # calculate additional regions ----
  # For regions which have sub-regions, calculate a separate region without the
  # sub-regions.  For example, split 'OECD Americas' into "United States' and
  # 'OECD Americas w/o United States'.
  region_modifications <- tribble(
    ~superset,             ~subsets,
    'OECD Americas',       'United States',
    'OECD Europe',         'European Union',
    'OECD Asia Oceania',   'Japan',
    'E. Europe/Eurasia',   'Russia',
    'Non-OECD Asia',       'China, India, Southeast Asia',
    'Latin America',       'Brazil'
  ) %>%
    mutate(`IEA region` = paste(.data$superset, 'w/o', .data$subsets)) %>%
    nest(data = .data$subsets) %>%
    mutate(result = map(.data$data, function(x) {
      tibble(subsets = unlist(strsplit(x$subsets, ', ', fixed = TRUE)))
    })) %>%
    select(-'data') %>%
    unnest(.data$result) %>%
    select('IEA region', 'superset', 'subsets')

  ## calculate region mappings ----
  country_groups <- bind_rows(
    country_groups %>%
      anti_join(region_modifications, c('IEA region' = 'superset')),

    region_modifications %>%
      left_join(country_groups, c('superset' = 'IEA region')) %>%
      anti_join(country_groups, c('subsets' = 'IEA region', 'iso3c')) %>%
      distinct(!!!syms(c('IEA region', 'iso3c')))
  )

  ## calculate region data ----
  d <- bind_rows(
    d %>%
      anti_join(region_modifications, c('IEA region' = 'superset')),

    bind_rows(
      region_modifications %>%
        left_join(d, c('superset' = 'IEA region')) %>%
        select('IEA region', 'name', 'value'),

      region_modifications %>%
        left_join(d, c('subsets' = 'IEA region')) %>%
        group_by(!!!syms(c('IEA region', 'name'))) %>%
        summarise(value = sum(.data$value) * -1, .groups = 'drop')
    ) %>%
      group_by(!!!syms(c('IEA region', 'name'))) %>%
      summarise(value = sum(.data$value), .groups = 'drop')
  )

  # return data and country groups ----
  list(data = d,
       country_groups = country_groups) %>%
    madrat_mule() %>%
    return()
}
