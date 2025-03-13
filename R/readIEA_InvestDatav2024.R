#' IEA World Energy Investment Outlook (2014)
#'
#' Read 2015-2024 investments statistical data in energy sector (electricity, oil, gas)
#' [IEA World Energy Investment Outlook (2024)](https://www.iea.org/data-and-statistics/data-product/world-energy-investment-2024-datafile)
#'
#' @return A [madrat_mule()] with a list containing the [tibble] `data` with
#'   2015–24 average annual investments into `Energy Supply` industry, in $bn(MER) 2023, and the [tibble]
#'   `country_groups` with `IEA region`s and corresponding `iso3c` country
#'   codes.
#'
#' @importFrom dplyr anti_join group_by left_join mutate pull select summarise n
#' @importFrom quitte madrat_mule
#' @importFrom readr read_csv
#' @importFrom readxl excel_sheets read_excel
#' @importFrom tidyr nest unnest
#' @importFrom assertr verify
#' @export
readIEA_InvestDatav2024 <- function() {
  # define file paths ----
  # (for easier debugging)
  # path <- '~/PIK/swap/inputdata/sources/IEA_InvestDatav2024/'
  path <- 'C:/D-data/tmp/IEA_InvestDatav2024'

  file_country_groups <- file.path(path, 'IEA_WEO_country_groups_adjusted.csv')
  file_data <- file.path(path, 'WorldEnergyInvestment2024_DataFile.xlsx')

  # read country groups ----
  country_groups <- read.csv2(file = file_country_groups, sep =";")

  # read data ----
  d <- tibble()
  for (sheet in setdiff(excel_sheets(file_data), c('Contents', 'Cover'))) {
    d <- bind_rows(
      d,
#read block "Total"
      tibble(
        `IEA_region` = read_excel(path = file_data, sheet = sheet, range = 'B4',
                                  col_names = 'IEA_region',
                                  col_types = 'text') %>%
          pull('IEA_region'),
        read_excel(path = file_data, sheet = sheet, range = 'B4:l6',
                   col_names = c('name', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023', '2024'),
                   col_types = c('text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'),
                   progress = FALSE)
      ) %>% mutate(prefix = "Total"),
#read block "Fuels"
      tibble(
        `IEA_region` = read_excel(path = file_data, sheet = sheet, range = 'B4',
                                  col_names = 'IEA_region',
                                  col_types = 'text') %>%
          pull('IEA_region'),
        read_excel(path = file_data, sheet = sheet, range = 'B8:l13',
                   col_names = c('name', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023', '2024'),
                   col_types = c('text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'),
                   progress = FALSE)
      ) %>% mutate(prefix = "Fuels"),
#read block "Electricity"
      tibble(
        `IEA_region` = read_excel(path = file_data, sheet = sheet, range = 'B4',
                                  col_names = 'IEA_region',
                                  col_types = 'text') %>%
          pull('IEA_region'),
        read_excel(path = file_data, sheet = sheet, range = 'B15:l23',
                   col_names = c('name', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023', '2024'),
                   col_types = c('text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'),
                   progress = FALSE)
      ) %>% mutate(prefix = "Electricity"),
#read block "End-Use"
      tibble(
        `IEA_region` = read_excel(path = file_data, sheet = sheet, range = 'B4',
                                  col_names = 'IEA_region',
                                  col_types = 'text') %>%
          pull('IEA_region'),
        read_excel(path = file_data, sheet = sheet, range = 'B25:l28',
                   col_names = c('name', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023', '2024'),
                   col_types = c('text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'),
                   progress = FALSE)
      ) %>% mutate(prefix = "End-Use"),
#read block "Other"
      tibble(
        `IEA_region` = read_excel(path = file_data, sheet = sheet, range = 'B4',
                                  col_names = 'IEA_region',
                                  col_types = 'text') %>%
          pull('IEA_region'),
        read_excel(path = file_data, sheet = sheet, range = 'B30:l31',
                   col_names = c('name', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023', '2024'),
                   col_types = c('text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'),
                   progress = FALSE)
      ) %>% mutate(prefix = "Other")


    )
  }

  d <- d %>% reshape2::melt(id = c("IEA_region", "name", "prefix")) %>%
    mutate(period = variable, variable = name, unit ="billion USD(2023)",
           model = "IEA InvestData", scenario = "historical") %>%
    dplyr::select(model, scenario, IEA_region, period, prefix, variable, unit, value) %>%
    tidyr::unite("variable", prefix:variable, remove = TRUE, sep = "|") %>%
    na.omit()
  for(regs in levels(factor(d$IEA_region))) {
    d <- d %>% filter(variable != regs)
  }
  x <- as.magpie(d, spatial = 3)
  return(x)
    # return data and country groups ----
}
# stop()
#
#
#   # calculate additional regions ----
#   # For regions which have sub-regions, calculate a separate region without the
#   # sub-regions.  For example, split 'OECD Americas' into "United States' and
#   # 'OECD Americas w/o United States'.
#   region_modifications <- tribble(
#     ~superset,             ~subsets,
#     'OECD Americas',       'United States',
#     'OECD Europe',         'European Union',
#     'OECD Asia Oceania',   'Japan',
#     'E. Europe/Eurasia',   'Russia',
#     'Non-OECD Asia',       'China, India, Southeast Asia',
#     'Latin America',       'Brazil'
#   ) %>%
#     mutate(`IEA region` = paste(.data$superset, 'w/o', .data$subsets)) %>%
#     nest(data = .data$subsets) %>%
#     mutate(result = purrr::map(.data$data, function(x) {
#       tibble(subsets = unlist(strsplit(x$subsets, ', ', fixed = TRUE)))
#     })) %>%
#     select(-'data') %>%
#     unnest(.data$result) %>%
#     select('IEA region', 'superset', 'subsets')
#
#   ## calculate region mappings ----
#   country_groups <- bind_rows(
#     country_groups %>%
#       anti_join(region_modifications, c('IEA region' = 'superset')),
#
#     region_modifications %>%
#       distinct(!!!syms(c('IEA region', 'superset'))) %>%
#       left_join(country_groups, c('superset' = 'IEA region')) %>%
#       select('IEA region', 'iso3c') %>%
#       anti_join(
#         region_modifications %>%
#           group_by(!!sym('IEA region')) %>%
#           left_join(country_groups, c('subsets' = 'IEA region')) %>%
#           distinct(!!!syms(c('IEA region', 'iso3c'))) %>%
#           rename(remove_iso3c = 'iso3c'),
#
#         c('iso3c' = 'remove_iso3c')
#       )
#   ) %>%
#     group_by(.data$iso3c) %>%
#     mutate(count = n()) %>%
#     ungroup() %>%
#     verify(expr = 1 == .data$count,
#            description = 'duplicate countries in country groups') %>%
#     select(-'count')
#
#   ## calculate region data ----
#   d <- bind_rows(
#     d %>%
#       anti_join(region_modifications, c('IEA region' = 'superset')),
#
#     bind_rows(
#       region_modifications %>%
#         left_join(d, c('superset' = 'IEA region')) %>%
#         select('IEA region', 'name', 'value'),
#
#       region_modifications %>%
#         left_join(d, c('subsets' = 'IEA region')) %>%
#         group_by(!!!syms(c('IEA region', 'name'))) %>%
#         summarise(value = sum(.data$value) * -1, .groups = 'drop')
#     ) %>%
#       group_by(!!!syms(c('IEA region', 'name'))) %>%
#       summarise(value = sum(.data$value), .groups = 'drop')
#   )
#
