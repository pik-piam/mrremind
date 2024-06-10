#' Read World Steel Statistical Yearbook Data
#'
#' Read combined data of World Steel Association statistical yearbooks
#' (https://www.worldsteel.org/steel-by-topic/statistics/steel-statistical-yearbook.html).
#'
#' @param subtype One of
#'   - `detailed` returning data for the worksheets
#'     - `Pig Iron Production`
#'     - `DRI Production`
#'     - `Total Production of Crude Steel`
#'     - `Production in Oxygen-Blown Converters`
#'     - `Production in Open Hearth Furnaces`
#'     - `Production in Electric Arc Furnaces`
#'     - `Apparent Steel Use (Crude Steel Equivalent)`
#'     from 1991 on or
#'   - `long` returning total production data from 1967 on
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Michaja Pehl
#'
#' @importFrom dplyr bind_rows filter group_by inner_join left_join mutate
#'             select summarise
#' @importFrom quitte add_countrycode_ madrat_mule
#' @importFrom readr read_delim read_rds
#' @importFrom rlang is_empty
#' @importFrom tibble as_tibble tribble
#' @importFrom tidyr pivot_longer
#'
#' @seealso [`readSource()`]

#' @export
readworldsteel <- function(subtype = 'detailed') {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'detailed' = function() {
      . <- NULL

      # to facilitate debugging
      file_path <- './Steel_Statistical_Yearbook_combined.ods'
      base_path <- file.path('./source/statistical_yearbook_2023_data/')

      .country_to_iso3c <- function(data)
      {
        data %>%
          add_countrycode_(origin = c(country = 'country.name'),
                           destination = 'iso3c', warn = FALSE) %>%
          # add fake iso3c codes for former countries and aggregates
          left_join(
            tribble(
              ~country,                  ~iso3c.alt,
              'Belgium-Luxembourg',      'blx',
              'Belgium-Luxemburg',       'blx',
              'Czechoslovakia',          'CSK',
              'Serbia and Montenegro',   'SCG',
              'Serbia-Montenegro',       'SCG',
              'Yugoslavia',              'YUG',
              'F.R. Yugoslavia',         'YUG',
              'Former Yugoslavia',       'YUG'),

            'country'
          ) %>%
          mutate(iso3c = ifelse(!is.na(.data$iso3c), .data$iso3c,
                                .data$iso3c.alt)) %>%
          select('iso3c', 'year', 'name', 'value') %>%
          assert(not_na, 'iso3c') %>%
          # combine country aggregates
          group_by(.data$iso3c, .data$year, .data$name) %>%
          summarise(value = sum(.data$value, na.rm = TRUE),
                    .groups = 'drop') %>%
          filter(0 != .data$value)
      }

      d_old <- lapply(
        # read these worksheets
        c('Pig Iron Production',
          'DRI Production',
          'Total Production of Crude Steel',
          'Production in Oxygen-Blown Converters',
          'Production in Open Hearth Furnaces',
          'Production in Electric Arc Furnaces',
          'Apparent Steel Use (Crude Steel Equivalent)'),
        function(sheet) {
          # from this file
          readODS::read_ods(path = file_path, sheet = sheet, na = '...') %>%
            as_tibble() %>%
            mutate(name = sheet) %>%
            pivot_longer(c(-'country', -'name'), names_to = 'year',
                         names_transform = list(year = function(x) {
                           as.integer(sub('^X', '', x)) }))
        }) %>%
        bind_rows() %>%
        .country_to_iso3c()

      layout <- tribble(
        ~name,                                           ~file,                                                           ~range,
        'Pig Iron Production',                           'P26_Production_of_pig_iron.xlsx',                               'A3:U54',
        'DRI Production',                                'P27_Production_of_direct_reduced_iron.xlsx',                    'A3:U35',
        'Total Production of Crude Steel',               'P01_Total_production_of_crude_steel.xlsx',                      'A3:U101',
        'Production in Oxygen-Blown Converters',         'P05_Production_of_crude_steel_in_oxygen_blown_converters.xlsx', 'A3:U54',
        'Production in Open Hearth Furnaces',            'P07_Production_of_crude_steel_in_other_processes.xlsx',         'A3:U82',
        'Production in Electric Arc Furnaces',           'P06_Production_of_crude_steel_in_electric_furnaces.xlsx',       'A3:U100',
        'Apparent Steel Use (Crude Steel Equivalent)',   'U01_Apparent_steel_use_(crude_steel_equivalent).xlsx',          'A3:U125')

      d_new <- lapply(seq_len(nrow(layout)),
                      function(i) {
                        readxl::read_excel(path = file.path(base_path, layout[[i,'file']]),
                                           range = layout[[i,'range']]) %>%
                          rename(country = 'Country') %>%
                          filter(!.data$country %in% c('World', 'Others')) %>%
                          pivot_longer(-'country', names_to = 'year',
                                       names_transform = as.integer) %>%
                          mutate(name = layout[[i,'name']])
                      }) %>%
        bind_rows() %>%
        .country_to_iso3c()

      d_new_incomplete_years <- d_new %>%
        group_by(.data$year) %>%
        distinct(.data$name) %>%
        summarise(count = n()) %>%
        filter(max(.data$count) != .data$count) %>%
        pull('year')

      d_new <- d_new %>%
        filter(!.data$year %in% d_new_incomplete_years)

      d <- overwrite(d_new, d_old)


      # split historic aggregates into current countries
      d %>%
        complete(nesting(!!!syms(c('iso3c', 'year'))),
                 tidyr::crossing(!!sym('name')),
                 fill = list(value = 0)) %>%
        as.magpie(spatial = 1, temporal = 2, tidy = TRUE) %>%
        toolISOhistorical(
          mapping = read_delim(
            file = system.file('extdata', 'ISOhistorical.csv',
                               package = 'madrat'),
            delim = ';',
            col_types = 'ccc') %>%
            inner_join(
              d %>%
                group_by(.data$iso3c) %>%
                summarise(year = max(.data$year), .groups = 'drop'),

              c('fromISO' = 'iso3c')
            ) %>%
            mutate(
              lastYear = ifelse(
                as.integer(sub('^y', '', .data$lastYear)) > .data$year,
                .data$lastYear, paste0('y', .data$year))) %>%
            select(-'year')
        ) %>%
        as.data.frame() %>%
        as_tibble() %>%
        select(iso3c = 'Region', name = 'Data1', year = 'Year',
               value = 'Value') %>%
        # remove fake magpie values
        filter(0 != .data$value, !is.na(.data$value)) %>%
        mutate(year = as.integer(as.character(.data$year))) %>%
        madrat_mule()
    },

    'long' = function() {
      read_rds('./data_steel_production.rds') %>%
        # kt/year * 1000 t/kt = t/year
        mutate(value = .data$value * 1000) %>%
        madrat_mule()
    },

    NULL)

  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               names(switchboard)))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}
