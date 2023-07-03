#' Read U.S. Geological Survey data
#'
#' @md
#' @param subtype One of
#'   - 'cement': read cement production data from
#'     [U.S. Geological Survey Minerals Yearbook](https://www.usgs.gov/centers/national-minerals-information-center/cement-statistics-and-information)
#'     (unit: tonnes per year)
#' @param x Data returned by [readUSGS()].
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Michaja Pehl
#'
#' @importFrom assertr assert
#' @importFrom countrycode countrycode
#' @importFrom dplyr bind_rows mutate pull rename select distinct
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom rlang is_empty
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer

#' @export
#' @rdname USGS
readUSGS <- function(subtype = 'cement') {
  # subtype switchboard ----
  switchboard <- list(
    'cement' = function() {
      # list of excel files and the sheet containing cement production ----
      files_sheets <- tribble(
        ~file,                              ~sheet,
        'cememyb_2002.xls',                 'Table23',
        'cemenmyb_2003.xls',                'T23',
        'cemenmyb_2004.xls',                'T22',
        'cemenmyb_2005.xls',                'T22',
        'myb1-2006-cemen.xls',              'T22',
        'myb1-2007-cemen.xls',              'T22',
        'myb1-2008-cemen.xls',              'T22',
        'myb1-2009-cemen.xls',              'T22',
        'myb1-2010-cemen.xls',              'T22',
        'myb1-2011-cemen.xls',              'T22',
        'myb1-2012-cemen.xls',              'T22',
        'myb1-2013-cemen.xlsx',             'T22',
        'myb1-2014-cemen.xlsx',             'T22',
        'myb1-2015-cemen.xlsx',             'T22',
        'myb1-2016-cement.xls',             'T22',
        'myb1-2017-cemen.xls',              'T22',
        'myb1-2018-cemen-adv-1.xlsx',       'T22',
        'Copy of myb1-2019-cemen-adv.xlsx', 'T22',
        'myb1-2020-cemen-ERT.xlsx',         'T22')

      # in case the cement files are moved to a subdirectory
      base_path <- './'
      # base_path <- '~/PIK/swap/inputdata/sources/USGS/'

      # country name to iso3c code mapping ----
      country.name_iso3c <- read_csv(
        file = file.path(base_path, 'country.names.csv'),
        comment = '#',
        show_col_types = FALSE)

      # read all excel sheets ----
      data <- tibble()
      for (i in 1:nrow(files_sheets)) {
        f <- path.expand(file.path(base_path, files_sheets[[i, 'file']]))
        ## read the entire sheet with global cement production data ----
        d <- suppressMessages(
          read_excel(path = f,
                     sheet = files_sheets[[i, 'sheet']],
                     col_types = 'text',
                     trim_ws = TRUE)
        )

        ## find those portions that contain data ----
        foo <- pull(d, 1)

        # data sections start with either 'Country' or 'Country or locality'
        # extend the regex to accomodate future changes
        starts <- which(foo %in% c('Country',
                                   'Country or locality'))

        # data sections end with 'See footnotes at end of table.',
        # 'Grand total', or 'Total', which in turn might have an estimator mark
        # ('e')
        # extend the regex to accomodate future changes
        ends <- which(foo %in% c('See footnotes at end of table.',
                                 'Grand total',
                                 'Total',
                                 'Totale',
                                 '\u2003Grand totale'))

        # subsections start with a heading ending with ':',
        # e.g. 'Tunesia, portland:'
        subsections <- which(grepl(':$', foo))

        combination <- sort(c(subsections, ends))
        subsection_ends <- combination[which(combination %in% subsections) + 1]
        ends <- setdiff(ends, subsection_ends)

        # filter cases in which 'Total' is directly follow by a 'See footnotes'
        # note
        ends <- setdiff(ends, ends[na.omit(match((ends + 1), ends))])

        # there should be as many 'starts' as 'ends'
        if (length(starts) != length(ends)) {
          stop('Unmatched start or end of global cement data in file ', f, '\n',
               'Matched segments:\n',
               paste(paste(sort(c(starts, ends)), foo[sort(c(starts, ends))]),
                     collapse = '\n'))
        }

        # fix subsection names to country names and replace with total data
        if (!is_empty(subsections)) {
          d[subsections,1] <- sub('^(.*),.*', '\\1', d[[subsections,1]])
          d[subsections,-1] <- d[subsection_ends,-1]
        }

        # valid data is everything between 'starts' and 'ends' (excluding),
        # without subsections, but including the (fixed) first lines of
        # subsections
        sequences_by_index <- function(start, end) {
          if (all(is_empty(start), is_empty(end))) {
            return(NULL)
          }
          lapply(1:length(start), function(i) { start[i]:end[i] }) %>%
            unlist()
        }

        ## combine valid data with data from other excel files ----
        header <- as.character(d[starts[1],])

        data <- bind_rows(
          data,

          d[setdiff(sequences_by_index(starts+1, ends-1),
                    sequences_by_index(subsections + 1, subsection_ends)),] %>%
            # rename columns
            `colnames<-`(header) %>%
            # remove comment columns
            select(c(1, which(grepl('^[0-9]{4}$', header)))) %>%
            # harmonise country column name
            rename(country.name = 1) %>%
            # convert to long format
            pivot_longer(cols = -1, names_to = 'year') %>%
            mutate(
              year = as.integer(sub('^([0-9]*).*', '\\1', .data$year)),
              # add reporting year; this supposes that the _last_ four-digit
              # number in the file name is the reporting year
              reporting.year = as.integer(sub('.*([0-9]{4}).*', '\\1', f)),
              # remove notes from country names
              country.name = sub('(, *)?[0-9]*$', '', .data$country.name),
              # clear special value codes
              value = case_when(
                '--' == .data$value ~ '0',
                '(5)' == .data$value ~ '0',
                'XX' == .data$value ~ '0',
                TRUE ~ .data$value)) %>%
            left_join(country.name_iso3c, 'country.name') %>%
            assert(not_na, 'iso3c',
                   error_fun = function(errors, data, warn) {
                     error_df <- data[errors[[1]]$error_df$index,]

                     stop('Unmachtched country names in ', f, '\n',
                          paste(format(error_df, )[-c(1, 3)], collapse = '\n'),
                          call. = FALSE)
                   }) %>%
            select(-'country.name')
        )
      }

      # estimate withheld information using GDP ----
      region_mapping <- toolGetMapping(name = 'regionmapping_21_EU11.csv',
                                       type = 'regional', where = "mappingfolder") %>%
        as_tibble() %>%
        select(iso3c = 'CountryCode', region = 'RegionCode')

      to_estimate <- data %>%
        # only estimate data when the last reporting year has no data
        group_by(!!!syms(c('year', 'iso3c'))) %>%
        filter(max(.data$reporting.year) == .data$reporting.year) %>%
        ungroup() %>%
        filter('W' == .data$value) %>%
        distinct(.data$year, .data$iso3c) %>%
        left_join(region_mapping, 'iso3c')

      estimation_base <- data %>%
        inner_join(region_mapping,'iso3c') %>%
        semi_join(to_estimate %>% select(-'iso3c'), c('year', 'region')) %>%
        arrange(.data$reporting.year) %>%
        group_by(!!!syms(c('year', 'iso3c', 'region'))) %>%
        filter(max(.data$reporting.year) == .data$reporting.year) %>%
        ungroup() %>%
        select(-'reporting.year') %>%
        left_join(
          calcOutput('GDP', aggregate = FALSE, average2020 = FALSE, years = unique(to_estimate$year)) %>%
            # historic data should be all identical, so just pick the 'default'
            # scenario
            `[`(,,'gdp_SSP2EU') %>%
            as.data.frame() %>%
            as_tibble() %>%
            select(iso3c = 'Region', year = 'Year', GDP = 'Value') %>%
            mutate(year = as.integer(as.character(.data$year))),

          c('iso3c', 'year')
        )

      estimates <- full_join(
        estimation_base %>%
          anti_join(to_estimate %>% select(-'region'), c('year', 'iso3c')) %>%
          mutate(value = as.numeric(.data$value)) %>%
          group_by(!!!syms(c('year', 'region'))) %>%
          summarise(production_per_GDP = sum(.data$value) / sum(.data$GDP),
                    .groups = 'drop'),

        estimation_base %>%
          semi_join(to_estimate, c('region', 'iso3c', 'year')),

        c('year', 'region')
      ) %>%
        mutate(value = .data$GDP * .data$production_per_GDP) %>%
        select('year', 'iso3c', 'value')

      data <- bind_rows(
        data %>%
          filter('W' != .data$value) %>%
          mutate(value = as.numeric(.data$value)),

        data %>%
          select(-'value') %>%
          right_join(estimates, c('year', 'iso3c'))
      ) %>%
        # kt/year * 1e3 t/kt = t/year
        mutate(value = .data$value * 1e3) %>%
        # merge countries with several entries (e.g. Kosovo & Serbia -> SRB)
        group_by(!!!syms(c('iso3c', 'reporting.year', 'year'))) %>%
        summarise(value = sum(.data$value), .groups = 'drop')

      return(madrat_mule(data))
    }
  )

  # check if the subtype called is available ----
  if (!subtype %in% names(switchboard)) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               paste(names(switchboard), collapse = ', ')))
  }

  # load data and to whatever ----
  return(switchboard[[subtype]]())
}

#' @export
#' @rdname USGS
convertUSGS <- function(x, subtype = 'cement') {
  . <- NULL
  # subtype switchboard ----
  switchboard <- list(
    'cement' = function() {
      x %>%
        madrat_mule() %>%
        group_by(!!!syms(c('iso3c', 'year'))) %>%
        filter(max(.data$reporting.year) == .data$reporting.year) %>%
        ungroup() %>%
        select(-'reporting.year') %>%
        complete(nesting(!!sym('year')),
                 iso3c = toolGetMapping(name = getConfig('regionmapping'),
                                        type = 'regional') %>%
                   pull('CountryCode') %>%
                   unique(),
                 fill = list(value = 0)) %>%
        as.magpie(spatial = 2, temporal = 1, datacol = ncol(.))
    }
  )

  # check if the subtype called is available ----
  if (!subtype %in% names(switchboard)) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               paste(names(switchboard), collapse = ', ')))
  }

  # load data and to whatever ----
  return(switchboard[[subtype]]())
}
