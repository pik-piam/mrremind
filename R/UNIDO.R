#' UNIDO data
#'
#' Read and convert data from United Nations Industrial Organisation.
#'
#' @md
#' @param subtype one of
#'     - `INDSTAT2`: read INDSTAT2 data
#' @param x result from `readUNIDO()` as passed to `convertUNIDO()`
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' `readUNIDO` returns raw INDSTAT2 data.  `convertUNIDO` converts to iso3c
#' country codes, selects industry subsectors value added data according to this
#' table
#'
#' | subsector     | ISIC | ctable | utable |
#' |---------------|------|--------|--------|
#' | manufacturing | D    | 20     | 17–20  |
#' | cement        | 20   | 20     | 17–20  |
#' | chemicals     | 24   | 20     | 17–20  |
#' | steel         | 27   | 20     | 17–20  |
#'
#' and filters data that is either unreasonable or would unduly bias regional
#' regressions according to this table
#'
#' | subsector     | iso3c | years            |
#' |---------------|-------|------------------|
#' | manufacturing | BIH   | 1990–91          |
#' | manufacturing | CHN   | 1963–97          |
#' | manufacturing | HKG   | 1963–2015        |
#' | manufacturing | IRQ   | 1994–98          |
#' | manufacturing | MAC   | 1963–2015        |
#' | manufacturing | MDV   | 1963–2015        |
#' | cement        | BDI   | 1980–2010        |
#' | cement        | CIV   | 1990–93          |
#' | cement        | HKG   | 1973–79          |
#' | cement        | IRQ   | 1992–97          |
#' | cement        | NAM   | 2007–10          |
#' | cement        | RUS   | 1970–90          |
#' | chemicals     | CIV   | 1989             |
#' | chemicals     | HKG   | 1973–79, 2008–15 |
#' | chemicals     | MAC   | 1978–79          |
#' | chemicals     | NER   | 1999–2002        |
#' | steel         | BGD   | 2011             |
#' | steel         | CHE   | 1995–96          |
#' | steel         | CHL   | 2008             |
#' | steel         | HKG   | 1973–79          |
#' | steel         | HRV   | 2012             |
#' | steel         | IRL   | 1980             |
#' | steel         | LKA   | 2006             |
#' | steel         | MAR   | 1989–2004        |
#' | steel         | MKD   | 1996             |
#' | steel         | PAK   | 1981–82          |
#' | steel         | TUN   | 2003–06          |
#'
#' `calcUNIDO()` calculates `otherInd` subsector values as the difference
#' between `manufacturing` and `cement`, `chemicals`, and `steel` values and is
#' intended to be called through [`calcOutput()`], which will aggregate regions.
#'
#' @author Michaja Pehl
#'
#' @seealso [`readSource()`], [`calcOutput()`]
#'
#' @importFrom assertr assert not_na verify
#' @importFrom dplyr anti_join bind_rows filter group_by inner_join left_join
#'     mutate select summarise
#' @importFrom GDPuc convertGDP
#' @importFrom magrittr %>%
#' @importFrom quitte list_to_data_frame madrat_mule
#' @importFrom readr read_csv
#' @importFrom rlang .data is_empty
#' @importFrom tibble tibble tribble
#' @importFrom tidyr unite

#' @rdname UNIDO
#' @export
readUNIDO <- function(subtype = 'INDSTAT2')
{
    "!# @monitor GDPuc::convertGDP"

    # define read functions for all subtypes ----
    switchboard <- list(
        `INDSTAT2` = function()
        {
            path <- '~/PIK/swap/inputdata/sources/UNIDO/' # used for debugging
            path <- './'

            read_csv(file = file.path(path, 'INDSTAT2',
                                      'INDSTAT2_2017_ISIC_Rev_3.csv'),
                     col_names = c('ctable', 'country', 'year', 'isic',
                                   'isiccomb', 'value', 'utable', 'source',
                                   'lastupdated', 'unit'),
                     col_types = 'iiiccdiddc',
                     na = '...') %>%
                select('ctable', 'country', 'year', 'isic', 'isiccomb',
                       'utable', 'source', 'lastupdated', 'unit', 'value') %>%
                filter(!is.na(.data$value)) %>%
                madrat_mule() %>%
                return()
        }
    )

    # check if the subtype called is available ----
    if (is_empty(intersect(subtype, names(switchboard))))
        stop(paste('Invalid subtype -- supported subtypes are:',
                   names(switchboard)))

    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
}

#' @rdname UNIDO
#' @export
convertUNIDO <- function(x, subtype = 'INDSTAT2')
{
    "!# @monitor GDPuc::convertGDP"

    # define convert functions for all subtypes ----
    switchboard <- list(
        `INDSTAT2` = function(x)
        {
            . <- NULL
            # add iso3c codes ----

            # FIXME We are substituting some historic country codes by 'default'
            # codes of current countries.  Generally, they are situated in the
            # same aggregation region, so this has no impact on the derivation
            # of regional statistics.  This does not apply to former Yugoslavia
            # however.  Since the countries in question (currently Slovenia and
            # Croatia, others might join the EU at a later time and then require
            # reclassification) are small compared to the respective regions
            # (Europe and Rest-of-World), the impact should be limited.
            x <- x %>%
                madrat_mule() %>%
                ## fix countries up ----
                filter(810 != .data$country) %>%   # SUN data synthetic anyhow
                left_join(
                    tribble(
                        ~country,   ~replacement,
                        200,        203,            # CSE for CSK
                        530,        531,            # CUW for ANT
                        890,        688,            # SRB for YUG
                        891,        688             # SRB for SCG
                    ),

                    'country'
                ) %>%
                mutate(country = ifelse(!is.na(.data$replacement),
                                        .data$replacement,
                                        .data$country)) %>%
                select(-'replacement') %>%
                ## add country codes ----
                left_join(
                    bind_rows(
                        countrycode::codelist %>%
                            select('iso3c', 'un') %>%
                            filter(!is.na(.data$iso3c), !is.na(.data$un)),

                        # country codes missing from package countrycode
                        tribble(
                            ~iso3c,   ~un,
                            'TWN',    158,   # Republic of China
                            'ETH',    230,   # Ethiopia and Eritrea
                            'DEU',    278,   # East Germany
                            'DEU',    280,   # West Germany
                            'PAN',    590,   # Panama
                            'SDN',    736    # Sudan
                        )
                    ),

                    c('country' = 'un')
                ) %>%
                assert(not_na, everything())

            # aggregate subsectors ----
            ## subsector selection ----
            subsector_selection <- bind_rows(
                tibble(subsector = 'manufacturing',
                       isic      = 'D',
                       ctable    = 20,
                       utable    = 17:20),

                tibble(subsector = 'cement',
                       isic      = '26',
                       ctable    = 20,
                       utable    = 17:20),

                tibble(subsector = 'chemicals',
                       isic      = '24',
                       ctable    = 20,
                       utable    = 17:20),

                tibble(subsector = 'steel',
                       isic      = '27',
                       ctable    = 20,
                       utable    = 17:20)
            )

            ## subsector exclusion ----
            subsector_exclusion <- bind_rows(
                list_to_data_frame(
                    list(
                        # unreasonable data
                        IRQ = 1994:1998,
                        MDV = unique(x$year),
                        BIH = 1990:1991,
                        # unrepresentative data
                        HKG = unique(x$year),
                        MAC = unique(x$year),
                        CHN = min(x$year):1997),
                    'iso3c', 'year') %>%
                    mutate(subsector = 'manufacturing'),

                # Data with an obvious mismatch between steel production and
                # steel value added figures is excluded.
                # Data for Hong Kong (1973-1979) is excluded since no data for
                # PR China is available for this period and the data would bias
                # any regression for the CHA region.
                list_to_data_frame(
                    list(BGD = 2011,
                         CHE = 1995:1996,
                         CHL = 2008,
                         HKG = 1973:1979,
                         HRV = 2012,
                         IRL = 1980,
                         LKA = 2006,
                         MAR = 1989:2004,
                         MKD = 1996,
                         PAK = 1981:1982,
                         TUN = 2003:2006),
                    'iso3c', 'year') %>%
                    mutate(subsector = 'steel'),

                list_to_data_frame(
                    list(BDI = 1980:2010,   # zero cement production
                         CIV = 1990:1993,   # cement VA 100 times higher than
                                            # before and after
                         NAM = 2007:2010,   # zero cement production
                         HKG = 1973:1979,   # no data for CHN prior to 1980
                         IRQ = 1992:1997,   # cement VA 100 times higher than
                                            # before and after
                         RUS = 1970:1990    # exclude data from Soviet period
                                            # which biases projections up
                    ),
                    'iso3c', 'year') %>%
                    mutate(subsector = 'cement'),

                list_to_data_frame(
                    list(CIV = 1989,
                         NER = 1999:2002,
                         HKG = c(1973:1979, 2008:2015),
                         MAC = c(1978:1979)),
                    'iso3c', 'year') %>%
                    mutate(subsector = 'chemicals')
            )

            ### aggregation ----
            x <- x %>%
                inner_join(
                    subsector_selection,

                    c('isic', 'ctable', 'utable')
                ) %>%
                anti_join(
                    subsector_exclusion,

                    c('iso3c', 'year', 'subsector')
                ) %>%
                # GDP conversion is only valid for monetary units
                verify('$' == .data$unit) %>%
                convertGDP(unit_in  = 'constant 2005 US$MER',
                           unit_out = mrdrivers::toolGetUnitDollar(),
                           replace_NAs = 'with_USA') %>%
                group_by(.data$iso3c, .data$subsector, .data$year) %>%
                filter(max(.data$lastupdated) == .data$lastupdated) %>%
                # for split countries, which lead to duplicates (e.g. CUW), use
                # the maximum
                summarise(value = max(.data$value), .groups = 'drop')

            # return ----
            x %>%
              as.magpie(spatial = 1, temporal = 3, data = ncol(.)) %>%
              toolCountryFill(verbosity = 2) %>%
              return()
        }
    )

    # check if the subtype called is available ----
    if (is_empty(intersect(subtype, names(switchboard))))
        stop(paste('Invalid subtype -- supported subtypes are:',
                   names(switchboard)))

    # ---- load data and do whatever ----
    return(switchboard[[subtype]](x))
}

#' @rdname UNIDO
#' @export
calcUNIDO <- function(subtype = 'INDSTAT2')
{
    # define calc functions for all subtypes ----
    switchboard <- list(
        `INDSTAT2` = function(x)
        {
            x <- readSource(type = 'UNIDO', subtype = subtype, convert = TRUE)

            x_manufacturing <- dimSums(x[,,'manufacturing'], dim = 3)
            x_no_manufacturing <- x[,,'manufacturing', invert = TRUE]
            x_otherInd <- setNames(
                ( x_manufacturing
                - dimSums(x_no_manufacturing, dim = 3)
                ),
                'otherInd')

            # $/year * 1e-9 $bn/$ = $bn/year
            x <- mbind(x_no_manufacturing, x_otherInd) * 1e-9

            # give proper variable names
            subsector_names <- c('cement', 'chemicals', 'steel', 'otherInd')
            variable_names  <- paste0('Value Added|Industry|',
                                      c('Cement', 'Chemicals', 'Steel',
                                        'Other Industry'))

            getNames(x) <- variable_names[match(getNames(x), subsector_names)]

            return(list(x = x,
                        weight = NULL,
                        unit = 'billion US$2017/yr',
                        description = 'industry subsector value added'))
        }
    )

    # check if the subtype called is available ----
    if (is_empty(intersect(subtype, names(switchboard))))
        stop(paste('Invalid subtype -- supported subtypes are:',
                   names(switchboard)))

    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
}
