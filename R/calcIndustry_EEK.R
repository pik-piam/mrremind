#' Industry Energy Efficiency Capital
#'
#' @param kap General internal capital stock, as calculated internally by
#'   `calcCapital()`.
#'
#' @return
#'
#' @importFrom assertr assert
#' @importFrom magclass mbind
#' @importFrom madrat calcOutput readSource getISOlist
#' @importFrom quitte madrat_mule
#' @importFrom dplyr %>% arrange bind_rows filter group_by lag lead mutate
#'                   row_number select
#' @importFrom tidyr nest pivot_longer unnest
#' @importFrom assertr assert
#' @importFrom purrr map reduce
#' @importFrom rlang .data .env sym syms

#' @export
calcIndustry_EEK <- function(kap) {
  # setup ----
  i <- log(4) / 50    # assuming 50 year lifetime of EEK
  base_year <- 2015

  . <- NULL

  # read data ----
  ## subsector activity projections ----
  industry_VA <- calcOutput(
    type = 'Industry_Value_Added',
    subtype = 'economic',
    match.steel.historic.values = TRUE,
    match.steel.estimates = 'IEA_ETP',
    China_Production = readSource(type = 'ExpertGuess',
                                  subtype = 'Chinese_Steel_Production',
                                  convert = FALSE) %>%
      madrat_mule(),
    aggregate = FALSE, years = base_year, supplementary = FALSE) %>%
    `[`(,,'gdp_SSP2EU') %>%
    magclass_to_tibble() %>%
    select('iso3c', subsector = 'name', VA = 'value') %>%
    mutate(subsector = sub('_VA$', '', .data$subsector))

  ## investment volumes into energy efficiency ----
  IEA_WEIO_2014 <- readSource('IEA_WEIO_2014', convert = FALSE) %>%
    madrat_mule()

  ## industry subsector activity and FE projections ----
  FEdemand <- calcOutput(type = 'FEdemand', subtype = 'FE', aggregate = FALSE,
                         supplementary = FALSE)

  # calculate EEK ----
  ## split industry VA into IEA investment sectors ----
  # Cement, Chemicals, and Steel are 'energy intensive', Other Industry is
  # 'non-energy intensive'
  industry_VA <- industry_VA %>%
    full_join(
      IEA_WEIO_2014 %>%
        getElement('country_groups'),

      'iso3c'
    ) %>%
    assert(not_na, everything()) %>%
    mutate(name = ifelse('otherInd' == .data$subsector,
                         'Non-energy intensive',
                         'Energy intensive')) %>%
    # calculate country/sector share in IEA region VA
    group_by(!!!syms(c('IEA region', 'name'))) %>%
    mutate(VA.share = .data$VA / sum(.data$VA)) %>%
    ungroup()

  ## calculate EEK from investments ----
  EEK <- IEA_WEIO_2014 %>%
    getElement('data') %>%
    # combine investment estimates with subsector VA figures
    inner_join(
      industry_VA,

      c('IEA region', 'name')
    ) %>%
    # assuming a "steady state", where investments only replace existing EEK
    # stock
    mutate(EEK = .data$VA.share * .data$value / .env$i) %>%
    select('iso3c', 'subsector', 'EEK')

  ## split steel EEK ----
  EEK <- EEK %>%
    # split steel EEK based on primary/secondary steel FE shares (higher FE
    # shares should lead to higher EEK shares)
    left_join(
      FEdemand %>%
        `[`(,base_year,'gdp_SSP2EU.fe', pmatch = 'left') %>%
        `[`(,,'steel', pmatch = TRUE) %>%
        magclass_to_tibble() %>%
        select(iso3c = 'region', 'item', FE = 'value') %>%
        # everything not 'steel_secondary' is 'steel_primary'
        mutate(item = sub('steel$', 'steel_primary', .data$item)) %>%
        extract('item', 'foo', '^fe.*_(steel_.*)$') %>%
        # calculate primary/secondary steel FE shares
        group_by(!!!syms(c('iso3c', 'foo'))) %>%
        summarise(FE = sum(.data$FE), .groups = 'drop_last') %>%
        mutate(FE.share = replace_na(.data$FE / sum(.data$FE), 0),
               subsector = 'steel') %>%
        ungroup() %>%
        select(-'FE'),

      c('iso3c', 'subsector')
    ) %>%
    mutate(subsector = ifelse(is.na(.data$foo), .data$subsector, .data$foo),
           EEK       = ifelse(is.na(.data$foo), .data$EEK,
                              .data$EEK * .data$FE.share)) %>%
    select('iso3c', 'subsector', 'EEK')

  ## deflate 2012 dollars ----
  # Converting from 2012 to 2005 dollars using GDP deflator from
  # https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=US
  EEK <- EEK %>%
    mutate(EEK = .data$EEK * 0.8743,
           # $bn/yr * 1e-3 $tn/$bn = $tn/yr
           EEK = .data$EEK * 1e-3)

  ## temper EEK share in total capital ----
  # Temper industry EEK share in total capital by applying a geometric average
  # between the regional shares and the global share.
  temper <- full_join(
    EEK %>%
      group_by(.data$iso3c) %>%
      summarise(EEK = sum(.data$EEK), .groups = 'drop') %>%
      sum_total_(group = 'iso3c', value = 'EEK', name = 'World'),

      kap %>%
        sum_total_(group = 'iso3c', value = 'kap', name = 'World'),

      'iso3c'
    ) %>%
    assert(not_na, everything()) %>%
    mutate(
      kap_ind_share = .data$EEK / .data$kap,
      temper = .data$kap_ind_share['World' == .data$iso3c],
      kap_ind_share_tempered = ifelse(
        'World' == .data$iso3c, FALSE,
        (.data$kap_ind_share * .data$temper ^ 2) ^ (1 / 3)),
      kap_ind_tempered = .data$kap_ind_share_tempered * .data$kap,
      kap_ind_tempered = ifelse('World' == .data$iso3c,
                                sum(.data$kap_ind_tempered),
                                .data$kap_ind_tempered),
      temper = .data$kap_ind_tempered / .data$EEK) %>%
    filter('World' != .data$iso3c)

  EEK <- full_join(
    EEK,

    temper %>%
      select('iso3c', 'temper'),

    'iso3c'
  ) %>%
    mutate(EEK = .data$EEK * .data$temper) %>%
    select(-'temper')

  ## calculate EEK growth rates ----
  # EEK is assumed to stay constant in relation to subsector output, so it
  # grows/shrinks as the output grows/shrinks.  Shrinking of EEK is limited by
  # the depreciation rate i.

  .limit_EEK_growth <- function(df) {
  # helper function to apply the capital depreciation rate limit
    df %>%
      # turn each row (period) into a list
      group_by(row_number()) %>%
      nest() %>%
      pull(.data$data) %>%
      # Sequentially operate on two one-row data frames, x being the result of
      # the previous operation, or starting at the base year row, y the
      # 'current' row.  So y holds the baseline change rate computed earlier
      # (below), while x holds the change rate conforming to the capital
      # depreciation limit, as it has already been processed.  reduce() returns
      # only the last row of the computation, so we get one output row for each
      # of the input rows.
      reduce(
        .f = function(x, y) {
          # combine data frames
          bind_rows(x, y) %>%
            # re-calculate change rate
            mutate(
              # case_when() seems broken (dplyr 1.0.9), using nested ifelse()
              # instead
              change = ifelse(
                # first row has already been processed, base_year stays constant
                # (at 1) anyways
                1 == row_number() | base_year == .data$year,
                .data$change,
                ifelse(
                  base_year < .data$year,
                  # capital after the base year can decrease by no more than the
                  # capital depreciation rate i
                  pmax(.data$change,
                       ( lag(.data$change, order_by = .data$year)
                       * (1 - i)
                       ^ (.data$year - lag(.data$year, order_by = .data$year))
                       )),
                  # capital before the base year can only have been higher by
                  # the inverse of the depreciation rate i
                  pmin(.data$change,
                       ( lead(.data$change, order_by = .data$year)
                       * (1 - i)
                       ^ (.data$year - lead(.data$year, order_by = .data$year))
                       ))
                )
              ))
        }
      )
  }

  EEK_change <- FEdemand %>%
    # select relevant subsector outputs, transform into usable format
    `[`(,,'ue_', pmatch = 'left') %>%
    magclass_to_tibble(c('iso3c', 'year', 'scenario', 'subsector', 'value')) %>%
    filter(.data$subsector %in% c('ue_cement', 'ue_chemicals',
                                  'ue_steel_primary', 'ue_steel_secondary',
                                  'ue_otherInd')) %>%
    # calculate baseline change rates relative to base_year
    group_by(!!!syms(c('iso3c', 'scenario', 'subsector'))) %>%
    mutate(
      # Replace zeros with the first non-zero data.  This keeps EEK constant
      # back in time for countries/subsectors that have no historic production.
      value = ifelse(0 != .data$value,
                     .data$value,
                     first(.data$value[0 != .data$value],
                           order_by = .data$year)),
      # change in production relative to base year
      change = .data$value / .data$value[base_year == .data$year],
      # temper change down to avoid unduly high EEK in developing regions,
      # especially SSA
      change = sqrt(.data$change)
    ) %>%
    # turn groups into list-columns to operate on individually
    nest() %>%
    # apply capital depreciation rate once backwards in time, once forward in
    # time from the base year, and combine the results
    mutate(foo = map(.data$data,
                     function(df) {
                       bind_rows(
                         df %>%
                           filter(base_year >= .data$year) %>%
                           arrange(desc(.data$year)) %>%
                           .limit_EEK_growth() %>%
                           arrange(.data$year),

                         df %>%
                           filter(base_year <= .data$year) %>%
                           arrange(.data$year) %>%
                           .limit_EEK_growth() %>%
                           filter(base_year < .data$year)
                       )
                     })
    ) %>%
    select(-'data') %>%
    unnest(.data$foo)

  EEK <- full_join(
    EEK %>%
      mutate(subsector = paste0('ue_', .data$subsector)),

    EEK_change,

    c('iso3c', 'subsector')
  ) %>%
    assert(not_na, everything()) %>%
    mutate(value = .data$EEK * .data$change,
           subsector = sub('^ue_', 'kap_', .data$subsector)) %>%
    select('iso3c', 'year', 'scenario', 'subsector', 'value')

  # return ----
  return(list(x = EEK %>%
                as.magpie(spatial = 1, temporal = 2, data = ncol(.)),
              weight = NULL,
              unit = 'trillion 2005US$',
              description = 'industry energy efficiency capital stock'))
}
