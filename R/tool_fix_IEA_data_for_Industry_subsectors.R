#' Apply corrections to IEA data needed for Industry subsectors
#'
#' Apply corrections to IEA data to cope with fragmentary time series and
#' replace outputs from blast furnaces and coke ovens, that are inputs into
#' industry subsectors, by their respective inputs.
#' The corrections done by this function are rather rudimentary and crude. This
#' gets smoothed away in regional aggregation. But do not use the resulting
#' country-level data without additional scrutiny.
#'
#' Use regional or global averages if IEA industry data lists energy use only as
#' "non-specified".
#' Outputs from blast furnaces (`BLFURGS`, `OGASES`) and coke ovens (`OVENCOKE`,
#' `COKEOVGS`, `COALTAR`, `NONCRUDE`), that are inputs into industry subsectors.
#' Used internally in [`calcIO()`] for subtype `output_Industry_subsectors`.
#'
#' @md
#' @param data MAgPIE object containing the IEA Energy Balances data
#'
#' @param ieamatch mapping of IEA product/flow combinations to REMIND
#'        `sety`/`fety`/`te` combinations as used in [`calcIO()`]
#'
#' @return a MAgPIE object
#'
#' @author Michaja Pehl
#'
#' @importFrom assertr not_na assert
#' @importFrom dplyr anti_join group_by inner_join left_join mutate pull rename
#'     select summarise
#' @importFrom magclass getRegions getYears getNames
#' @importFrom readr read_delim cols col_skip col_character
#' @importFrom quitte cartesian interpolate_missing_periods overwrite
#'             character.data.frame interpolate_missing_periods_
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @importFrom tidyr complete gather nesting spread

tool_fix_IEA_data_for_Industry_subsectors <- function(data, ieamatch) {

  . <- NULL

  # replace coke oven and blast furnace outputs ----
  .clean_data <- function(m, keep_zeros = FALSE) {
    m %>%
      as.data.frame() %>%
      as_tibble() %>%
      select(iso3c = 'Region', year = 'Year', product = 'Data1', flow = 'Data2',
             value = 'Value') %>%
      filter(0 != .data$value | keep_zeros) %>%
      character.data.frame() %>%
      mutate(year = as.integer(.data$year))
  }

  ## flow definitions ----
  IEA_flows <- tribble(
    ~summary.flow,   ~flow,
    # Total Primary Energy Production
    'TPES',          'INDPROD',    # primary energy production
    'TPES',          'IMPORTS',
    'TPES',          'EXPORTS',
    'TPES',          'MARBUNK',    # international marine bunkers
    'TPES',          'AVBUNK',     # international aviation bunkers
    NA_character_,   'TRANSFER',   # inter-product transfers, product transfers,
    # and recycling
    'TPES',          'STOCKCHA',   # stock changes

    # Transformation Processes
    'TOTTRANF',      'MAINELEC',    # main activity producer electricity plants
    'TOTTRANF',      'AUTOELEC',    # autoproducer electricity plants
    'TOTTRANF',      'MAINCHP',     # main activity producer CHP plants
    'TOTTRANF',      'AUTOCHP',     # autoproducer electricity plants
    'TOTTRANF',      'MAINHEAT',    # main activity producer heat plants
    'TOTTRANF',      'AUTOHEAT',    # autoproducer heat plants
    'TOTTRANF',      'THEAT',       # heat pumps
    'TOTTRANF',      'TBOILER',     # electric boilers
    'TOTTRANF',      'TELE',        # chemical heat for electricity production
    'TOTTRANF',      'TBLASTFUR',   # blast furnaces
    'TOTTRANF',      'TGASWKS',     # gas works
    'TOTTRANF',      'TCOKEOVS',    # coke ovens
    'TOTTRANF',      'TPATFUEL',    # patent fuel plants
    'TOTTRANF',      'TBKB',        # peat briquette plants
    'TOTTRANF',      'TREFINER',    # oil refineries
    'TOTTRANF',      'TPETCHEM',    # petrochemical plants
    'TOTTRANF',      'TCOALLIQ',    # coal liquefaction plants
    'TOTTRANF',      'TGTL',        # gas-to-liquid plants
    'TOTTRANF',      'TBLENDGAS',   # blended natural gas
    'TOTTRANF',      'TCHARCOAL',   # charcoal production plants
    'TOTTRANF',      'TNONSPEC',    # non-specified transformation

    # Energy Industry Own Use and Losses
    'TOTENGY',       'EMINES',      # coal mines
    'TOTENGY',       'EOILGASEX',   # oil and gas extraction
    'TOTENGY',       'EBLASTFUR',   # blast furnaces
    'TOTENGY',       'EGASWKS',     # gas works
    'TOTENGY',       'EBIOGAS',     # gasifications plants for biogases
    'TOTENGY',       'ECOKEOVS',    # coke ovens
    'TOTENGY',       'EPATFUEL',    # patent fuel plants
    'TOTENGY',       'EBKB',        # peat briquette plants
    'TOTENGY',       'EREFINER',    # oil refineries
    'TOTENGY',       'ECOALLIQ',    # coal liquefaction plants
    'TOTENGY',       'ELNG',        # liquefaction/regasification plants
    'TOTENGY',       'EGTL',        # gas-to-liquied plants
    'TOTENGY',       'EPOWERPLT',   # own use in electricity, CHP, and heat
    # plants
    'TOTENGY',       'EPUMPST',     # pumped storage plants
    'TOTENGY',       'ENUC',        # nuclear industry
    'TOTENGY',       'ECHARCOAL',   # charcoal production plants
    'TOTENGY',       'ENONSPEC',    # non-specified energy industry
    'TOTENGY',       'DISTLOSS',    # distribution and transmission losses

    # Final Consumption
    'TFC',           'IRONSTL',    # iron and steel
    'TFC',           'CHEMICAL',   # chemical and petrochemical
    'TFC',           'NONFERR',    # non-ferrous metals
    'TFC',           'NONMET',     # non-metallic minerals
    'TFC',           'TRANSEQ',    # transport equipment
    'TFC',           'MACHINE',    # machinery
    'TFC',           'MINING',     # mining and quarrying
    'TFC',           'FOODPRO',    # food production
    'TFC',           'PAPERPRO',   # paper, pulp, and print
    'TFC',           'WOODPRO',    # wood and wood products
    'TFC',           'CONSTRUC',   # construction
    'TFC',           'TEXTILES',   # textiles
    'TFC',           'INONSPEC',   # non-specified industry
    'TFC',           'WORLDAV',    # world aviation bunkers
    'TFC',           'DOMESAIR',   # domestic aviation
    'TFC',           'ROAD',       # road
    'TFC',           'RAIL',       # rail
    'TFC',           'PIPELINE',   # pipeline transport
    'TFC',           'WORLDMAR',   # world marine bunkers
    'TFC',           'DOMESNAV',   # domestic navigation
    'TFC',           'TRNONSPE',   # non-specified transport
    'TFC',           'RESIDENT',   # residential
    'TFC',           'COMMPUB',    # commercial and public services
    'TFC',           'AGRICULT',   # agriculture/forestry
    'TFC',           'FISHING',    # fishing
    'TFC',           'ONONSPEC',   # non-specified other consumption


    'TOTIND',        'IRONSTL',    # iron and steel
    'TOTIND',        'CHEMICAL',   # chemical and petrochemical
    'TOTIND',        'NONFERR',    # non-ferrous metals
    'TOTIND',        'NONMET',     # non-metallic minerals
    'TOTIND',        'TRANSEQ',    # transport equipment
    'TOTIND',        'MACHINE',    # machinery
    'TOTIND',        'MINING',     # mining and quarrying
    'TOTIND',        'FOODPRO',    # food production
    'TOTIND',        'PAPERPRO',   # paper, pulp, and print
    'TOTIND',        'WOODPRO',    # wood and wood products
    'TOTIND',        'CONSTRUC',   # construction
    'TOTIND',        'TEXTILES',   # textiles
    'TOTIND',        'INONSPEC',   # non-specified industry

    # Transport
    'TOTTRANS',      'WORLDAV',    # world aviation bunkers
    'TOTTRANS',      'DOMESAIR',   # domestic aviation
    'TOTTRANS',      'ROAD',       # road
    'TOTTRANS',      'RAIL',       # rail
    'TOTTRANS',      'PIPELINE',   # pipeline transport
    'TOTTRANS',      'WORLDMAR',   # world marine bunkers
    'TOTTRANS',      'DOMESNAV',   # domestic navigation
    'TOTTRANS',      'TRNONSPE',   # non-specified transport

    # Other Consumption
    'TOTOTHER',      'RESIDENT',   # residential
    'TOTOTHER',      'COMMPUB',    # commercial and public services
    'TOTOTHER',      'AGRICULT',   # agriculture/forestry
    'TOTOTHER',      'FISHING',    # fishing
    'TOTOTHER',      'ONONSPEC',   # non-specified other consumption

    # Non-Energy Use
    'NONENUSE',      'NEINTREN',   # non-energy use in industry/transformation/
    # energy
    NA_character_,   'NECHEM',     # non-energy use chemical/petrochemical
    'NONENUSE',      'NETRANS',    # non-energy use in transport
    'NONENUSE',      'NEOTHER',    # non-energy use in other

    # Electricity Output
    'ELOUTPUT',      'ELMAINE',   # main activity producer electricity plants
    'ELOUTPUT',      'ELAUTOE',   # autoproducer electricity plants
    'ELOUTPUT',      'ELMAINC',   # main activity producer CHP plants
    'ELOUTPUT',      'ELAUTOC',   # autoproducer CHP plants

    # Heat Output
    'HEATOUT',       'HEMAINC',   # main activity producer CHP plants
    'HEATOUT',       'HEAUTOC',   # autoproducer CHP plants
    'HEATOUT',       'HEMAINH',   # main activity producer heat plants
    'HEATOUT',       'HEAUTOH'    # autoproducer heat plants
  )

  base_flows <- unique(IEA_flows$flow)
  summary_flows <- unique(na.omit(IEA_flows$summary.flow))
  all_flows <- c(base_flows, summary_flows)

  ### blast furnace flows to be replaced ----
  # all transformation, energy system and final consumption flows, except for
  # those related to blast furnaces
  flow_BLASTFUR_to_replace <- setdiff(all_flows, c('EBLASTFUR', 'TBLASTFUR'))

  ### coke oven flows to be replaced ----
  # all transformation, energy system and final consumption flows, except for
  # those related to coke ovens
  flow_COKEOVS_to_replace <- setdiff(all_flows, c('ECOKEOVS', 'TCOKEOVS'))

  ## blast furnace data ----
  # all products in/out of blast furnace transformation and energy demand, except
  # summary flows 'TOTAL' and 'MRENEW'
  data_BLASTFUR <- data %>%
    `[`(,, c('EBLASTFUR', 'TBLASTFUR'), pmatch = 'right') %>%
    `[`(,, c('TOTAL', 'MRENEW'), pmatch = 'left', invert = TRUE) %>%
    .clean_data() %>%
    group_by(!!!syms(c('iso3c', 'year', 'product'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop')

  ### blast furnace inputs ----
  # inputs into transformation/energy system are negative
  data_BLASTFUR_inputs <- data_BLASTFUR %>%
    filter(0 > .data$value)

  ### blast furnace outputs ----
  # outputs from transformation are positive
  data_BLASTFUR_outputs <- data_BLASTFUR %>%
    filter(0 < .data$value)

  ### blast furnace output products ----
  # products blast furnaces supply to other flows
  outputs_BLASTFUR <- data_BLASTFUR_outputs %>%
    pull('product') %>%
    unique()

  ### product/flow to be replaced ----
  # all blast furnace outputs and flows to be replaced, that are actually present
  # in the data
  product_flow_BLASTFUR_to_replace <- intersect(
    cartesian(outputs_BLASTFUR, flow_BLASTFUR_to_replace),
    getNames(data))

  ### blast furnace product use ----
  data_BLASTFUR_use <- data %>%
    `[`(,, product_flow_BLASTFUR_to_replace) %>%
    .clean_data()

  ## blast furnace replacement data ----
  # outputs are replaced joule-by-joule with inputs, according to the input shares
  # right_join() filters out countries/years that do not use blast furnace
  # products
  data_BLASTFUR_replacement <- right_join(
    data_BLASTFUR_inputs %>%
      group_by(!!!syms(c('iso3c', 'year'))) %>%
      mutate(factor = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-'value'),

    data_BLASTFUR_use %>%
      select(-'product'),

    c('iso3c', 'year')
  ) %>%
    # FIXME: filter countries/years that have no inputs into blast furnaces, yet
    # outputs from them and use of blast furnace products (e.g. ISR 1973)
    filter(!is.na(.data$product)) %>%
    assert(not_na, everything(),
           description = 'Only valid blast furnace replacement data') %>%
    mutate(value = .data$value * .data$factor) %>%
    group_by(!!!syms(c('iso3c', 'year', 'product', 'flow'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop')

  ## coke oven data ----
  # all products in/out of coke oven transformation and energy demand, except
  # summary flows 'TOTAL' and 'MRENEW'
  data_COKEOVS <- data %>%
    `[`(,, c('ECOKEOVS', 'TCOKEOVS'), pmatch = 'right') %>%
    `[`(,, c('TOTAL', 'MRENEW'), pmatch = 'left', invert = TRUE) %>%
    .clean_data() %>%
    group_by(!!!syms(c('iso3c', 'year', 'product'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop')

  #### apply blast furnace replacement ----
  # Coke ovens and blast furnaces can be both inputs and outputs to one another at
  # the same time.  To untangle this, we first replace blast furnace outputs that
  # are inputs into coke ovens by coke oven outputs, which are netted with the
  # direct outputs (here), and then replace coke oven outputs that are blast
  # furnace inputs by coke oven inputs (further below).
  data_COKEOVS <- bind_rows(
    data_COKEOVS %>%
      filter(!.data$product %in% outputs_BLASTFUR),

    data_BLASTFUR_replacement %>%
      filter(.data$flow %in% c('ECOKEOVS', 'TCOKEOVS')) %>%
      select(-'flow')
  ) %>%
    group_by(!!!syms(c('iso3c', 'year', 'product'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop')

  ### coke oven inputs ----
  # inputs into transformation/energy system are negative
  data_COKEOVS_inputs <- data_COKEOVS %>%
    filter(0 > .data$value)

  ### coke oven outputs ----
  # outputs from transformation are positive
  data_COKEOVS_outputs <- data_COKEOVS %>%
    filter(0 < .data$value)

  ### coke oven output products ----
  # products coke ovens supply to other flows
  outputs_COKEOVS <- data_COKEOVS_outputs %>%
    pull('product') %>%
    unique()

  ### product/flows to be replaced ----
  # all coke oven outputs and flows to be replaced, that are actually present in
  # the data
  product_flow_COKEOVS_to_replace <- intersect(
    cartesian(outputs_COKEOVS, flow_COKEOVS_to_replace),
    getNames(data))

  ### coke oven product use ----
  data_COKEOVS_use <- data %>%
    `[`(,, product_flow_COKEOVS_to_replace) %>%
    .clean_data()

  ## coke oven replacement data ----
  # outputs are replaced joule-by-joule with inputs, according to the input shares
  # right_join() filters out countries/years that do not use coke oven products
  data_COKEOVS_replacement <- right_join(
    data_COKEOVS_inputs %>%
      group_by(!!!syms(c('iso3c', 'year'))) %>%
      mutate(factor = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-'value'),

    data_COKEOVS_use %>%
      select(-'product'),

    c('iso3c', 'year')
  ) %>%
    # FIXME: filter countries/year that have no inputs into coke ovens but use
    # coke oven outputs (which most likely are imported)
    filter(!is.na(.data$product)) %>%
    assert(not_na, everything(),
           description = 'Only valid coke oven replacement data') %>%
    mutate(value = .data$value * .data$factor) %>%
    select('iso3c', 'year', 'product', 'flow', 'value')

  ## coke oven loss data ----
  # coke oven losses (true losses from ECOKEOVS and transformation energy from
  # TCOKEOVS) are allotted to the IRONSTL sector
  # losses are the difference of inputs and outputs, weighted by input shares
  # right_join() filters out countries/years that do not use coke oven products
  data_COKEOVS_loss <- right_join(
    data_COKEOVS_inputs,

    data_COKEOVS_outputs %>%
      group_by(!!!syms(c('iso3c', 'year'))) %>%
      summarise(output = sum(.data$value), .groups = 'drop'),

    c('iso3c', 'year')
  ) %>%
    # FIXME: filter countries/year that have no inputs into coke ovens but use
    # coke oven outputs (which most likely are imported)
    filter(!is.na(.data$product)) %>%
    assert(not_na, everything(),
           description = 'Only valid coke oven loss data') %>%
    group_by(!!!syms(c('iso3c', 'year'))) %>%
    mutate(value = (sum(-.data$value) - .data$output)
           * .data$value / sum(.data$value),
           flow = 'IRONSTL') %>%
    ungroup() %>%
    select('iso3c', 'year', 'product', 'flow', 'value')

  ## recalculate blast furnace inputs w/ coke oven replacements ----

  #### apply coke oven replacement ----
  # Coke ovens and blast furnaces can be both inputs and outputs to one another at
  # the same time.  To untangle this, we first replace blast furnace outputs that
  # are inputs into coke ovens by coke oven outputs, which are netted with the
  # direct outputs (above), and then replace coke oven outputs that are blast
  # furnace inputs by coke oven inputs (here).
  data_BLASTFUR <- bind_rows(
    data_BLASTFUR %>%
      filter(!.data$product %in% outputs_COKEOVS),

    data_COKEOVS_replacement %>%
      filter(.data$flow %in% c('EBLATFUR', 'TBLASTFUR')) %>%
      select(-'flow')
  ) %>%
    group_by(!!!syms(c('iso3c', 'year', 'product'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop')

  ### blast furnace inputs ----
  # inputs into transformation/energy system are negative
  data_BLASTFUR_inputs <- data_BLASTFUR %>%
    filter(0 > .data$value)

  ### blast furnace replacement data ----
  # outputs are replaced joule-by-joule with inputs, according to the input shares
  # right_join() filters out countries/years that do not use blast furnace
  # products
  data_BLASTFUR_replacement <- right_join(
    data_BLASTFUR_inputs %>%
      group_by(!!!syms(c('iso3c', 'year'))) %>%
      mutate(factor = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-'value'),

    data_BLASTFUR_use %>%
      select(-'product'),

    c('iso3c', 'year')
  ) %>%
    # FIXME: filter countries/years that have no inputs into blast furnaces, yet
    # outputs from them and use of blast furnace products (e.g. ISR 1973)
    filter(!is.na(.data$product)) %>%
    assert(not_na, everything(),
           description = 'Only valid blast furnace replacement data') %>%
    mutate(value = .data$value * .data$factor) %>%
    group_by(!!!syms(c('iso3c', 'year', 'product', 'flow'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop')

  ## blast furnace loss data ----
  # blast furnace losses (true losses from EBLASTFUR and transformation energy
  # from TBLASTFUR) are allotted to the IRONSTL sector
  # losses are the difference of inputs and outputs, weighted by input shares
  # right_join() filters out countries/years that do not use blast furnace
  # products
  data_BLASTFUR_loss <- right_join(
    data_BLASTFUR_inputs,

    data_BLASTFUR_outputs %>%
      group_by(!!!syms(c('iso3c', 'year'))) %>%
      summarise(output = sum(.data$value), .groups = 'drop'),

    c('iso3c', 'year')
  ) %>%
    # FIXME: filter countries/years that have no inputs into blast furnaces, yet
    # outputs from them and use of blast furnace products (e.g. ISR 1973)
    filter(!is.na(.data$product)) %>%
    assert(not_na, everything(),
           description = 'Only valid blast furnace loss data') %>%
    group_by(!!!syms(c('iso3c', 'year'))) %>%
    mutate(value = (sum(-.data$value) - .data$output)
           * .data$value / sum(.data$value),
           flow = 'IRONSTL') %>%
    ungroup() %>%
    select('iso3c', 'year', 'product', 'flow', 'value')

  ## replace coke oven and blast furnace products ----
  data_replace <- bind_rows(
    # filter already replaced data
    data_COKEOVS_replacement %>%
      filter(!.data$flow %in% c('EBLATFUR', 'TBLASTFUR')),

    data_BLASTFUR_replacement %>%
      filter(!.data$flow %in% c('ECOKEOVS', 'TCOKEOVS')),

    data_COKEOVS_loss %>%
      sum_total_('product', name = 'TOTAL'),

    data_BLASTFUR_loss %>%
      sum_total_('product', name = 'TOTAL')
  ) %>%
    group_by(!!!syms(setdiff(colnames(.), 'value'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop') %>%
    arrange(!!!syms(c('iso3c', 'year', 'product', 'flow'))) %>%
    as.magpie(spatial = 1, temporal = 2, data = ncol(.))

  data_replace[is.na(data_replace)] <- 0

  regions_keep <- sort(getRegions(data))
  years_keep   <- sort(getYears(data))
  names_keep   <- sort(setdiff(getNames(data),
                               c(product_flow_COKEOVS_to_replace,
                                 product_flow_BLASTFUR_to_replace)))

  regions_replace <- sort(getRegions(data_replace))
  years_replace   <- sort(getYears(data_replace))
  names_replace   <- sort(getNames(data_replace))

  data_fixed <- new.magpie(cells_and_regions = regions_keep, years = years_keep,
                           names = unique(c(names_keep, names_replace)),
                           fill = 0)

  data_fixed[regions_keep,years_keep,names_keep] <- data %>%
    `[`(regions_keep, years_keep, names_keep)

  data_fixed[regions_replace,years_replace,names_replace] <- (
      data_fixed[regions_replace,years_replace,names_replace]
    + data_replace[regions_replace,years_replace,names_replace]
  )

  data <- data_fixed

  # all industry subsector flows
  flows_to_fix <- c('IRONSTL', 'CHEMICAL', 'NONFERR', 'NONMET', 'TRANSEQ',
                    'MACHINE','MINING', 'FOODPRO', 'PAPERPRO', 'WOODPRO',
                    'CONSTRUC', 'TEXTILES')

  # all products associated with those flows
  products_to_fix <- ieamatch %>%
    filter(.data$iea_flows %in% flows_to_fix) %>%
    getElement('iea_product') %>%
    unique()

  region_mapping <- toolGetMapping(name = 'regionmapping_21_EU11.csv',
                                   type = 'regional') %>%
    as_tibble() %>%
    select('iso3c' = .data$CountryCode, 'region' = .data$RegionCode)

  # extend industry subsector time series ----
  # subset of data containing industry subsector products and flows
  data_industry <- data %>%
    `[`(,,intersect(getNames(data),
                    cartesian(products_to_fix,
                              c(flows_to_fix, 'TOTIND', 'INONSPEC')))) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(iso3c = 'Region', year = 'Year', product = 'Data1', flow = 'Data2',
           value = 'Value') %>%
    mutate(year = as.integer(as.character(.data$year))) %>%
    filter(0 != .data$value) %>%
    inner_join(region_mapping, 'iso3c') %>%
    assert(not_na, .data$region)

  # all products that are consumed only in the non-specified subsector of
  # industry are "suspicious" and are therefore fixed
  data_to_fix <- inner_join(
    data_industry %>%
      filter('TOTIND' != .data$flow) %>%
      group_by(.data$iso3c, .data$region, .data$year, .data$product) %>%
      summarise(total = sum(.data$value, na.rm = TRUE), .groups = 'drop'),

    data_industry %>%
      filter(.data$flow %in% c('TOTIND', 'INONSPEC')) %>%
      spread(.data$flow, .data$value),

    c('iso3c', 'region', 'year', 'product')
  ) %>%
    # abs(1 - (.data$total / .data$TOTIND)) > 1e-3 |
    filter(.data$INONSPEC == .data$TOTIND) %>%
    select(.data$iso3c, .data$region, .data$year, .data$product, .data$TOTIND)

  # use all non-suspicious data to calculate regional and global averages
  data_for_fixing <- anti_join(
    data_industry %>%
      filter('TOTIND' != .data$flow),

    data_to_fix %>%
      select(-.data$TOTIND),

    c('iso3c', 'region', 'year', 'product')
  ) %>%
    as_tibble()

  data_for_fixing <- full_join(
    # compute global averages
    data_for_fixing %>%
      group_by(.data$year, .data$product, .data$flow) %>%
      summarise(value = sum(.data$value), .groups = 'drop_last') %>%
      mutate(global_share = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-.data$value) %>%
      # and expand to all regions
      mutate(region = NA_character_) %>%
      complete(nesting(!!!syms(c('year', 'product', 'flow', 'global_share'))),
               region = unique(region_mapping$region)),

    # compute regional averages
    data_for_fixing %>%
      group_by(.data$year, .data$region, .data$product, .data$flow) %>%
      summarise(value = sum(.data$value), .groups = 'drop_last') %>%
      mutate(regional_share = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-.data$value),

    c('year', 'region', 'product', 'flow')
  ) %>%
    # use regional averages if available, global averages otherwise
    mutate(value = ifelse(!is.na(.data$regional_share), .data$regional_share,
                          .data$global_share)) %>%
    select(-.data$regional_share, -.data$global_share) %>%
    interpolate_missing_periods_(
      periods = list(year = sub('^y([0-9]{4})$', '\\1', getYears(data)) %>%
                       as.integer() %>%
                       sort()),
      expand.values = TRUE, method = 'linear')

  # calculated fixed data
  data_industry_fixed <- left_join(
    data_to_fix,
    data_for_fixing,
    c('region', 'year', 'product')
  ) %>%
    # replace "suspicious" data with averages
    mutate(value = .data$TOTIND * .data$value) %>%
    select(.data$iso3c, .data$region, .data$year, .data$product, .data$flow,
           .data$value) %>%
    assert(not_na, .data$value) %>%
    overwrite(data_industry) %>%
    select(COUNTRY = .data$iso3c, TIME = .data$year, PRODUCT = .data$product,
           FLOW = .data$flow, Value = .data$value) %>%
    as.magpie()

  # replace fixed data
  data[getRegions(data_industry_fixed),
       getYears(data_industry_fixed),
       getNames(data_industry_fixed)] <- data_industry_fixed

  return(data)
}
