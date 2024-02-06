#' Share of Industry Subsectors in FE Use
#'
#' Calculates industry subsector shares in final energy carrier use for the
#' `fixed_shares` realisation of the `industry` module.
#'
#' For the region mapping `regionmapping_21_EU11.csv`, these are based on IEA
#' data from `calcOutput(type = 'FEdemand')`, for all other
#' region mappings on vintage data which is ultimately based on Enerdata data.
#'
#' @note There is a discrepancy between the shares calculated from these two
#' sources, that will affect REMIND emission reporting.
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Lavinia Baumstark
#' @author Michaja Pehl
#'
#' @seealso [`calcOutput()`][madrat::calcOutput].
#' @md
#'
#' @importFrom dplyr as_tibble select filter inner_join tribble group_by
#'   summarise mutate ungroup
#' @importFrom quitte character.data.frame
#' @importFrom tidyr pivot_wider pivot_longer complete
#'
#' @export
calcShareIndFE <- function() {
  if ('regionmapping_21_EU11.csv' != getConfig('regionmapping')) {
    x <- readSource("REMIND_11Regi",subtype="shareIndFE")
    w <- calcOutput("IO",subtype="output",aggregate=FALSE)[,2010,]
  } else {
    x <- calcOutput(type = 'FEdemand', aggregate = FALSE) %>%
      as.data.frame() %>%
      as_tibble() %>%
      select('iso3c' = 'Region', 'year' = 'Year', 'scenario' = 'Data1',
             'pf' = 'Data2', 'value' = 'Value') %>%
      character.data.frame() %>%
      filter('2005' == !!sym('year'),
             'gdp_SSP2' == !!sym('scenario')) %>%
      select(-'year', -'scenario') %>%
      inner_join(
        tribble(
          ~pf.fixed_shares,   ~sector.fixed_shares,   ~pf.subsectors,
          'fesoi',            'cement',               'feso_cement',
          'fesoi',            'chemicals',            'feso_chemicals',
          'fesoi',            'steel',                'feso_steel',
          'fesoi',            'otherInd',             'feso_otherInd',
          'fehoi',            'cement',               'feli_cement',
          'fehoi',            'chemicals',            'feli_chemicals',
          'fehoi',            'steel',                'feli_steel',
          'fehoi',            'otherInd',             'feli_otherInd',
          'fegai',            'cement',               'fega_cement',
          'fegai',            'chemicals',            'fega_chemicals',
          'fegai',            'steel',                'fega_steel',
          'fegai',            'otherInd',             'fega_otherInd',
          'fehei',            'cement',               NA,
          'fehei',            'chemicals',            NA,
          'fehei',            'steel',                NA,
          'fehei',            'otherInd',             'fehe_otherInd',
          'feeli',            'cement',               'feel_cement',
          'feeli',            'chemicals',            'feelhth_chemicals',
          'feeli',            'chemicals',            'feelwlth_chemicals',
          'feeli',            'steel',                'feel_steel_primary',
          'feeli',            'steel',                'feel_steel_secondary',
          'feeli',            'otherInd',             'feelhth_otherInd',
          'feeli',            'otherInd',             'feelwlth_otherInd'),
        c('pf' = 'pf.subsectors')
      ) %>%
      select(-'pf', !!sym('pf') := 'pf.fixed_shares') %>%
      group_by(!!sym('iso3c'), !!sym('pf'), !!sym('sector.fixed_shares')) %>%
      summarise(!!sym('value') := sum(!!sym('value'), na.rm = TRUE),
                .groups = 'drop_last') %>%
      mutate(!!sym('value') := !!sym('value') / sum(!!sym('value'),
                                                    na.rm = TRUE)) %>%
      ungroup() %>%
      filter('otherInd' != !!sym('sector.fixed_shares')) %>%
      pivot_wider(names_from = 'pf', values_fill = 0) %>%
      mutate(!!sym('feh2i') := !!sym('fegai')) %>%
      pivot_longer(matches('^fe..i$'), names_to = 'pf', ) %>%
      complete(
        !!sym('iso3c'),
        !!sym('sector.fixed_shares') := c('cement', 'chemicals', 'steel'),
        !!sym('pf') := c('fesoi', 'fehoi', 'fegai', 'feh2i', 'fehei', 'feeli'),
        fill = list(value = 0)) %>%
      select('region' = 'iso3c', 'type' = 'pf',
             'variable' = 'sector.fixed_shares', 'value') %>%
      as.magpie()

    w <- calcOutput('IO', subtype = 'output', aggregate = FALSE)[,2015,]
  }

  w <- w[,,intersect(getNames(x, dim = 1),getNames(w, dim=2))]
  w <- dimSums(w,dim=c(3.1,3.3))
  # duplicate fegai as a weight for feh2i
  w <- mbind(w, setNames(w[,,'fegai'], 'feh2i'))

  return(list(x           = x,
              weight      = w,
              unit        = "ratio",
              description = "share of industry sub-sectors in FE use"))
}

# Extension notes MJP 2019-07-25
# # Add shares for H2, heat, and electricity, since they are needed for the new
# # emissions accounting by Renato.
# bind_rows(
#   # Read electricity shares from 2010 IEA data
#   calcOutput('IO', subtype = 'output_Industry_subsectors') %>%
#     as.data.frame() %>%
#     as_tibble() %>%
#     filter(2010 == Year,
#            grepl('^feel[^_]*_.*', Data2)) %>%
#     select(region = Region, pf = Data2, value = Value) %>%
#     character.data.frame() %>%
#     separate(pf, c('type', 'sector'), sep = '_') %>%
#     mutate(type = sub('feel.*$', 'feeli', type)) %>%
#     group_by(region, sector, type) %>%
#     summarise(value = sum(value)) %>%
#     group_by(region, type) %>%
#     mutate(share = value / sum(value)) %>%
#     ungroup() %>%
#     arrange(region, type) %>%
#     select(-value) %>%
#     filter('otherInd' != sector) %>%
#     # put 100 % of heat into otherInd
#     complete(nesting(region, sector), type = c('feeli', 'fehei')) %>%
#     mutate(share = round(ifelse('fehei' == type, 0, share), 2)) %>%
#     spread(sector, share),
#
#   # use gas shares for H2
#   read_delim(
#     file = '~/PIK/swap/inputdata/sources/REMIND_11Regi/shareIndustyFE.csv',
#     delim = ';',
#     col_types = 'ccddd',
#     skip = 3) %>%
#     filter('fegai' == type) %>%
#     mutate(type = 'feh2i')
# ) %>%
#   write_delim(
#     path = '~/PIK/swap/inputdata/sources/REMIND_11Regi/shareIndustyFE.csv',
#     delim = ';',
#     append = TRUE,
#     col_names = FALSE)
