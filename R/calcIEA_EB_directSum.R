#' Calculate simple sums of IEA Energy Balances
#'
#' Calculates `PE|Coal`, `PE|Oil`, `PE|Gas`, and `FE` as direct sums of relevant
#' products for `TES` (PEs) or `TFC` (FE), minus (negative) bunkers.  Used for
#' benchmarking in the `historical.mif` and for the IIASA Scenario Compass
#' Initiative vetting.
#'
#' @md
#' @importFrom madrat readSource
#' @importFrom magclass dimSums mbind setNames
#' @importFrom quitte cartesian
#'
#' @returns A [`magpie`][magclass::magclass] object.
#' @seealso [`calcOutput()`][madrat::calcOutput]

#' @export
calcIEA_EB_directSum <- function()
{
    iea_data <- readSource('IEA', subtype = 'EnergyBalances-latest') * 4.1868e-5

    vars <- list(
        list(variable = 'PE|Coal',
             products = c('HARDCOAL', 'BROWN', 'ANTCOAL', 'COKCOAL', 'BITCOAL',
                          'SUBCOAL', 'LIGNITE'),
             flows_positive = 'TES',
             flows_negative = c('AVBUNK', 'MARBUNK')),

        list(variable = 'PE|Oil',
             products = c('OILSHALE', 'PARWAX', 'CRUDEOIL', 'NGL', 'NONCRUDE',
                          'NONBIOGASO', 'NONBIOJETK', 'OTHKERO', 'NONBIODIES',
                          'RESFUEL', 'NAPHTHA', 'WHITESP', 'LUBRIC', 'ONONSPEC',
                          'AVGAS', 'COALTAR', 'REFFEEDS', 'ADDITIVE', 'JETGAS',
                          'CRNGFEED', 'LPG', 'BITUMEN'),
             flows_positive = 'TES',
             flows_negative = c('AVBUNK', 'MARBUNK')),

        list(variable = 'PE|Gas',
             products = 'NATGAS',
             flows_positive = 'TES',
             flows_negative = c('AVBUNK', 'MARBUNK')),

        list(variable = 'FE',
             products = 'TOTAL',
             flows_positive = 'TFC',
             flows_negative = c('AVBUNK', 'MARBUNK')))

    x <- lapply(vars, function(vars)
    {
        product_flows_positive <- intersect(getNames(iea_data),
                                            cartesian(vars[['products']],
                                                      vars[['flows_positive']]))
        product_flows_negative <- intersect(getNames(iea_data),
                                            cartesian(vars[['products']],
                                                      vars[['flows_negative']]))

        setNames(
            ( dimSums(iea_data[,,product_flows_positive], dim = 3)
            - dimSums(iea_data[,,product_flows_negative], dim = 3)
            ),
            vars[['variable']])
    }) |>
        mbind()

    return(list(x = x, weight = NULL, unit = 'EJ',
                description = 'Simple sums of IEA Energy Balances'))
}
