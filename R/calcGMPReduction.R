#' Calculates Global Methane Pledge (GMP) pledged CH4 emissions reductions in 2030 for each region,
#' assuming 30% reduction relative to 2020 levels and using CEDS2020 emissions as weights.
#'
#' Information about the list of countries that have commited to the GMP
#' can be found under mrremind::readGlobalMethanePledge()
#'
#' @return MAgPIE object with regional pledged reduction fraction of 2020 emissions by all countries,
#' weighted by CEDS2020 emissions of pledged countries
#' @author Gabriel Abrahao
calcGMPReduction <- function() {
    # 2020 emissions to use as aggregation weights
    fullceds <- readSource("CEDS2024")[, 2020, "ch4"] # in MtCH4
    emis2020 <- setYears(dimSums(fullceds, dim = 3), NULL)

    # Binary data of all countries, with 1 if it has commited to the GMP
    ismember <- readSource("GlobalMethanePledge")

    # Preallocating with ismember
    x <- ismember

    x <- new.magpie(
        cells_and_regions = getISOlist(),
        years = NULL,
        fill = 0
    )

    x[getRegions(ismember),,] <- x[getRegions(ismember),,] + ismember

    # The GMP countries commit to reduce their emissions by 30% (relative to 2020 levels) until 2030
    x[, , ] <- x[, , ] * 0.30

    return(list(
        x = x,
        weight = emis2020,
        unit = "fraction of 2020 emissions",
        description = "Pledged reduction fraction of 2020 emissions by all countries, weighted by CEDS2020 emissions"
    ))
}
