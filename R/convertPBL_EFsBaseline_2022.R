#' Disaggregates PBL emission factors from the 26 IMAGE regions to ISO3
#' country level. As these are emission factors, weights are constant,
#' assuming all countries in a region emit the same amount of each gas
#' per unit of activity.
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @importFrom dplyr mutate select
#'
#' @export
convertPBL_EFsBaseline_2022 <- function(x) {
    imagemapping <- toolGetMapping("regionmapping_IMAGE_PBL_Stegmann2022.csv", type = "regional", where = "mrremind")
    remindmapping <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")

    xiso <- toolAggregate(x, rel = imagemapping, dim = 1,  from = "RegionCode", to = "CountryCode")

    lfilled <- lapply(getYears(xiso), \(yr){toolFillWithRegionAvg(xiso[,yr,], regionmapping = remindmapping, callToolCountryFill = T)})
    xfilled <- mbind(lfilled)

    return(xfilled)
}
