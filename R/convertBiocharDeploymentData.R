#' Provide Biochar price path data
#'
#' @author Tabea Dorndorf
#'
convertBiocharDeploymentData <- function(x) {
  map0 <- toolGetMapping("regionmappingH12.csv", type = "regional",
                         where = "mappingfolder")

  weights0 <- readxl::read_xlsx(file.path("BiocharDeploymentData_2411.xlsx"),
                                sheet = "EURcountries")


  map <- merge(map0, weights0, by = "CountryCode", all.x = TRUE)

  map <- map %>% group_by(RegionCode) %>%
    mutate(OtherRegionShareCounts = sum(!is.na(data))) %>% ungroup()

  map <- map %>% group_by(RegionCode) %>%
    mutate(RegionCountTotal = dplyr::n()) %>% ungroup()

  map <- map %>% group_by(RegionCode) %>%
    mutate(RemainingShare = 1- sum(data, na.rm = TRUE)) %>% ungroup()

  map <- map %>% mutate(RemainingRegionCount = RegionCountTotal - OtherRegionShareCounts) %>%
    mutate(data2 = RemainingShare * 1/RemainingRegionCount)

  map <- map %>% mutate(data = dplyr::if_else(is.na(data), data2, data))

  weight <- map %>% select(c(CountryCode, data)) %>%
    as.magpie()

  x <- toolAggregate(x, rel = map0, from = "RegionCode", to = "CountryCode",
                     weight = weight)

  return(x)
}
