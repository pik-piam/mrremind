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

  map <- map %>%
    group_by(.data$RegionCode) %>%
    mutate("OtherRegionShareCounts" = sum(!is.na(.data$data))) %>%
    ungroup()

  map <- map %>%
    group_by(.data$RegionCode) %>%
    mutate("RegionCountTotal" = dplyr::n()) %>%
    ungroup()

  map <- map %>%
    group_by(.data$RegionCode) %>%
    mutate("RemainingShare" = 1 - sum(.data$data, na.rm = TRUE)) %>%
    ungroup()

  map <- map %>%
    mutate("RemainingRegionCount" =
             .data$RegionCountTotal - .data$OtherRegionShareCounts) %>%
    mutate("data2" = .data$RemainingShare * 1 / .data$RemainingRegionCount)

  map <- map %>%
    mutate("data" = dplyr::if_else(is.na(.data$data), .data$data2, .data$data))

  weight <- map %>% select(c(.data$CountryCode, .data$data)) %>% as.magpie()

  x <- toolAggregate(x, rel = map0, from = "RegionCode", to = "CountryCode",
                     weight = weight)

  return(x)
}
