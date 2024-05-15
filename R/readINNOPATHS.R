#' Read INNOPATHS
readINNOPATHS <- function() {
  utils::read.csv(file = "INNPATHS database.csv", sep = ";") %>%
    tidyr::pivot_longer(tidyselect::starts_with("X"),
                        names_to = "period",
                        names_pattern = "X(.*)",
                        names_transform = as.numeric) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::select(-"MODEL", -"SCENARIO") %>%
    as.magpie(spatial = 1, temporal = 4)
}
