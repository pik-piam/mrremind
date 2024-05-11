#' Read INNOPATHS
#'
#' @author Falk Benke
#' @importFrom utils read.csv
#' @importFrom reshape2 melt
#' @importFrom dplyr select filter mutate %>%

readINNOPATHS <- function() {
  
  data <- read.csv(file = "INNPATHS database.csv", sep = ";")

  data <- melt(data, id.vars = 1:5, variable.name = "period", value.name = "value") %>% 
    filter(!is.na(!!sym("value"))) %>%
    mutate(!!sym("value") := as.double(!!sym("value")),
           !!sym("period") := as.numeric(sub("X","",!!sym("period")))) %>%
    select(-c("MODEL", "SCENARIO"))
  
  x <- as.magpie(data, spatial = 1, temporal = 4)

  return(x)
}
