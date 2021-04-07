#' Read INNOPATHS
#'
#' @author Falk Benke
#' @importFrom utils read.csv
#' @importFrom reshape2 melt
#' @importFrom magclass as.magpie
#' @importFrom dplyr inner_join select

readINNOPATHS <- function() {
  
  period <- value <- conv_value <- REMIND <- REMIND_VARIABLE <- REMIND_UNIT <- REGION <- NULL
  
  data <- read.csv(file = "INNPATHS database.csv", sep = ";")
  data <- melt(data, id.vars = 1:5, variable.name = "period", value.name = "value") %>% filter(!is.na(value))
  data$value <- as.double(data$value)

  mapping <- read.csv(file = "Mapping_generic_INNOPATHS.csv", sep = ";") %>%
    filter(!is.na(REMIND))
  mapping$REMIND_UNIT <- gsub("\\)", "", gsub(".*\\(", "", mapping$REMIND))
  mapping$REMIND_VARIABLE <- gsub(" \\(.*", "", mapping$REMIND)
  mapping$INN_UNIT <- gsub("\\)", "", gsub(".*\\(", "", mapping$Variable))
  mapping$INN_VARIABLE <- gsub(" \\(.*", "", mapping$Variable)

  d <- inner_join(data, mapping, by = c(VARIABLE = "INN_VARIABLE")) %>%
    mutate(conv_value = value / factor) %>%
    select(REGION, period, REMIND_VARIABLE, REMIND_UNIT, conv_value)

  d$REMIND_VARIABLE <- paste0(d$REMIND_VARIABLE, " (", d$REMIND_UNIT, ")") 
  d$REMIND_UNIT <- NULL

  x <- as.magpie(d, spatial = 1, temporal = 2)

  return(x)
}
