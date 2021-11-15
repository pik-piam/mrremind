#' @importFrom dplyr filter %>%
#' @importFrom rlang sym
#' 
readIEA_WEO_2021 <- function() {
  variable <- NULL
  
  data <- rbind(
    read.csv(file = "WEO2021_Free_Data_Regions.csv")  %>% mutate(source := "A"),
    read.csv(file = "WEO2021_Free_Data_Supply_Refining.csv") %>% mutate(source := "B"),
    read.csv(file = "WEO2021_Free_Data_World.csv") %>% mutate(source := "C")
  ) %>% mutate(
    variable := paste0(!!sym("Category"), "-", !!sym("Product"), "-", !!sym("Flow"), " (", !!sym("Unit"), ")"),
    year = as.numeric(!!sym("Year"))
  ) %>% select("Region", "year", "Scenario", "variable", "Value") %>%
    group_by(!!sym("Region"), !!sym("year"), !!sym("Scenario"), !!sym("variable")) %>%
    distinct() %>%
    ungroup()
  
  x <- as.magpie(data, temporal = 2, spatial = 1, datacol = 5)
  x <- magpiesort(x)

}