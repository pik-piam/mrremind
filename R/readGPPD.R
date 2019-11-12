#' Capacities from the Global Power Plant Database
#' @description The database includes i) Technical characteristics (fuel, technology, ownership) ii) Operational characteristics
#' (generation) iii) Plants’ geolocation iv) Plants over 1 megawatt (MW) v) Plants in operation only. 
#' However, only iv) Capacities are extracted. For more information see https://resourcewatch.org/data/explore/Powerwatch
#' @param x MAgPIE object to be converted
#' @return Magpie object, country capacities (MW) from the Global Power Plant Database for different fuels
#' @author Aman Malik


readGPPD <- function()
{
  input <- read.csv("global_power_plant_database.csv")
  # summing capacity over country and type
  input <- input %>% 
    group_by(country,primary_fuel) %>% 
    summarise(capacity=sum(capacity_mw))
  x <- as.magpie(input, spatial=1,data=2)
  return(x)
}

