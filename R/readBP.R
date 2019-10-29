
#' BP Capacity and Generation Data
#' @description  BP data. See README in input file for more details.
#' @details Data contains historical electricity renewable capacities (in MW for Wind, Solar, and Geothermal) and Generation (in TWh for Nuclear, Hydro, Wind, Solar, Other Renewables, and Geo Biomass) 
#' @param subtype Either "Capacity" or "Generation"
#' @return A magpie object 
#' @author Aman Malik
#' @importFrom tidyr gather
#' @importFrom dplyr filter
#' @importFrom readxl read_excel
#' @importFrom reshape merge_recurse




readBP <- function(subtype) {
  y3 <- NULL
  Country <- NULL
  Year <- NULL
  filename <- c("bp-statistical-review-of-world-energy-2017-underpinning-data.xlsx")
  # Function to tidy input data, y1 is the dataframe and y2 is the specific technology
  tidy_data <- function(y1,y2){
    columns2remove <- c("2005-15","2016__1", "2016__2") # some columns in excel sheet converted to these forms while reading from readxl. Not required.
    rows2remove <- c("Total|CIS|OECD|European")
    colnames(y1)[1] <- "Country" # renaming column name
    y1$Country <- gsub("\\. ", " ",y1$Country) # Removing dots in country names
    y1 <- y1 %>%
      gather(colnames(y1[1,-1]),key="Year", value=y3) %>%  # data in wide format, y3 are capacity/generation values
      filter(!grepl(rows2remove, Country),!is.na(y3),!y3=="n/a",!Year %in% columns2remove) # removing entries with criteria columns2rmove, rows2remove, and 'n/a'
    
    y1$y3 <- as.numeric(y1$y3) # change type from default (character) to numeric
    colnames(y1)[3] <- y2
    return(y1)
    }
    
    
  
  # Capacity Data for Wind, Solar, and Geobiomass ---------------------------
  if (subtype == "Capacity") {
    
    y1 <- read_excel(filename, sheet ="Solar capacity" ,skip = 3)
    data_solar <- tidy_data(y1,"Solar")
    y1 <- read_excel(filename, sheet ="Wind capacity" ,skip = 3)
    data_wind <- tidy_data(y1,"Wind")
    y1 <- read_excel(filename, sheet ="Geothermal capacity" ,skip = 3)
    data_geothermal <- tidy_data(y1,"Geothermal")
    
    #Merging dataframes, accepting all values
    my_list <- list(data_solar,data_wind,data_geothermal)
    data <- merge_recurse(my_list)
  } 
  # Generation data for Nuclear, Hydro, Solar, Wind, Geobiomass, Other Renewables--------
  else if(subtype== "Generation") {
   y1 <- read_excel(filename, sheet = "Nuclear Consumption - TWh", skip = 2)
   data_nuclear <- tidy_data(y1,"Nuclear")
    
   y1 <- read_excel(filename, sheet = "Hydro Consumption - TWh", skip = 2)
   data_hydro <- tidy_data(y1,"Hydro")
  
   data_other_renewables <- read_excel(filename,  sheet = "Other renewables -TWh", skip = 2)
   data_other_renewables <- tidy_data(y1,"Other_Renewables")
   
   y1 <- read_excel(filename, sheet = "Solar Consumption - TWh", skip = 2)
   data_solar <- tidy_data(y1,"Solar")
   
   y1 <- read_excel(filename, sheet = "Wind Consumption - TWh", skip = 2)
   data_wind <- tidy_data(y1,"Wind")
   
   y1 <- read_excel(filename, sheet = "Geo Biomass Other - TWh", skip = 2)
   data_geo_biomass <- tidy_data(y1,"Geo_biomass")
   
   my_list <- list(data_wind,data_solar,data_hydro,data_geo_biomass,data_nuclear,data_other_renewables)
   data <- merge_recurse(my_list) # merging all datasets into one
   data <- filter(data,!grepl("\\.",data$Year))
   
  }
  else {
    stop("Not a valid subtype!")
  }
  x <- as.magpie(data,temporal=2,spatial=1,datacol=3) 
  return(x)
  
}





