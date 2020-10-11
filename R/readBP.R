
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
  value <- NULL
  Country <- NULL
  Year <- NULL
  filename <- c("bp-stats-review-2020-all-data.xlsx")
  # Function to tidy input data, y1 is the dataframe and y2 is the specific technology
  tidy_data <- function(y1,y2){
    years <- as.character(c(1900:2020)) # only columns with years required
    rows2remove <- c("Total|OECD|European")
    colnames(y1)[1] <- "Country" # renaming column name
    y1$Country <- gsub("\\. ", " ",y1$Country) # Removing dots in country names
    y1 <- y1 %>%
      gather(colnames(y1[1,-1]),key="Year", value=value) %>%  
      filter(!grepl(rows2remove, Country),!is.na(value),!value=="n/a",Year %in% years) %>% 
      mutate(Year=as.integer(Year),value=as.numeric(value)) %>% 
      mutate(Country=gsub(pattern = " and ",replacement = " & ",x = Country))
    
    colnames(y1)[3] <- y2  
    
      return(y1)
    }
    
    
  
  # Capacity Data for Wind, Solar, and Geobiomass ---------------------------
  if (subtype == "Capacity") {
    
    y1 <- read_excel(filename, sheet ="Solar Capacity" ,range = "A4:Y69")
    data_solar <- tidy_data(y1,"Solar")
    y1 <- read_excel(filename, sheet ="Wind Capacity",range = "A4:Z70")
    data_wind <- tidy_data(y1,"Wind")
    y1 <- read_excel(filename, sheet ="Geothermal Capacity" ,range="A4:Z44")
    data_geothermal <- tidy_data(y1,"Geothermal")
    
    #Merging dataframes, accepting all values
    my_list <- list(data_solar,data_wind,data_geothermal)
    data <- merge_recurse(my_list)
  } 
  # Generation data for Nuclear, Hydro, Solar, Wind, Geobiomass, Other Renewables--------
  else if(subtype== "Generation") {
   y1 <- read_excel(filename, sheet = "Nuclear Generation - TWh", range = "A3:BD114")
   data_nuclear <- tidy_data(y1,"Nuclear")
    
   y1 <- read_excel(filename, sheet = "Hydro Generation - TWh", range="A3:BD114")
   data_hydro <- tidy_data(y1,"Hydro")
  
   y1 <- read_excel(filename, sheet = "Solar Generation - TWh", range="A3:BD114")
   data_solar <- tidy_data(y1,"Solar")
   
   y1 <- read_excel(filename, sheet = "Wind Generation - TWh", range="A3:BD114")
   data_wind <- tidy_data(y1,"Wind")
   
   y1 <- read_excel(filename, sheet = "Geo Biomass Other - TWh", range="A3:BD114")
   data_geo_biomass <- tidy_data(y1,"Geo_biomass")
   
   my_list <- list(data_wind,data_solar,data_hydro,data_geo_biomass,data_nuclear)
   data <- merge_recurse(my_list) # merging all datasets into one
   data <- filter(data,!grepl("\\.",data$Year))
   
  }
  else {
    stop("Not a valid subtype!")
  }
  x <- as.magpie(data,temporal=2,spatial=1,datacol=3) 
  x <- magpiesort(x)
  return(x)
  
}





