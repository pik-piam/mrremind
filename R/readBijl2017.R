#' Read Bijl2017
#' 
#' Read-in the data from Bijl 2017 food demand paper (http://dx.doi.org/10.1016/j.gloenvcha.2017.04.003)
#' 
#' 
#' @param subtype data subtype. 
#' @return magpie object of the Bijl et. al 2017 data
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("Bijl2017","demand",convert=FALSE)
#' }
#' 
#' @importFrom magclass as.magpie
#' @importFrom magclass magpiesort
#' @importFrom madrat toolSubtypeSelect
#' @export



readBijl2017 <- function(subtype){
  if(subtype=="calib_demand_income"){csvtoread="Fig 2 - Calibrating Food Demand on Income.csv"}
  if(subtype=="Bijl_vs_hist"){csvtoread="Fig 3 - Comparing model with historical data (5 regions).csv"}
  if(subtype=="demand"){csvtoread="Fig 4 - SSP Scenario Projections (dry matter) (upper panel).csv"}
  if(subtype=="inequality"){csvtoread="Fig 5 - Regional and Local Inequality (SSP2) (Aggregated) (all regions).csv"}
  if(subtype=="undernourished"){csvtoread="Fig 6 - Effect of income inequality on estimated hunger (all time).csv"}
  if(subtype=="comparison_faoProj_developing"){csvtoread="Fig 8a - Comparison to FAO Projection (Developing countries).csv"}
  if(subtype=="comparison_faoProj_food"){csvtoread="Fig 8b - Comparison to FAO Projection (Total food (kcal_person_day)).csv"}
  if(subtype=="comparison_literature"){csvtoread="Fig 9 - Literature Comparison.csv"}
  
  if (subtype=="calib_demand_income") {
    # read the data
    data <- read.csv(csvtoread,header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
    
    # Adjust columns
    colnames(data) <- tolower(colnames(data))
    colnames(data) <- gsub(pattern = "\\.",replacement = "_",x = colnames(data))
    
    # fix names in the "foods" columns
    data$foods <- gsub(pattern = " ",replacement = "_",x = data$foods)
    
    # Make a magpie object out of this
    to_convert <- data[,c(2,3,1,4:32)]
    to_convert$year <- paste0("y",to_convert$year)
    data_mag <- as.magpie(to_convert,spatial=1,temporal=3) #### UNITS ARE: kcal/cap/day ####
    
    # Renaming of dimensions
    names(dimnames(data_mag))[1] <- "region"
    names(dimnames(data_mag))[2] <- "year"
    names(dimnames(data_mag))[3] <- "scenario.FoodCategory"
    
    # return the magpie object
    return(data_mag)
  } 
  else
  if (subtype=="Bijl_vs_hist") {
    # read the data
    data <- read.csv(csvtoread,header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
    
    # Adjust columns
    colnames(data) <- tolower(colnames(data))
    
    # fix names in the "foods" columns and "thing" coulmn
    data$foods <- gsub(pattern = " ",replacement = "_",x = data$foods)
    data$foods <- gsub(pattern = "\\\n",replacement = "_",x = data$foods)
    
    data$thing <- gsub(pattern = "Model",replacement = "Bijl_et_al_17",x = data$thing)
    data$thing <- gsub(pattern = " ",replacement = "_",x = data$thing)
    
    # Make a magpie object out of this
    to_convert <- data[,c(2,3,5,1,4)]
    to_convert$year <- paste0("y",to_convert$year)
    data_mag <- as.magpie(to_convert,spatial=1,temporal=4) #### UNITS ARE: kcal/cap/day ####
    
    # Renaming of dimensions
    names(dimnames(data_mag))[3] <- "scenario.source"
    
    # return the magpie object
    return(dimOrder(x = data_mag,perm = c(2,1)))
  }
  else
  if (subtype=="demand") {
    # read the data
    data <- read.csv(csvtoread,header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
    
    # Rename X in years to y
    colnames(data) <- gsub(pattern = "^X",replacement = "y",x = colnames(data))
    
    # Subset the data by removig the column "popseg". Our region data is already in column "region" which has to be changed to "GLO"
    data <- data[ , -which(names(data) %in% c("popseg"))]
    data$region <- "GLO"
    
    # Dash in SSPs has to be changed to underscore
    data$scenario <- gsub(pattern = "\\-",replacement = "_",x = data$scenario)
    data$scenario <- tolower(data$scenario)
    
    # Food categories need to be renamed with removal of space and ANDs
    data$FoodCategory <- gsub(pattern = " \\& ",replacement = " and ",x = data$FoodCategory)
    data$FoodCategory <- gsub(pattern = " ",replacement = "_",x = data$FoodCategory)
    
    # Make a magpie object out of this
    data_mag <- as.magpie(data) #### UNITS ARE: kcal/cap/day ####
    
    # Renaming of dimensions
    names(dimnames(data_mag))[1] <- "region"
    names(dimnames(data_mag))[2] <- "year"
    names(dimnames(data_mag))[3] <- "scenario.FoodCategory"
    
    # return the magpie object
    return(data_mag)
  }
  else
  if (subtype=="inequality") {
    # read the data
    data <- read.csv(csvtoread,header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
    
    # Adjust columns
    data <- data[,-4]
    
    # Make a magpie object out of this
    to_convert <- data[,c(2,3,1,4)]
    to_convert$year <- paste0("y",to_convert$year)
    data_mag <- as.magpie(to_convert,spatial=1,temporal=3) #### UNITS ARE: kcal/cap/day ####
  
    # return the magpie object
    return(data_mag)
  }
  else 
  if(subtype=="undernourished"){
    # read the data
    data <- read.csv(csvtoread,header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
    
    # Rename columns
    colnames(data) <- gsub(pattern = "\\.",replacement = "_",x = colnames(data))
    
    # "data source" need to be renamed with removal
    data$data_source <- gsub(pattern = " ",replacement = "_",x = data$data_source)
   
    # Make a magpie object out of this
    data_mag <- as.magpie(data,spatial=2) #### UNITS ARE: million (capita) ####

    # return the magpie object
    return(data_mag)
  }
  else
  if (subtype=="comparison_faoProj_developing") {
  # read the data
  data <- read.csv(csvtoread,header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
  
  # Replace dot with underscore
  colnames(data) <- gsub(pattern = "\\.",replacement = "_",x = colnames(data))
  colnames(data) <- gsub(pattern = "thing",replacement = "source",x = colnames(data))
  
  # Content of "source","region_alexandratos" and "food_alexandratos" need to be corrected
  data$source <- gsub(pattern = " ",replacement = "_",x = data$source)
  data$source <- gsub(pattern = "^Model$",replacement = "Bijl_et_al_17",x = data$source)
  data$source <- gsub(pattern = "^Model_w._FAO_income$",replacement = "Bijl_w_FAOincome",x = data$source)
  data$source <- gsub(pattern = "\\._",replacement = "_",x = data$source)
  
  data$region_Alexandratos <- gsub(pattern = " ",replacement = "_",x = data$region_Alexandratos)
  
  data$food_Alexandratos <- gsub(pattern = ", ",replacement = "_",x = data$food_Alexandratos)
  data$food_Alexandratos <- gsub(pattern =  " *\\(.*?\\) *",replacement =  "", x = data$food_Alexandratos)
  # data$food_Alexandratos <- gsub(pattern = "\\(kcal\\/person\\/day\\)",replacement = "kc_pc_pd",x = data$food_Alexandratos)
  data$food_Alexandratos <- gsub(pattern = " ",replacement = "_",x = data$food_Alexandratos)
  data$food_Alexandratos <- gsub(pattern = "Vegetable_oils_oilseeds_and_products",replacement = "Vegetable_oils",x = data$food_Alexandratos)
  data$food_Alexandratos <- gsub(pattern = "Milk_and_dairy_excl._butter",replacement = "Dairy",x = data$food_Alexandratos)
  data$food_Alexandratos <- gsub(pattern = "Sugar_and_sugar_crops",replacement = "Sugar",x = data$food_Alexandratos)
  
  # make magpie object dropping the indexed part 
  to_convert <- data[,c(-7)]
  data_mag <- magpiesort(as.magpie(to_convert, temporal = 4))
  getRegions(data_mag) <- getNames(data_mag,dim = 2)
  data_mag <- collapseNames(data_mag)
  return(data_mag)
  }
  else
  if (subtype=="comparison_faoProj_food") {
    # read the data
    data <- read.csv(csvtoread,header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
    
    # Replace dot with underscore
    colnames(data) <- gsub(pattern = "\\.",replacement = "_",x = colnames(data))
    colnames(data) <- gsub(pattern = "thing",replacement = "source",x = colnames(data))
    
    # Content of "source","region_alexandratos" and "food_alexandratos" need to be corrected
    data$source <- gsub(pattern = " ",replacement = "_",x = data$source)
    data$source <- gsub(pattern = "^Model$",replacement = "Bijl_et_al_17",x = data$source)
    data$source <- gsub(pattern = "^Model_w._FAO_income$",replacement = "Bijl_w_FAOincome",x = data$source)
    data$source <- gsub(pattern = "\\._",replacement = "_",x = data$source)
    
    data$region_Alexandratos <- gsub(pattern = " ",replacement = "_",x = data$region_Alexandratos)
    data$region_Alexandratos <- gsub(pattern = "-",replacement = "",x = data$region_Alexandratos)
    data$region_Alexandratos <- gsub(pattern = "\\/",replacement = "and",x = data$region_Alexandratos)
    
    data$food_Alexandratos <- gsub(pattern = "\\(kcal\\/person\\/day\\)",replacement = "kc_pc_pd",x = data$food_Alexandratos)
    data$food_Alexandratos <- gsub(pattern = " ",replacement = "",x = data$food_Alexandratos)
    data$food_Alexandratos <- gsub(pattern = "foodkc",replacement = "food_kc",x = data$food_Alexandratos)
    
    # make magpie object dropping the indexed part
    to_convert <- data[,c(-3,-7)]
    data_mag <- as.magpie(to_convert,spatial=2,temporal=3)
    return(data_mag) ### unit is kcal/cap/day
  } else 
  if(subtype=="comparison_literature"){
    # read the data
    data <- read.csv(csvtoread,header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
    
    # Rename X in years to y
    colnames(data) <- gsub(pattern = "^X",replacement = "y",x = colnames(data))
    
    # Content of "model" need to be renamed
    data$model <- gsub(pattern = " ",replacement = "_",x = data$model)
    data$model <- gsub(pattern = "^Our_model$",replacement = "Bijl_et_al_17",x = data$model)
    data$model <- gsub(pattern = "^Our_model_w._FAO_income$",replacement = "Bijl_w_FAOincome",x = data$model)
    
    # Make a magpie object out of this dropping the column percentage
    data_mag <- as.magpie(data[,1:5],temporal=4) #### UNITS ARE: million (capita) ####
    
    # # Make a magpie object out of data dropping the time but keeping percentage
    # data_mag_percentage <- as.magpie(data[,c(1:3,6)]) #### UNITS ARE: million (capita) ####
    # 
    # data_mag_percentage_expanded <- magpie_expand(data_mag_percentage,ref = data_mag)
    # temp <- add_dimension(x = data_mag,dim = 3.4,add = "type",nm = "slope")
    # temp2 <- add_dimension(x = data_mag_percentage_expanded,dim = 3.4,add = "type",nm = "percentage")
    # data_to_return <- mbind(temp,temp2)
    # return the magpie object
    # return(data_to_return)
    return(data_mag) ### World total calories (indexed 2005)
  }
  
  else {stop("Invalid subtype ", subtype)}
} 