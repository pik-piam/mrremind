#' Read IRENA
#' 
#' Read-in an IRENA csv file as magclass object
#' 
#' 
#' @param subtype data subtype. Either "Capacity" or "Generation"
#' @return magpie object of the IRENA data with historical electricity renewable capacities (MW) or generation levels (GWh) 
#' @author Renato Rodrigues
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="IRENA",subtype="Capacity")
#' }
#'  
#' @importFrom reshape2 melt
#' @importFrom readr read_csv

 readIRENA <- function(subtype) {
   if (subtype == "Capacity") {
     #Reading renewables electricity capacity values in MW from csv
     data <- read_csv("Capacity.csv")
   } else if (subtype== "Generation") {
     #Reading renewables electricity generation values in GWh from csv
     data <- read_csv("Generation.csv")
   }else {
     stop("Not a valid subtype!")
   }
   # data in wide format 
   data <- melt(data, id.vars=c("Country/area", "Technology"), variable.name="years", value.name="value")  # melt requires library(reshape2)
   #replacing X by y on years preffix
   #data$years <- gsub("X", "y", data$years)
   # rearrange column order to more readable format: year, country, tech, value (capacity or generation)
   data <- data[,c(3,1,2,4)]  
   # creating capacity or generation magpie object
   x <- as.magpie(data,temporal=1,spatial=2,datacol=4)
   return(x)
 }  
