#' Converts timber share
#' Update dd-Jmm-jjjj - Please add comment if changes made here (Abhi)
#' 
#' @param x MAgPIE object to be converted
#' @return A MAgPIE object containing country disaggregated data
#' @author Abhijeet Mishra
#' @examples
#' 
#' \dontrun{ a <- readSource("TimberShare",convert=FALSE)
#' }
#' 
convertTimberShare<-function(x) {
map <- read.csv(toolMappingFile("regional",getConfig("regionmapping")),sep=";")
y <- toolAggregate(x = x,rel = map,from = "RegionCode",to = "CountryCode",dim = 1)

#y <- toolCountryFill(x, fill = NA, BHR="QAT", HKG="CHN", MUS="MDG", PSE="ISR", SGP="MYS", TLS="IDN")
  
return(y)
}