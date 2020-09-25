#' Read European Environment Agency (EEA) data
#' 
#' Read-in European Environment Agency (EEA) data on ETS emissions as magclass object
#' 
#' 
#' @param subtype data subtype. "ETS" or "historical"
#' @return magpie object of European Environment Agency (EEA) ETS emissions (GtCO2) 
#' @author Renato Rodrigues
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="EEA_EuropeanEnvironmentAgency",subtype="ETS")
#' }
#'  
#' @importFrom magclass as.magpie
#' 

readEEA_EuropeanEnvironmentAgency <- function(subtype) {
   
  if (subtype == "ETS") { 
    #Reading ETS emissions
    data <- read.csv("(Tab)_Emissions_by_sector_data.csv")
    data <- data[,-c(2,6)] # removing unit columns
    data <- data[(!(is.na(data$Years))),] #removing NA Years
    colnames(data) <- c("region","ETS_info","sector","period","value")
    data$region <- toolCountry2isocode(data$region)
    data <- data[(!(is.na(data$region))),] #removing NA countries
    data$value <- as.numeric(gsub(" ","",data$value))/1000000 # changing value column to numeric and changing unit to Mt CO2-eq
    x <- as.magpie(data,spatial=1,temporal=4,datacol=5)
  } else if (subtype == "historical") {
    data <- read.csv(file='eea_historical_2005.csv', stringsAsFactors = FALSE, strip.white = TRUE)
    data <- data[,c("region", "variable", "period", "value")]
    data$variable <- paste0(data$variable, sep=" ", "(Mt CO2-equiv/yr)")
    x <- as.magpie(data,spatial=1,datacol=4,temporal=3)
  } else {
    stop("Not a valid subtype!")
  }
  
  return(x)
 }  
