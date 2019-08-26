#' Read Janbeck 2011 Coastal Population data, for Ocean Plastic input
#' 
#' Read-in a xlsx file as magclass object
#' kg/cap
#' 
#' @return magpie object of coastal population by country, 50km buffer around coastline, 
#' keeping percentage same in future
#' @author David Chen
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="CoastalPop")
#' }
#' 
#' @importFrom magclass as.magpie

readCoastalPop <- function() {
  #### x <- read.csv("C:/PIK/inputdata/sources/CoastalPop/JanbeckOceanPlastic.csv")
x <- read.csv("JanbeckOceanPlastic.csv")
x <- x[,c(1,3)]
year <- rep("y2015", 192)
x <- cbind(x, year)


region <- as.vector(x[,1])
region[32] <- "Cocos (Keeling) Islands"
region[35] <- "Congo"
region[36] <- "Congo, the Democratic Republic of the"
region[56] <- "Falkland Islands (Malvinas)"
region[96] <- "Korea, Democratic People's Republic of"
region[97] <- "Korea, Republic of"
region[150] <- "Sint Maarten (Dutch part)"
region[151] <- "Saint Pierre and Miquelon"
region[174] <- "Gambia"
region[188] <- "Virgin Islands, U.S."

region <- toolCountry2isocode(region)

x <- cbind(x, region)
x <- x[,c(4,3,2)]
#remove channel islands and dhekelia which are not recognized by magclass, and Svalbard? which gets converted to REU somehow
x <- x[-c(28,45, 168),]
colnames(x) <- c("country", "year", "coastal population")

x <- as.magpie(x, spatial=1, temporal=2)

return(x)
}


