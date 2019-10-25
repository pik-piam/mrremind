#' Data on Coal Plants by country (in MW) from EndCoal.org (Actual source: Coal Swarm
#' and Global Plant Tracker) July 2018
#' @author Aman Malik


# read in Data
readEndCoal <- function(){
input <- read_excel("Coal Plants By Country July 2018.xlsx")
x <- as.magpie(input,spatial=1,temporal=NULL,data=2)

return(x)
}