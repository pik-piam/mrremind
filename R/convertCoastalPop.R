#' convertCoastalPop
#' 
#' @description Fills and completes Janbeck 2011 coastal population data for all years based on constant percentage of coastal pop for future years
#' @return pop in millions
#' @author David Chen
#' @seealso \code{\link{readSource}}

convertCoastalPop <- function(){

x <- readSource("CoastalPop", convert=F)
x <- toolCountryFill(x, fill=NA)

x <- x/1000000
total_pop <- calcOutput("Population", aggregate=F)

coastal_percentage <- x[,"y2015",]/total_pop[,"y2015","pop_SSP2"]
coastal_percentage <- collapseNames(coastal_percentage, 2)

##populations don't match, some islands have greater coastal population than actual??
coastal_percentage[which(coastal_percentage>1),] <- 1
coastal_percentage[which(is.na(coastal_percentage)),] <- 0

#coastal pop
x <- total_pop * setYears(coastal_percentage, NULL)

return(x)
}