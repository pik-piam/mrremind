#' Nuclear data from world-nuclear.org
#' @description  Data on currently operating and under-construction nuclear power plants, reactors planned and proposed, 
#' electricity generation from nuclear  
#' @author Christoph Bertram

readIAEA <- function(){
  
  x_2016 <- read.csv("January2016.csv",sep=";",skip=2,check.names=FALSE)
  x_2018 <- read.csv("August2018.csv",sep=";",skip=2,check.names=FALSE)
  # convert into magclass
  x_2016 <- as.magpie(x_2016,spatial=1,datacol=2)
  getYears(x_2016) <- "2016"
  x_2018 <- as.magpie(x_2018,spatial=1,datacol=2)
  getYears(x_2018) <- "2018"
  
  # put data for both time steps together
  x <- mbind(x_2016,x_2018)
  
  return(x)
}
