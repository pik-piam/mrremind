readStrefler <- function(){
  x <- read.csv("f33_data_weathering_graderegi.csv", sep=";", header=TRUE, row.names = 1)
  x <- as.magpie(x,spatial=1)
  getSets(x)[1] <- "region"
  getSets(x)[2] <- "year"
  getSets(x)[3] <- "data1"
  return(x)
}