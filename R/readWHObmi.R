#' @title readWHObmi
#' @description Reads in data on body mass index (BMI) recommendations from WHO
#' http://www.who.int/childgrowth/standards/bmi_for_age/en/
#' http://www.who.int/growthref/who2007_bmi_for_age/en/
#' 
#' @return magpie object 
#' 
#' @seealso
#' \code{\link{readNCDrisc}}
#' 
readWHObmi <- function() {
  file <- "BMI.csv"
  bmi<-read.csv(file, sep=",",dec = ",",skip = 3,stringsAsFactors = FALSE)
  bmi[, c(3:8)] <- sapply(bmi[, c(3:8)], as.numeric)
  bmi<-as.magpie(bmi,spatial=0,temporal=0)    
  return(bmi)
}  


