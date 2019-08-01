#' @title readKuczmarski
#' @description Reads in data on body height recommendations and standard deviations
#' body height:
#' https://www.cdc.gov/nchs/data/series/sr_11/sr11_246.pdf
#' Table 16
#' Kuczmarski RJ, Ogden CL, Guo SS, et al. 2000 CDC growth charts for the
#' United States: Methods and development. National Center for Health Statistics.
#' Vital Health Stat 11(246). 2002
#' 
#' @return magpie object 
#' 
#' @seealso
#' \code{\link{readNCDrisc}}
#' 
readKuczmarski <- function() {
  file <- "BMI.csv"
  bmi<-read.csv(file, sep=",",dec = ",",skip = 3,stringsAsFactors = FALSE)
  bmi[, c(3:9)] <- sapply(bmi[, c(3:9)], as.numeric)
  bmi<-as.magpie(bmi,spatial=0,temporal=0)    
  return(bmi)
}  


