readHI <- function() {
      hi <- read.csv("hi.csv") #, header=TRUE)
      hi <- as.magpie(hi, datacol=2)
      getYears(hi) <- "y2000"
      return(hi)
}  
