
readIEA_CCS <- function(){
  
  x <- read.csv("ccs_storage.csv",sep=";")
  x <- as.magpie(x,spatial=1)
  
  return(x)
}
