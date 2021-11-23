


calcNetForeignAsset <- function() {
  
  # read data
  x <- readSource("IMF")
  
  # filter all data until 2005
  x <- x[,getYears(x,as.integer=TRUE)<=2005,]
  # sum over years
  x <- dimSums(x,dim=2)
  
  # delete dimensions that are same and not in the GAMS-code
  getNames(x) <- NULL
  getYears(x) <- NULL
  
  # convert billion into trilllion
  x <- x / 1000
 
  return(list(x           = x,
              weight      = NULL,
              unit        = "trillion U.S. dollar",
              description = "net foreign asset"))
}
