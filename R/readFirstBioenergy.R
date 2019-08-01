# read input data from Hermann's paper about 1st generation bioenergy demand
# H. Lotze-Campen et al./Agricultural Economics 45 (2014) 103-116


readFirstBioenergy <- function() {
  file <- "F:/Dropbox/magpie/moinput/1stGenerationBioenergyDemand.csv"
  df <- read.csv(file,sep=",")
  df$Commodities <- tolower(df$Commodities)
  molten <- melt(df)
  molten$variable <- gsub("X","y",molten$variable)
  #petaJoule
  molten$value <- molten$value*1000
  tmp <-molten[-grep("World",molten$Country),c("Country","Commodities","variable","value")]
  out<- acast(tmp,Country~variable~Commodities)
  
  country <- toolCountry2isocode(dimnames(out)[[1]])
  country[is.na(country)]<- "EUR"
  dimnames(out)[[1]] <- country
  out <- as.magpie(out)
  return(out)
}