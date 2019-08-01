convertKoeppen<-function(x) {
  dimnames(x)[[1]][which(dimnames(x)[[1]]=="ROM")]<-"ROU"
  dimnames(x)[[1]][which(dimnames(x)[[1]]=="ZAR")]<-"COD"
  dimnames(x)[[1]][which(dimnames(x)[[1]]=="MON")]<-"MCO"
  dimnames(x)[[1]][which(dimnames(x)[[1]]=="WSH")]<-"ESH"
  
  x <- toolCountryFill(x, fill = NA, BHR="QAT", HKG="CHN", MUS="MDG", PSE="ISR", SGP="MYS", TLS="IDN")

  return(x)
}