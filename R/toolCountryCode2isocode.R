toolCountryCode2isocode <- function(code) {
    code <- as.character(code)
	#data(moinput)
  if (length(code) > 100) { # transformation into factor for better performance. Applied only to large objects.
    code <- as.factor(code)
    countrycode2iso <- read.csv2(system.file("extdata","countrycode2iso.csv",package = "moinput"),row.names=NULL)
    countrycode2iso1<-as.vector(countrycode2iso[,"x"])
    names(countrycode2iso1)<-countrycode2iso[,"X"]
    tmp <- as.character(countrycode2iso1[levels(code)])
    if(any(is.na(tmp))) warning("Following country codes could not be found in the country code list and returned as NA: ", paste(levels(code)[is.na(tmp)],collapse=", "))
    levels(code) <- tmp
    return(as.character(code))
  } else {
    tmp <- as.character(countrycode2iso1[code])
    if(any(is.na(tmp))) warning("Following country codes could not be found in the country code list and returned as NA: ", paste(unique(code[is.na(tmp)]),collapse=", "))
    return(tmp)
  }
}
