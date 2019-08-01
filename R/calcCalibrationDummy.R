
calcCalibrationDummy <- function() {
  #data(moinput)
  iso_country <- read.csv2(system.file("extdata","iso_country.csv",package = "moinput"),row.names=NULL)
  iso_country1<-as.vector(iso_country[,"x"])
  names(iso_country1)<-iso_country[,"X"]
  x <- new.magpie(cells_and_regions = iso_country1,years = NULL, names = c("crop","past"), fill = 1)
  
  return(list(x=x,
              weight=x,
              unit="-",
              description="Regional yield calibration file",
              note='All values in the file are set to 1 if a new regional setup is used.'))
}
