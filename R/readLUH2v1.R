readLUH2v1 <- function(subtype) {
  files <- c(states="states_1900_2015.RData",
             transitions_to_cropland="transitions_1950_2014.RData",
             irrigation="ManIrrig_1900_2015.RData")
  file <- toolSubtypeSelect(subtype,files)
  load(file)
  x<-clean_magpie(x)
  return(x)
}  