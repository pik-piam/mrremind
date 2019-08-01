readIIASABioenergy <- function() {
  data <- read.csv("AgMIP_1stgen_bio_Source.csv",sep=";",stringsAsFactors=FALSE)
  colnames(data) <- sub("^X([0-9]{4})","y\\1",colnames(data))
  return(as.magpie(data,spatial=1))
}