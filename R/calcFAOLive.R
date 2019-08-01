calcFAOLive <- function() {
  data <- readSource("FAO","Live")
  return(list(x=data,weight=NULL))
}
