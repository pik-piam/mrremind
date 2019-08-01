#' @importFrom utils download.file tail

downloadLUH2v2 <- function(subtype=NULL) {
  
  links <- c("http://gsweb1vh2.umd.edu/LUH2/LUH2_v2h/states.nc",
             "http://gsweb1vh2.umd.edu/LUH2/LUH2_v2h/transitions.nc",
             "http://gsweb1vh2.umd.edu/LUH2/LUH2_v2h/management.nc",
             "http://gsweb1vh2.umd.edu/LUH2/LUH2_v2h/staticData_quarterdeg.nc")
             
  
  ### download files
  fnames <- sapply(links, function(x){tail(strsplit(x, split = "/")[[1]], 1)})
  
  lapply(1:length(links), FUN = function(x){ download.file(links[x], destfile=fnames[x], mode="wb")})

}