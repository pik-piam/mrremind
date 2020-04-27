calcExportshare <- function(){
  input <- readSource(type = "Eurobserver",subtype = "share")
  
  return(list(x      = input,
              weight = NULL,
              unit        = "",
              description = "share of world exports in solar pv, wind, and hydropower"))
  
}
