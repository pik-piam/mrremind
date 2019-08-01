#' @importFrom magclass as.magpie getNames<-

calcConsumption <- function() {
  tmp <- readSource("PWT")[,,c("cgdpe", "csh_c")]
  data <- as.magpie(as.array(tmp[,,"cgdpe"]) * as.array(tmp[,,"csh_c"]))
  getNames(data) <- "Consumption (mill. US$2005)"
  
  return(list(x=data,weight=NULL))
}
