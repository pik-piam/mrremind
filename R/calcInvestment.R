#' @importFrom magclass as.magpie getNames

calcInvestment <- function() {
  tmp <- readSource("PWT")[,,c("cgdpe", "csh_i")]
  data <- as.magpie(as.array(tmp[,,"cgdpe"]) * as.array(tmp[,,"csh_i"]))
  getNames(data) <- "Investment (mill. US$2005)"
  
  return(list(x=data,weight=NULL))
}
