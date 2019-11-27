#' Read GGDC10
#' 
#' Read-in GGDC10 data as magclass object.
#' 
#' @return magpie object of the GGDC10 data
#' @author Marcos Marcolino, Lavinia Baumstark
#' @seealso \code{\link{readSource}} 
#' @examples
#' 
#' \dontrun{ a <- readSource(type="GGDC10")
#' }
#' 

readGGDC10<-function(){
  
  data <- as.data.frame(read_excel("ggdc10.xlsx",sheet="dataset"))
  
  data$Region     <- NULL
  data$Regioncode <- NULL
  data$Type       <- data$Variable
  data$Variable   <- NULL
  data <- as.magpie(data,spatial="Country",temporal="Year",datacol=3)

  return(data)
}
