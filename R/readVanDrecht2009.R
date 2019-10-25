#' readvanDrecht2009
#' 
#' Reads a dataset containing values for sewage
#' 
#' 
#' @return A MAgPIE object containing sewage quantities and losses
#' @author Benjamin Leon Bodirksy
#' @examples
#' 
#' 
#'   \dontrun{
#'     x <- readSource("VanDerWerf2010")
#'   }
#' 
#' @importFrom reshape2 melt
readVanDrecht2009 <- function(){
  file <- "input.csv"
  #read file
  data <- read.csv(file = file, header = T,stringsAsFactors = F)
  molten <- melt(data, id.vars = c("indicator","year","scenario"))
  molten<-molten[,c(4,2,3,1,5)]

  out<-as.magpie(molten,spatial=1)
  
  dimnames(out)[[1]]<-sub(dimnames(out)[[1]],pattern = "\\.",replacement = "_")
  out[,c("y1970","y1990","y2000"),]<-out[,c("y1970","y1990","y2000"),"hist"]
  out<-out[,,"hist",invert=TRUE]
  return(out)
}
