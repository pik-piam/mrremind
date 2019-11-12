#' @title readWBGEM
#' @description read raw data of world commodity prices from the dataset of World Bank global economic monitor
#' @return magpie object of global price of commodities
#' @author Xiaoxi Wang
#' @seealso \code{\link{readSource}}
#' @examples 
#' 
#' \dontrun{ readSource(type = "WBGEM")}
#' 
#' @importFrom  reshape2 melt acast
#' @importFrom  magclass as.magpie


readWBGEM <- function(){
  file <- "ca1f3e3e-e666-4ce8-94d3-796854021263_Data.csv"
  x <- read.csv(file,header = TRUE)
  
  # select annual data
  years <- paste0("X",seq(1960,2017,by=1),"..",seq(1960,2017,by=1),".")
  y <- cbind(x[,c(1:3)],x[,colnames(x)[colnames(x) %in% years]])
  colnames(y)[4:length(colnames(y))] <- gsub("\\.", "",gsub("X[0-9]...","",colnames(y)[4:length(colnames(y))]))
  
  # convert to magpie object
  y <- y[-which(y$Country.Code==""),]
  out <- as.magpie(acast(suppressMessages(melt(y)),Country.Code~variable~Series),spatail =1, temporal =2)
  dimnames(out)[[1]] <- "GLO"
  return(out)
}




