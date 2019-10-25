#' Read IMF
#' 
#' Read-in data that are based on IMF
#' 
#' 
#' @return magpie object of the data
#' @author Lavinia Baumstark
#' @importFrom readxl read_excel
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="IMF")
#' }
#' 
readIMF<-function(){
  
  # read in data
  a <- read_excel("current_account.xlsx")
 
  a$`Subject Descriptor`            <- NULL
  a$Units                           <- NULL
  a$`Country/Series-specific Notes` <- NULL
  a$Scale                           <- NULL
  a$`Estimates Start After`         <- NULL
 
  # dlete NAs
  a <- a[-which(is.na(a$Country)),]
  
  # rename some countreis to eliminate the "."
  a$Country <- gsub("St\\.","Saint",a$Country)
  
  out <- as.magpie(a,spatial=1,datacol=2)
  
  getNames(out) <- "current account [billion U.S. dollar]"
  
  return(out)
}
