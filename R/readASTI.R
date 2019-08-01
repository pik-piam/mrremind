#' Read ASTI R&D investment spreadsheet
#' 
#' Read R&D investment data from ASTI database published by IFPRI
#' 
#' 
#' @return ASTI R&D data as MAgPIE object
#' @author Xiaoxi Wang
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("ASTI")
#' }
#' @importFrom reshape2 melt acast
#' @importFrom readxl read_excel
#' 
readASTI <- function() {
  file <- "asti_data.xlsx"
  sheet <- "Ag-GDP-05-PPP - Agricultural gr"
  x <- as.data.frame(read_excel(file,sheet=sheet,col_names = TRUE))
  colnames(x)[1] <- "country"
  x[x==-999] <- NA
  # remove x$country=="ASTI"
  x <- x[(1:138),]
  
  # transform to a magpie object 
  country <- x$country
  #data(moinput)
  iso_country <- read.csv2(system.file("extdata","iso_country.csv",package = "moinput"),row.names=NULL)
  iso_country1<-as.vector(iso_country[,"x"])
  names(iso_country1)<-iso_country[,"X"]
  tmp <- as.character(iso_country1[as.vector(country)])
  vcat(2,"Following country names could not be found in the country list and returned as NA: ", 
            paste(as.vector(country)[is.na(tmp)], collapse = ", "))
  x$country <- tmp
  index <- which(!is.na(x$country))
  x <- x[index,]
  molten <- suppressMessages(melt(x,id.var="country"))
  names(molten)[2]<- "year"
  molten$variable <- "RD_investment"
  casten <- acast(molten,country~year~variable)
  dimnames(casten)[[2]]<-paste("y", dimnames(casten)[[2]], sep="")
  x<- as.magpie(casten)
  
  return(x)
}
