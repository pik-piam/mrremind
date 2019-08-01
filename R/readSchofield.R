#' Read parameters of Schofield equations
#' 
#' University, United Nations. 2004. Human Energy Requirements: Report of a Joint FAO/WHO/UNU Expert Consultation: Rome, 17-24 October 2001. Vol. 1. Food & Agriculture Org. http://books.google.com/books?hl=en&lr=&id=8WW7LP5h6usC&oi=fnd&pg=PR3&dq=%22working+groups.+The+work+of+these+groups+preceded+the+expert+consultation+and+served+as%22+%22for+discussions+and+exchange+during+the+meeting.+Thanks+are+also+due+to+Dr+E.%22+&ots=8Fizk--Hr6&sig=ocPdelHKyX2_npUTh41zZRRHf68.
#' 
#' @return MAgPIE object
#' @author Benjamin Bodirsky
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("Schofield") }
#' 

readSchofield<- function() {
  file <- "schofield.csv"
  file2<-read.csv(file, sep=",",dec = ".")
  file2<-as.magpie(file2,spatial=0,temporal=0,datacol=3)
  return(file2)
}  
