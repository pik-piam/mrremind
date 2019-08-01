#' Read SRES
#' 
#' Read-in an SRES data csv file as magclass object. Works for both population
#' and GDP
#' 
#' 
#' @param subtype data subtype. "pop_a1","pop_a2","pop_b1","pop_b2" or
#' "gdp_a1","gdp_a2","gdp_b1","gdp_b2"
#' @return magpie object of the SRES data. Units are million people or USD1990
#' market exchange rate.
#' @author Benjamin Leon Bodirsky
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="SRES",subtype="pop_a1")
#' }
#' 
readSRES <- function(subtype) {

  
  ### read in sres population or gdp estimates (source is the CIESIN database)
  
  x<-read.csv(paste0(subtype,".csv"))
  UNcode<-x[,which(colnames(x)==c("UN.Code"))]
  x<-x[,-which(colnames(x)%in%c("MESSAGE.Region","IMAGE2.1.Region","UN.Code","Name","IIASA.11.Regions","ASF.Region","X.UN.11.Regions","SRES.4.Regions","Source","AIM.Region","SRES.Region"))]
  names(x)<-gsub(pattern = "X",replacement = "y",x = names(x))
  x<-as.array(as.matrix(x))
  dimnames(x)[[1]]<-UNcode
  x<-as.magpie(x,spatial=1)
  
  dimnames(x)[[1]]<-as.integer(dimnames(x)[[1]])
  x<-rename_dimnames(x,query="countries_query.csv", from="UN_Code", to="ISO_3166_1_alpha_3")
  dimnames(x)[[1]][which(dimnames(x)[[1]]=="YUG")]<-"SCG"
  getSets(x)<-c("region","year","scenario")
  getNames(x)<-subtype
  sres<-clean_magpie(x)
  
  # data mistake
  if (subtype=="sres_b2_pop") {sres<-sres*1000}

  sres=sres/1000000
  
  return(sres)
}  


