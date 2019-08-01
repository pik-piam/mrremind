#' Read SSP
#' 
#' Read-in an SSP data csv.zip file as magclass object
#' 
#' 
#' @param subtype data subtype. Either "all" or "ratioPM"
#' @return magpie object of the SSP data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="SSP",subtype="all")
#' }
#' 
#' @importFrom madrat toolSubtypeSelect
#' 
readSSP<- function(subtype) {
  files <- c(all="SspDb_country_data_2013-06-12.csv.zip",
             ratioPM="OECD-WB PPP-MER2005_conversion_rates.xlsx")
  
  file <- toolSubtypeSelect(subtype,files)
  
  if(subtype=="all") {
    unziped_file <- gsub("[[:alnum:],[:punct:]]*/","",file)
    unziped_file <- gsub(".zip","",unziped_file)
    ssp.raw <- read.csv(unz("SspDb_country_data_2013-06-12.csv.zip",unziped_file),check.names=FALSE, stringsAsFactors=FALSE)
  
    ssp <- ssp.raw
    ssp[,1]<- paste(ssp$MODEL,ssp$SCENARIO,ssp$VARIABLE,ssp$UNIT, sep = '.')
  
    ssp$SCENARIO <- NULL
    ssp$VARIABLE <- NULL
    ssp$UNIT <- NULL
  
    names(ssp)[names(ssp)=="MODEL"]     <- "mod.scen"
    names(ssp)[names(ssp)=="REGION"]    <- "iso3c"
  
    final <- ssp[,colSums(is.na(ssp)) != nrow(ssp)] # remove column with NANs
    
    x <-as.magpie(final,datacol=3)  
  } else if(subtype=="ratioPM") {
    data <- as.data.frame(read_excel(file))
    colnames(data) <- c("Region","value") 
    x <- as.magpie(data)
    getYears(x) <- "y2005"   #automated?
  }
  return(x)
}
