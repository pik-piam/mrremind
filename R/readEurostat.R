#' Read Eurostat historical emissions 
#' 
#' Read-in Eurostat historical emissions csv files as magclass object
#' 
#' @param subtype emissions for original eurostat emissions split, MACCemi for MACC historical emissions, or
#' sectorEmi for sector specific emissions
#' @return magpie object of Eurostat historical emissions (MtCO2) 
#' @author Renato Rodrigues
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="Eurostat",subtype="emissions")
#' }
#'  
#' @importFrom madrat toolCountry2isocode
#' @importFrom reshape2 melt
#' 

readEurostat <- function(subtype="emissions") {
  
  if(subtype %in% c("emissions","MACCemi","sectorEmi")){
    #Reading Eurostat historical emissions 
    type <- c("GHG","CO2","CH4","N2O","HFC","PFC","HFC_PFC_NSP","SF6","NF3")
    data <- NULL
    for (t in type){
      df <- read.csv(paste0("eurostat_",t,".csv"))[,-c(3,7)]
      colnames(df) <- c("period", "region", "emi", "sector", "value")
      df[df==":"]<-NA
      df$value <- gsub(",","",df$value)
      df$value <- as.numeric(df$value)/1000 # convert from Thousand tonnes to Mt
      df$emi <- t
      data <- rbind(data,df)
    }
    # mapping reg
    data$region <- toolCountry2isocode(data$region, mapping = c("germany (until 1990 former territory of the frg)"="DEU"))
    x <- as.magpie(data,spatial=2,temporal=1,datacol=5)
  } else {
    stop("Not a valid subtype!")
  }
     
  return(x)
 }  
