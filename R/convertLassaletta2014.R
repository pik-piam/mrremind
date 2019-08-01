#' @title convertLassaletta2014
#' @description converts the dataset of
#' Lassaletta, L., G. Billen, B. Grizzetti, J. Angalde, and J. Garnier. 2014. 
#' 50 Year Trends in Nitrogen Use Efficiency of World Cropping Systems: The Relationship between Yield and Nitrogen Input to Cropland.
#' Environmental Research Letters.
#' into a dataset including all countries. Replacing Soviet Union by Russia and Yugoslavia by Serbia without detailed disaggregation.
#' @param x data object that gets provided by wrapper function readSource
#' @param subtype budget provides the nr cropland budgets, fert_to_cropland the sahre of inorganic fertilizers being applied to croplands
#' @return Magpie object with results on country level.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{readLassaletta2014}},
#' \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' readSource("Lassaletta2014",convert=TRUE)
#' }


convertLassaletta2014<-function(x,subtype){
  if(subtype=="budget"){
    vcat(verbosity = 2,"replacing Soviet Union by Russia and Yugoslavia by Serbia without detailed disaggregation")
    dimnames(x)[[1]][which(dimnames(x)[[1]]=="SUN")]<-"RUS"
    dimnames(x)[[1]][which(dimnames(x)[[1]]=="YUG")]<-"SRB"
    dimnames(x)[[1]][which(dimnames(x)[[1]]=="CSK")]<-"CZE"
    x<-toolCountryFill(x,fill = 0, verbosity = 2)
  } else if (subtype=="fert_to_cropland"){
    x<-x/100
    x2<-toolCountryFill(x,fill = 0, verbosity = 2)
    mapping <- read.csv2(system.file("extdata", "ISOhistorical.csv", 
                                     package = "madrat"), stringsAsFactors = F)
    
    x2[c("SRB","MNE","SVN","HRV","MKD","BIH"),,]<-setCells(x["YUG",,],"GLO")
    x2[mapping$toISO[which(mapping$fromISO =="SUN")],,]<-setCells(x["SUN",,],"GLO")
    x2[mapping$toISO[which(mapping$fromISO =="CSK")],,]<-setCells(x["CSK",,],"GLO")
    
    x<-x2
  }
  
  return(x)
}
