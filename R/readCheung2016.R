#' Read climate impact data on  marine fishery production for from Cheung 2016 
#' 
#' Read-in a csv file as magclass object
#' in absolut and percentage values
#' 
#' @param subtype "Area" data subtype. Areas in square km for each Large Marine Ecosystem obtained from Seaaroundus.org
#' "PrimProdinmgCday" data subtype. Primary Production in mg C day^-1 for each Large Marine Ecosystem obtained from Seaaroundus.org
#' "Degrees" data subtype. relative change to fishery production in degree1p5,degree2p5,degree3p5 scenarios. obtained from Cheung et al 2016
#' @return magpie object of the fishery data with Area, Primary Productivity and Degrees subtypes 
#' @author Jasmin Wehner 
#' @seealso \code{\link{readSource}}

#' @importFrom magclass as.magpie 
#' @export
readCheung2016 <- function(subtype) {
  
  Cheungdata <- read.csv("C:/Users/wehne/ownCloud/PIK/inputdata/sources/Cheung2016/cheungtable.csv",stringsAsFactors = FALSE)
  #Cheungdata <- read.csv("/p/projects/landuse/users/wehner/PIK/inputdata/sources/Cheung2016/cheungtable.csv",stringsAsFactors = FALSE)
  
  #MFA - Major Fishing area; LME Large Marine Ecosystem; PP Primary production
  colnames(Cheungdata) <- c("MajorFishingArea","LargeMarineEcosys","Area","PrimProdinmgCday","degree1p5","degree2p5","degree3p5")
  
  #remove comma or percentage sign in respective columns
  Cheungdata[3:7]<-data.frame(apply(Cheungdata[3:7], 2, function(x) 
    as.numeric(gsub("\\,|%","",as.character(x)))))
  Cheungdata[,2] <- gsub("\\.","",Cheungdata[,2])
  Cheungdata[,2] <- gsub("\\-"," ",Cheungdata[,2])
  Cheungdata[,2] <- gsub(" +"," ",Cheungdata[,2])
  
  
  if (subtype == "Area"){ # Reference Year (e.g. BAU, 2010)
    x_Area <- as.magpie(Cheungdata[,c(1:2,4)], spatial=0, temporal=0) 
  } else if(subtype == "PrimProdinmgCday"){ 
    x_PrimProdinmgCday <- as.magpie(Cheungdata[,c(1:2,3)], spatial=0, temporal=0) 
  } else if(subtype == "Degrees"){
    x_Degrees <- as.magpie(Cheungdata[,c(1:2,5:7)], spatial=0, temporal=0) 
  }
}
