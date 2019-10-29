#' @importFrom magclass getCells<- getCells
correctLUH2v2<-function(x,subtype){
  
  if(any(is.na(x))){
    vcat(verbosity=1, paste(sum(is.na(x))/length(x)*100,"% of data points with NAs in LUH2. set to 0."))
    x[is.na(x)]<-0
  }
  if(any(x<0)){
    vcat(verbosity=1, paste(sum(x<0)/length(x)*100,"% of data points with negative values in LUH2. set to 0."))
    x[x<0]<-0
  }

  #rename old "AFR.1"-style in new "GLO.1"-style
  getCells(x) <- paste0("GLO",substring(getCells(x),4))
  x<-toolCell2isoCell(x)
  
  if(subtype=="states") {
    if(sum(x["JPN","y2005",c("pastr","range")])<0.01) {
      pasture<-x["JPN","y2000",c("pastr","range")]
      bugged_years=2001:2015
      x["JPN",bugged_years,"secdf"]=x["JPN",bugged_years,"secdf"] - setYears(dimSums(x["JPN","y2000",c("pastr","range")],dim=3),NULL)
      x["JPN",bugged_years,c("pastr","range")]=x["JPN",bugged_years,c("pastr","range")] + setYears(x["JPN","y2000",c("pastr","range")],NULL)
    } else {stop("it seems the Japan bug in LUH2v2 has been removed. Please remove the bugfix in correct LUH2v2 before proceeding!")}
  }
  
  return(x)
}  