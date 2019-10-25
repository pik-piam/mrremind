correctLPJmlCarbon<-function(x){
  toberemoved<-function(x){
    if(any(is.na(x))){
      percent <- sum(is.na(x))/length(x)*100
      verbosity <- ifelse(percent>1, 1, 2)
      vcat(verbosity=verbosity,paste(percent,"% of data points with NAs in LPJml_rev21. set to 0."))
      x[is.na(x)]<-0
    }
    if(any(x<0)){
      percent <- sum(x<0)/length(x)*100
      verbosity <- ifelse(percent>1, 1, 2)
      vcat(verbosity=verbosity,paste(percent,"% of data points with negative values in LPJml_rev21. set to 0."))
      x[x<0]<-0
    }
    return(x)
  }
  x <- toberemoved(x)
  x <- toolCell2isoCell(x)
  
  return(x)
}  