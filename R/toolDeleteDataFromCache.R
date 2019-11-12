
toolDeleteDataFromCache <- function(type=NULL,subtype=NULL,convert=NULL, ...){
  path <- getConfig()$cachefolder
  if(!is.null(convert)) {
    #if(!(type%in%getSources())) stop('Type "',type, '" is not a valid source type. Available sources are: "',paste(getSources(),collapse='", "'),'"')
    if(convert==TRUE){
      fname <- paste0("convert",type)
    }
    else if(convert==FALSE){
      fname <- paste0("read",type)
    }
    else if(convert=="onlycorrect"){
      fname <- paste0("correct", type)
    }
    if(is.null(subtype)){
      fname <-  paste0(fname, ".mz")
    }
    else{
      fname <- paste0(fname, subtype, ".mz")
    }
    tmppath <- paste0(getConfig("cachefolder"),"/",fname)
  }
  else{
    tmpargs <- paste(names(list(...)),list(...),sep="_",collapse="-")
    if(tmpargs!="") tmpargs <- paste0("-",tmpargs)
    fname <- paste0("calc",type,tmpargs)
    tmppath <- paste0(getConfig("cachefolder"),"/",fname,".Rda")
  }
  
  
  file.remove(tmppath)
}