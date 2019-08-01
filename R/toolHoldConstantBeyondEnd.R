#' @title toolHoldConstantBeyondEnd
#' @description Holds a historical dataset constant for the entire simulation period "time".
#'
#' @param x MAgPIE object to be continued.
#' @return MAgPIE object with completed time dimensionality.
#' @author Benjamin Leon Bodirsky
#' @importFrom magpiesets findset
#' @export

toolHoldConstantBeyondEnd<-function(x){
  years <- findset("time")
  missingyears <- setdiff(years,getYears(x))
  if(length(missingyears)==0) return(x)
  out <- add_columns(x = x,addnm = missingyears,dim = 2.1)
  lastyear <- paste0("y",max(getYears(x,as.integer = TRUE)))
  out[,missingyears,] <- setYears(out[,lastyear,],NULL)
  return(out)
}
