#' @title readBodirsky2018
#' @description Reads in regression parameters estimated using mrregression
#' not yet published
#' @param subtype bmi_share for the estimated regression paramters for bmi shares.
#' @return magpie object 
#' 
#' @seealso
#' \code{\link{readNCDrisc}}
#' 
readBodirsky2018 <- function(subtype="bmi_shr") {
  if(subtype=="bmi_shr"){
    file <- "f15_bmi_shr_regr_paras.csv"
    bmi<-read.magpie(file,file_type = "cs3")
    out<-bmi
  } else if ( subtype =="demand_regression" ){
    file <- "f15_demand_regression_parameters.cs3"
    bmi<-read.magpie(file,file_type = "cs3")
    out<-bmi
  } else if ( subtype =="intake_regression" ){    
    file <- "f15_intake_regression_parameters.cs3"
    bmi<-read.magpie(file,file_type = "cs3")
    out<-bmi
  } else if ( subtype =="bodyheight_regression" ){    
    file <- "f15_bodyheight_regr_paras.csv"
    bmi<-read.magpie(file,file_type = "cs3")
    out<-bmi
  }
  return(out)
}  


