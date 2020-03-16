#' @title readBodirsky2018
#' @description Reads in regression parameters estimated using mrregression, and reads in some scenario
#' based on Bodirsky et al not yet published
#' @param subtype bmi_share, demand_regression, intake_regression or bodyheight_regression for the estimated regression paramters of different regressions. scenarios for scenario projections. 
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
  } else if (subtype=="scenarios") {
    ssp1=read.magpie("supplementary_data_SSP1.cs3")
    ssp2=read.magpie("supplementary_data_SSP2.cs3")
    ssp3=read.magpie("supplementary_data_SSP3.cs3")
    ssp4=read.magpie("supplementary_data_SSP4.cs3")
    ssp5=read.magpie("supplementary_data_SSP5.cs3")
    out<-mbind(
      add_dimension(ssp1,dim = 3.1,add = "scenario",nm = "SSP1"),
      add_dimension(ssp2,dim = 3.1,add = "scenario",nm = "SSP2"),
      add_dimension(ssp3,dim = 3.1,add = "scenario",nm = "SSP3"),
      add_dimension(ssp4,dim = 3.1,add = "scenario",nm = "SSP4"),
      add_dimension(ssp5,dim = 3.1,add = "scenario",nm = "SSP5")
    )
  }
  return(out)
}  


