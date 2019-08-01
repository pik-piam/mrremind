#' @title calcGHGPrices
#' @description reads in GHG prices from past runs
#'
#' @return list of magpie object with results on country level, weight on country level, unit and description.
#' @author David Chen, Benjamin Leon Bodirsky, David Klein
#' @param emissions which type of emissions shall be returned. ghg just returns n2o, ch4 and co2, pollutants a longer list including also air pollutants
#' @param datasource REMIND for prices from R2M4 coupled runs, REMMAG for old coupled runs, SSPResults for prices from the SSP scenarios from the IIASA database, SSP_and_REM for a combination of REMIND and SSPResults
#' @seealso
#' \code{\link{readSSPResults}}
#' @examples   
#' 
#' \dontrun{ 
#' calcOutput("GHGPrices")
#' }
#' @importFrom magpiesets findset

##IMPORT FROM mrvalidation doesn't necessarily work


calcGHGPrices <- function(emissions="pollutants",datasource="REMMAG") {
  
  if (datasource == "REMIND") {
    x <- readSource("REMIND", subtype = "intensive")
    out_c <- x[,,"Price|Carbon (US$2005/t CO2)"]*44/12 # US$2005/tCO2 -> US$2005/tC
    getNames(out_c,dim=2) <- "co2_c"
    
    out_n2o_direct <- x[,,"Price|N2O (US$2005/t N2O)"]*44/28 # US$2005/tN2O -> US$2005/tN
    getNames(out_n2o_direct,dim=2) <- "n2o_n_direct"
    
    out_n2o_indirect <- x[,,"Price|N2O (US$2005/t N2O)"]*44/28 # US$2005/tN2O -> US$2005/tN
    getNames(out_n2o_indirect,dim=2) <- "n2o_n_indirect"
    
    out_ch4 <- x[,,"Price|CH4 (US$2005/t CH4)"]
    getNames(out_ch4,dim=2) <- "ch4"
    
    x <- mbind(out_n2o_direct,out_n2o_indirect,out_ch4,out_c)
    
    x <- time_interpolate(x,seq(1995,2150,5),extrapolation_type = "constant")
    
    # Set prices to zero before and in 2020
    x[,getYears(x)<="y2020",]<-0
    
    # find scenario names that have NPi or NDC in their name and set their GHG prices to zero (replicating the coupling script)
    set_to_zero <- getNames(x)[grepl("NPi|NDC",getNames(x))]
    x[,,set_to_zero] <- 0
    
    # swap dimensions (scenario and gas) such that in the output file gas is in lines and scenarios in columns
    getNames(x) <- gsub("^([^\\.]*)\\.(.*$)","\\2.\\1",getNames(x))
    
    description <- "GHG certificate prices for different scenarios based on data from REMIND-MAgPIE coupling"

    
  } else if(datasource=="REMMAG") {
    x   <- readSource("REMMAG","ghgprices")
    if (emissions=="pollutants"){
      pollutants<-findset("pollutants")
      y<-add_columns(x,dim = 3.1,addnm = setdiff(pollutants,getNames(x,dim=1)))
      y[,,]<-0
      y[,,"ch4"]<-x[,,"ch4"]
      y[,,"co2_c"]<-x[,,"co2_c"]
      y[,,c("n2o_n_direct","n2o_n_indirect")]<-collapseNames(x[,,"n2o_n"])
      x<-y[,,pollutants]
    } else if (emissions!="ghg") {stop("unknown emission type")}

    description <- "ghg certificate prices for different scenarios based on data from REMIND-MAgPIE-coupling"

    
  } else if (datasource=="SSPResults") {

    x<-readSource("SSPResults")
    x<- collapseNames(x[,,"Price|Carbon (US$2005/t CO2)"])
    x<- x*44/12
    
    pollutants<-findset("pollutants") 
    
    y<-add_dimension(x,dim = 3.1,nm = "co2_c")
    y<-add_columns(y,dim = 3.1,addnm = setdiff(pollutants,getNames(y,dim=1)))
    
    y[,,]<-0
    y[,,"co2_c"]<-x                                           
    y[,,"ch4"]<-(x*28/3.66)    
    y[,,c("n2o_n_direct","n2o_n_indirect")]<-collapseNames(x)*298*1.57/3.66                                              
    x<-y[,,c(pollutants)]
    
    if(emissions=="ghg"){
      x=x[,,c("co2_c","n2o_n_direct","ch4")]
      getNames(x, dim=3) <- c("co2_c", "n2o_n", "ch4")
    } else if (emissions!="pollutants") {stop("emissions has to be set to ghg or pollutants")}
    x[,c(2005,2010),]<-0
    
    x<-time_interpolate(dataset = x[,2010+(0:9)*10,],interpolated_year = c(1965+(0:8)*5,2015+(0:8)*10),integrate_interpolated_years = TRUE,extrapolation_type = "constant")
    x<-toolHoldConstantBeyondEnd(x)

    description <- "ghg certificate prices for different scenarios based on the multimodel SSP results from the IIASA DB"

  } else if (datasource=="SSP_and_REM") {
    ssp <- calcOutput("GHGPrices",datasource = "SSPResults",aggregate = FALSE)
    rem <- calcOutput("GHGPrices",datasource = "REMIND", aggregate = FALSE)
    
    x <- mbind(ssp[,getYears(rem),],rem)
    
    x <- complete_magpie(x,fill=0)
    
    # sort scenarios alphabetically
    x <- x[,,sort(getNames(x,dim=2))]
    
    description <- "ghg certificate prices for different scenarios based on data from REMIND-MAgPIE-coupling and the multimodel SSP results from the IIASA DB"

  } 
  
  pop <- calcOutput("Population",aggregate=FALSE)
  
  return(list(x=x, weight=pop[,2010,1], 
              unit="US$ 2005 per t N2O-N CH4 and CO2-C", 
              description=description,
              note="As weight for aggregation currently population data from 2010 is used.",
              min=0,
              max=10^7))
  
}