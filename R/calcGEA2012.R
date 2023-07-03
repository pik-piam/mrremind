#' @title calcGEA2012
#' @description Extracts oil, gas and coal data from the GEA 2012 into a scenario- and time-dependent grade structure
#' @param subtype oil, coal, gas, or bounds
#' @param datatype extraseed, exportbound, or decoffset for bounds subtype
#' @return MAgPIE object containing regionally aggregated GEA 2012 data
#' @author Stephen Bi
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ a <- calcOutput("GEA2012")
#' }
#' 

calcGEA2012 <- function(subtype,datatype) {
  
  mapping <- toolGetMapping(getConfig("regionmapping"),type="regional", where = "mappingfolder")
  regionNames <- unique(mapping$RegionCode)
  ts1 <- 5
  ts2 <- 10
  ttot <- c(seq(2005,2055,ts1),seq(2060,2100,ts2))
  EJ_2_TWyr <- 1/31.536
  ngrades <- 7
  scens <- c("low","med","high")
  nscen <- length(scens)
  
  if (subtype=='coal') {
    x <- readSource("GEA2012",subtype=subtype)
    #x <- dimOrder(x,c(1,3,2))
    w <- new.magpie(getRegions(x),getYears(x),getNames(x),fill=1)
    w[,,"xi3"] <- NA
    mixed <- TRUE
    unit <- "mixed"
  }else if (subtype %in% c('oil','gas')) {
    x <- readSource("GEA2012",subtype=subtype)
    #x <- dimOrder(x,c(1,3,2))
    w <- new.magpie(getRegions(x),getYears(x),getNames(x),fill=1)
    w[,,"xi3"] <- NA
    mixed <- TRUE
    unit <- "mixed"
  }else if (subtype=="bounds") {
    #This subtype is used to define region-specific bounds found in the GAMS code of the previous implementation of timeDepGrades
    if (datatype=="exportbound") {
      xport2005MEA <- 1.4876897061
      x <- new.magpie(regionNames,ttot,names=paste(rep("peoil",nscen),scens,sep="."),fill=NA)
      x["MEA","y2020","peoil"][,,c("low","med")] <- xport2005MEA*1.2877
      x["MEA","y2025","peoil"][,,c("low","med")] <- xport2005MEA*1.40094
      x["MEA","y2030","peoil"][,,c("low","med")] <- xport2005MEA*1.52414
      x["MEA","y2035","peoil"][,,c("low","med")] <- xport2005MEA*1.65817
      x["MEA","y2020","peoil"][,,"high"] <- xport2005MEA*1.2877*1.25
      x["MEA","y2025","peoil"][,,"high"] <- xport2005MEA*1.40094*1.3
      x <- toolNAreplace(x,replaceby = 0)[[1]]
      unit <- "TWyr"
    }else if (datatype=="decoffset") {
      #SB 031720 This "decline offset" parameter is taken from the initial implementation of 31_fossil/timeDepGrades pre-moinput by NB & JH
      #Comment from *NB, IM* The parameter values are based on World Energy Outlook (2008, 2009) and further assumptions/approximations
      x <- new.magpie(regionNames,NULL,names=paste(rep(c("peoil","pegas","pecoal"),each=ngrades),c(1:ngrades),sep="."),fill=-2e-4)
      x[c("REF","MEA"),,c("peoil","pegas")] <- -2e-5
      unit <- "TWyr"
    }else if (datatype=="extraseed") {
      x <- new.magpie(regionNames,ttot,names=paste(rep(c("peoil","pegas"),each=ngrades),c(1:ngrades),sep="."),fill=0)
      x["USA","y2010","pegas"] [,,"2"] <- 2*EJ_2_TWyr
      unit <- "TWyr"
    }
    x <- toolAggregate(x,mapping,weight=NULL)
    w <- new.magpie(getRegions(x),getYears(x),getNames(x),fill=1)
    mixed <- FALSE
  }else {
    stop("Invalid subtype. Please choose oil, gas, coal, or bounds.")
  }
  
  return(list(x=x,
              weight=w,
              unit=unit,
              description=paste("Cost grades and quantities of",subtype),
              note=c("Data from the IIASA Global Energy Assessment 2012"),
              mixed_aggregation=mixed))
  
}
