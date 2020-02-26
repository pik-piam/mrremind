#' @title calcTauHistorical
#' @description Calculates historical trends in agricultural land use intensity Tau based on FAO yield trends.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Isabelle Weindl
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("TauHistorical")
#' }
#' @importFrom magclass getYears
#' @importFrom utils tail

calcTauHistorical <- function() {
  
  ###read in data
  tau    <- readSource("Tau","historical")
  tc.annual <- calcOutput("TCguess",aggregate=F)
  
  ###details regarding temporal dimension of data
  past<-findset("past")
  years <- intersect(getYears(tau),past)
  last.data.year <- tail(getYears(tau),1)
  missingyears <- past[-match(years, past, nomatch = FALSE)]
  
  ###Harmonization regarding temporal dimension
  out      <- collapseNames(tau[,,"tau.total"])
  weight   <- collapseNames(tau[,,"xref.total"]) 
  
  out.missing <- weight.missing <- new.magpie(cells_and_regions = getRegions(tau),years = missingyears,names = NULL)
  
  #fill data for first missing time step in data
  fist.timestep.length <- as.numeric(substring(missingyears[1], 2)) - as.numeric(substring(last.data.year, 2))
  out.missing[,1,] <- out[,last.data.year,]*(1+setYears(tc.annual,missingyears[1]))^fist.timestep.length
  weight.missing[,1,] <- setYears(weight[,last.data.year,],missingyears[1])
  
  #fill data for other missing time steps if necessary
  if(length(missingyears)>1){
    for(t in 1:(length(missingyears)-1)){
      tmp.timestep.length <- as.numeric(substring(missingyears[t+1], 2)) - as.numeric(substring(missingyears[t], 2))
      out.missing[,missingyears[t+1],]<- out.missing[,missingyears[t],]*(1+setYears(tc.annual,missingyears[t+1]))^tmp.timestep.length
      weight.missing[,missingyears[t+1],] <- setYears(weight[,last.data.year,],missingyears[t+1])
    }
  }
  
  out      <- mbind(out[,years,],out.missing)
  weight   <- mbind(weight[,years,],weight.missing)
  
  return(list(x=out,
              weight=weight,
              unit="1",
              description="Historical trends in agricultural land use intensity Tau based on FAO yield trends"))
}