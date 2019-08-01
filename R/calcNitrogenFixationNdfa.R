#' @title calcNitrogenFixationNdfa
#' @description calculates the share of N in biomass derived from biological fixation
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcNitrogenFixationPast}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("calcNitrogenFixationNdfa")
#' }
#' 
calcNitrogenFixationNdfa<-function(){
  ndfa<-setYears(readSource("Herridge",subtype = "ndfa"),NULL)
  harvest<-collapseNames(calcOutput("FAOmassbalance",aggregate = FALSE)[,,"nr"][,,"production"])
  harvest<-add_dimension(harvest,dim = 3.1,add = "data1",nm = "organ")
  res<-collapseNames(calcOutput("ResBiomass",aggregate = FALSE)[,,"nr"])
  biomass<-mbind(harvest[,,getNames(res,dim=2)],res)
  weight<-dimSums(biomass,dim=3.1)
  out<-weight<-toolHoldConstantBeyondEnd(weight)
  out[,,]<-ndfa
  weight[,,c("begr","betr")]<-10^-10

  return(list(x=out,
              weight=weight,
              unit="share of N",
              description="Share of N in biomass derived from biological fixation"))
}