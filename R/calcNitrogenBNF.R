#' @title calcNitrogenBNF
#' @description calculates fixation from freeliving bacteria and from nitrogen-fixing crops and natural vegetation
#' @param cellular cellular disaggreagation or national values
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcNitrogenFixationPast}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("NitrogenBNF")
#' }
#' 

calcNitrogenBNF<-function(cellular=FALSE){

  past<-findset("past")
  land <- calcOutput("LanduseInitialisation",aggregate = FALSE,cellular=TRUE)
  bnf_rate <- calcOutput("NitrogenFixationRateNatural",aggregate = FALSE)
  bnf=land*bnf_rate
  bnf[,,c("crop","urban")]<-0
  
  if(!cellular){
    mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)  
    bnf <- groupAggregate(data = bnf,query = mapping,from="celliso",to="iso",dim=1)
    bnf  <- toolCountryFill(bnf,fill=0)
  } else {
    bnf<-toolCell2isoCell(bnf)
  }
  
  bnf[,,"crop"]<-dimSums(
    calcOutput("NitrogenFixationPast",aggregate = FALSE,cellular=cellular,fixation_types="both",sum_plantparts=TRUE)
    ,dim=c(3))

  return(list(x=bnf,
              weight=NULL,
              unit="Mt Nr",
              description="Natural and anthropogenic nitrogen fixation by vegetation and freeliving bacteria",
              isocountries=!cellular))
}
