#' @title calcEFch4AWMS
#' @description emission factors for methane from animal waste management, depending on manure managed in confinements. The emission factors were calculated based on FAOSTAT estimates due to lack of all necessary parameters in the IPCC Guidelines
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcEFch4Rice}},
#' \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EFch4AWMS")
#' }
#' 
#' @importFrom magclass getSets
#' @importFrom stats median

calcEFch4AWMS<-function(){
  
  emis<-readSource("FAO","EmisAgManureManag")
  ch4<-emis[,,c("Emissions_(CH4)_(Manure_management)_(Gigagrams)","Manure_(N_content)_(Manure_management)_(Kg)")]

  map <- toolMappingFile(type = "sectoral",name="FAOitems_animals_ch4.csv",readcsv=TRUE, where = "moinput")
  ch4_ag <- toolAggregate(x = ch4,dim = 3.1,rel = map,from = "fao",to="magpie",partrel = TRUE, verbosity = 2)
  ef <-  collapseNames((ch4_ag[,,"Emissions_(CH4)_(Manure_management)_(Gigagrams)"]/1000)
         /(ch4_ag[,,"Manure_(N_content)_(Manure_management)_(Kg)"]/1000000000))
  
  # countries without values get the world average
  for (i in getNames(ef,dim=1)){
    tmp<-ef[,,i]
    tmp[is.nan(tmp)]<-median(ef[,,i],na.rm = T)
    tmp[tmp==0]<-median(ef[,,i],na.rm = T)
    tmp[tmp==Inf]<-median(ef[,,i],na.rm = T)
    ef[,,i]<-tmp
  }
  
  weight=collapseNames(ch4_ag[,,"Manure_(N_content)_(Manure_management)_(Kg)"]/1000000000)
  
  # extend for whole period
  time<-findset("time")
  ef<-time_interpolate(dataset = ef,
                       interpolated_year = time,
                       integrate_interpolated_years = FALSE,
                       extrapolation_type = "constant")
  ef<-clean_magpie(ef)
  weight<-time_interpolate(dataset = weight,
                       interpolated_year = time,
                       integrate_interpolated_years = FALSE,
                       extrapolation_type = "constant")
  weight<-clean_magpie(weight)
  
  
  return(list(x=ef,
              weight=weight,
              unit="t CH4 per t N excretion ",
              description="Methane emissions during animal waste management as a ratio to nitrogen excretion")
  )                   
}

