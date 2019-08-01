#' @title calcACCMIP
#' @description reads in the ACCMIP atmospheric deposition database. Speeds up caching
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @param glo_incl_oceans if true, a global value will be returned that also includes deposition on oceans and should be equivalent to total emissions.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcAtmosphericDeposition}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ACCMIP")
#' }
#' 

calcACCMIP<-function(glo_incl_oceans=FALSE){
  
  if(glo_incl_oceans=="TRUE"){
    prefix="glo_"
  } else {prefix=""}
  
  ACCMIP<-mbind(
    add_dimension(
      mbind(
        mbind(
          readSource("ACCMIP",subtype=paste0(prefix,"nhx_1850"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"nhx_1980"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"nhx_2000"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"nhx_26_2030"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"nhx_26_2100"),convert = FALSE)
        ),
        mbind(
          readSource("ACCMIP",subtype=paste0(prefix,"noy_1850"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"noy_1980"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"noy_2000"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"noy_26_2030"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"noy_26_2100"),convert = FALSE)
        )
      ),dim = 3.1,add = "scenario",nm="rcp26"),
    add_dimension(
      mbind(
        mbind(
          readSource("ACCMIP",subtype=paste0(prefix,"nhx_1850"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"nhx_1980"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"nhx_2000"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"nhx_45_2030"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"nhx_45_2100"),convert = FALSE)
        ),
        mbind(
          readSource("ACCMIP",subtype=paste0(prefix,"noy_1850"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"noy_1980"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"noy_2000"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"noy_45_2030"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"noy_45_2100"),convert = FALSE)
        )
      ),dim = 3.1,add = "scenario",nm="rcp45"),
    add_dimension(
      mbind(
        mbind(
          readSource("ACCMIP",subtype=paste0(prefix,"nhx_1850"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"nhx_1980"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"nhx_2000"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"nhx_85_2030"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"nhx_85_2100"),convert = FALSE)
        ),
        mbind(
          readSource("ACCMIP",subtype=paste0(prefix,"noy_1850"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"noy_1980"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"noy_2000"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"noy_85_2030"),convert = FALSE),
          readSource("ACCMIP",subtype=paste0(prefix,"noy_85_2100"),convert = FALSE)
        )
      ),dim = 3.1,add = "scenario",nm="rcp85")
  )
  
  return(list(
    x=ACCMIP,
    weight=NULL,
    unit="Mt Nr, NH3N and NO2N",
    isocountries=FALSE,
    min=0,
    max=200,
    description="Atmospheric deposition by cell"))
}