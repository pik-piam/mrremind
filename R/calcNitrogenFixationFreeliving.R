#' @title calcNitrogenFixationFreeliving
#' @description calculates fixation rates from freeliving bacteria per area
#' @return List of magpie objects with results on global level, empty weight, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcNitrogenFixationPast}}
#' \code{\link{readHerridge}} 
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("NitrogenFixationFreeliving")
#' }
#' 


calcNitrogenFixationFreeliving<-function(){
  out<-setYears(readSource("Herridge",subtype = "freeliving",convert=FALSE),NULL)
  return(list(x=out,
              weight=NULL,
              unit="Mt Nr / Mha",
              description="Nitrogen fixation  freeliving bacteria"))
}
