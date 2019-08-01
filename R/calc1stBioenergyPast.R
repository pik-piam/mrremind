#' @title calc1stBioenergyPast
#' @description 
#' Calculates first generation biofuels production, imports, exports for biogas, bioethanol and biodiesel from IEA database. 
#' The unit is Petajoule. 
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Xiaoxi Wang, Isabelle Weindl
#' @seealso
#' \code{\link{setConfig}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("1stBioenergyPast")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @importFrom madrat toolMappingFile
#' @export

calc1stBioenergyPast <- function(){
  df <- toolCalcIEAfromStructureMappingPEFE(
    readSource("IEA",subtype="EnergyBalances"),
    toolMappingFile("sectoral","structuremappingPE.csv"),
    subtype="magpie")
  
  #Unit conversion from ktoe to PJ
  df <- df[,,c("biogas","ethanol","oils","woodfuel")]*0.041868
  
  return(list(x=df,
              weight=NULL,
              unit="PJ",
              description="1st generation bionegy demand")
  )
}
