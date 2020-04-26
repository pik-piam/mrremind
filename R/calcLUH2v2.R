#' @title calcLUH2v2
#' @description Integrates the LUH2v2 landuse-dataset.
#'
#' @param landuse_types magpie: magpie landuse classes. LUH2v2: original landuse classes.
#' @param irrigation if true, the areas are returned sperated by irrigated and rainfed. Irrigation includes flooded area. possible inconsistencies, to be checked.
#' @param cellular if true, dataset is returned on 0.5 degree resolution
#' @param selectyears defaults to past
#' 
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Florian Humpenoeder
#' @seealso
#' \code{\link{calcLanduseInitialisation}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LUH2v2")
#' }
#' @importFrom magclass getNames

calcLUH2v2<-function(landuse_types="magpie",irrigation=FALSE,cellular=FALSE,selectyears="past"){
  

  selectyears <- sort(findset(selectyears,noset = "original"))
  
  if (cellular){
    x <- readSource("LUH2v2",subtype = "states",convert="onlycorrect")[,selectyears,]
    getSets(x) <- c("iso","cell","t","landuse")   
    if (irrigation){
      y<-readSource("LUH2v2",subtype="irrigation",convert="onlycorrect")[,selectyears,]
    }
    
    
    
  } else {
    x <- readSource("LUH2v2",subtype = "states",convert=TRUE)[,selectyears,]
    getSets(x) <- c("iso","t","landuse")
    if (irrigation){
      y <- readSource("LUH2v2",subtype="irrigation",convert=TRUE)[,selectyears,]
    }
  }


  if (irrigation){
    if(is.null(selectyears)){vcat(verbosity = 3,"too many years may lead to memory problems if irrigation=T")}
    #### this section can be placed into the correct function once it exists
    # deactivated, as otherwhise irrigated area exceeds cropland area
    #vcat(verbosity = 3,"Flooded are added to irrigated area")
    #y[,,"irrig_c3ann"]<-y[,,"irrig_c3ann"]+y[,,"flood"]
    #y<-y[,,c("irrig_c3ann","irrig_c3per" ,"irrig_c4ann","irrig_c4per","irrig_c3nfx")]
    ###
    y<-y[,,c("irrig_c3ann","irrig_c3per" ,"irrig_c4ann","irrig_c4per","irrig_c3nfx")]
    getNames(y)<-substring(getNames(y),7)
    x<-add_dimension(x,dim = 3.2,add = "irrigation",nm = "total")
    x<-add_columns(x,dim = 3.2,addnm=c("irrigated","rainfed"))
    x[,,"irrigated"]=0
    y<-add_dimension(y,dim=3.2,add="irrigation",nm="irrigated")
    x[,,paste(getNames(y,dim=1),"irrigated",sep=".")]<-y
    if (any((collapseNames(x[,,"total"])-collapseNames(x[,,"irrigated"]))<0)){
      vcat(verbosity = 2,"Flooded/irrigated area larger than rainfed area. Irrigation limited to total cropland area.")
      tmp<-collapseNames(x[,,"irrigated"])
      tmp[((collapseNames(x[,,"total"])-collapseNames(x[,,"irrigated"]))<0)]<-collapseNames(x[,,"total"])[((collapseNames(x[,,"total"])-collapseNames(x[,,"irrigated"]))<0)]
      x[,,"irrigated"]<-tmp
    }
    x[,,"rainfed"]=collapseNames(x[,,"total"])-collapseNames(x[,,"irrigated"])
    if (any(x[,,"rainfed"]<0)){
      vcat(verbositiy=1,"Flooded/irrigated area larger than rainfed area despite fix.")
    }
  }
  
  if (landuse_types=="magpie") {
    mapping<-toolMappingFile(type = "sectoral",name = "LUH2v2.csv",readcsv = TRUE)
    #x<-groupAggregate(x,query=mapping,dim = 3,from="luh2v2",to="land")
    x<-toolAggregate(x,mapping,dim = 3.1,from="luh2v2",to="land")
  } else if (landuse_types=="LUH2v2") {
    x<-x
  } else {
    vcat(verbositiy=1,"non-existant landuse_types")
  }
  
  return(list(
    x=x,
    weight=NULL,
    unit="Million ha",
    description="Million hectare land area for different land use types.",
    isocountries=!cellular))
}
