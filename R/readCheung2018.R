#' Read climate impact data on  marine fishery production for from Cheung 2018
#'
#' Read-in a csv file as magclass object
#' in percentage values
#'
#' @param subtype "General" data subtype. Areas in square km and Primary Production in mg C day^-1 for each Exclusive Economic Zone obtained from Seaaroundus.org
#' "models" data subtype. DBEM Model output for RCP2.6;RCP8.5 and  Dynamic Model output for RCP2.6;RCP8.5 obtained from Cheung et al 2018
#' @return magpie object of the fishery data with respecitive model outputs
#' @author Jasmin Wehner
#' @seealso \code{\link{readSource}}

#' @importFrom magclass as.magpie
#' @export
readCheung2018 <- function(subtype) {

  Cheungdata2018DBEM <- read.csv("FAO_Impacts_CC_on_fisheries_DBEM3.csv",stringsAsFactors = FALSE,encoding="UTF-8")
  Cheungdata2018DynMod <- read.csv("FAO_Impacts_CC_on_fisheries_Dynamic_Model3.csv",stringsAsFactors = FALSE,encoding="UTF-8")
  
  Cheungdatamerged <- merge(Cheungdata2018DBEM, Cheungdata2018DynMod, by="Scenarios",all.x=T)
  
  #Major Fishing area according to FAO; EEZ according to Cheung 2018 and Seaaroundus.org; Primary production to Cheung 2018 and Seaaroundus.org
  metadata= c("ExclEconZone","PrimProdinmgCday","ExclEconZoneAreainkm2")
  colnames(Cheungdatamerged)[1:3] <- metadata
  
  Cheungdatamerged$ExclEconZone = iconv(Cheungdatamerged$ExclEconZone, "UTF-8", "UTF-8",sub="")
  
  Cheungdatamerged$ExclEconZone <- gsub("\\(|\\)|\\,|\\-" , "" , Cheungdatamerged$ExclEconZone)
  Cheungdatamerged$ExclEconZone <- gsub(" +" , " " , Cheungdatamerged$ExclEconZone)

  Cheungdatamerged$ExclEconZoneAreainkm2 <- gsub("," , "" ,Cheungdatamerged$ExclEconZoneAreainkm2)
  Cheungdatamerged$PrimProdinmgCday <- gsub("," , "" ,Cheungdatamerged$PrimProdinmgCday)

  Cheungdatamerged$ExclEconZoneAreainkm2 <- as.numeric(Cheungdatamerged$ExclEconZoneAreainkm2)
  Cheungdatamerged$PrimProdinmgCday <- as.numeric(Cheungdatamerged$PrimProdinmgCday)

   DBEM_scenarios = c("DBEM.model..Mid.Century.RCP2.6",                          
    "DBEM.model..Mid.Century.RCP8.5",                          
    "DBEM.model..End.of.Century.RCP2.6",                       
    "DBEM.model..End.of.Century.RCP8.5"
  )
  
  DynMod_scenarios = c(
    "Dynamic.size.based.food.web.model..Mid.Century.RCP2.6",   
    "Dynamic.size.based.food.web.model..Mid.Century.RCP8.5" ,  
    "Dynamic.size.based.food.web.model..End.of.Century.RCP2.6",
    "Dynamic.size.based.food.web.model..End.of.Century.RCP8.5"
  )
  
  Cheungdatamerged <- Cheungdatamerged[,c(metadata,DBEM_scenarios,DynMod_scenarios)]
  DBEM_scenario_position <- match(DBEM_scenarios, colnames(Cheungdatamerged))
  DynMod_scenario_position <- match(DynMod_scenarios, colnames(Cheungdatamerged))
  
  colnames(Cheungdatamerged)[DBEM_scenario_position] <- c("DBEMRCP2p62050","DBEMRCP8p52050","DBEMRCP2p62100","DBEMRCP8p52100")
  colnames(Cheungdatamerged)[DynMod_scenario_position] <- c("DynModelRCP2p62050","DynModelRCP8p52050","DynModelRCP2p62100","DynModelRCP8p52100")



  if (subtype == "General"){ # Reference Year (e.g. BAU, 2010)
    x <- as.magpie(Cheungdatamerged[,c(1:3)], spatial=0, temporal=0)
  }  else {
    
    x <- as.magpie(Cheungdatamerged[,-c(2,3)], spatial=0, temporal=0)
    
    getNames(x,dim = 2) <- sub(pattern = "RCP",replacement = ".RCP",x = getNames(x,dim = 2))
    tmp <- sub(pattern = "2050",replacement = ".y2050",x = getNames(x,dim = 3))
    tmp <-  sub(pattern = "2100",replacement = ".y2100",x = tmp)
    getNames(x,dim = 3) <- tmp
    x=as.magpie(unwrap(x))
    getSets(x)<-c("region","year","fishing_area","fish_model","rcp")
  }
  return(x)
}
