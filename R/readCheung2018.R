#' Read climate impact data on  marine fishery production for from Cheung 2018
#' 
#' Read-in a csv file as magclass object
#' in percentage values
#' 
#' @param subtype "General" data subtype. Areas in square km and Primary Production in mg C day^-1 for each Exclusive Economic Zone obtained from Seaaroundus.org
#' "ModelOutputDBEM" data subtype. DBEM Model output for RCP2.6;RCP8.5 obtained from Cheung et al 2018
#' "ModelOutputDynModel" data subtype. Dynamic Model output for RCP2.6;RCP8.5 obtained from Cheung et al 2018
#' @return magpie object of the fishery data with respecitive model outputs 
#' @author Jasmin Wehner 
#' @seealso \code{\link{readSource}}

#' @importFrom magclass as.magpie 
#' @export 
readCheung2018 <- function(subtype) {
  
  Cheungdata2018DBEM <- read.csv("/p/projects/rd3mod/inputdata/sources/Cheung2018/FAO_Impacts_CC_on_fisheries_DBEM3.csv",stringsAsFactors = FALSE)
  Cheungdata2018DynMod <- read.csv("/p/projects/rd3mod/inputdata/sources/Cheung2018/FAO_Impacts_CC_on_fisheries_Dynamic_Model3.csv",stringsAsFactors = FALSE)
  Cheungdatamerged <- merge(Cheungdata2018DBEM, Cheungdata2018DynMod, by="Scenarios",all.x=T)
  #Cheungdatamerged <- merge(Cheungdata2018DBEM, Cheungdata2018DynMod, by=c("Scenarios","Prim.Production.mgCm.2day.1","EEZ.area.in.km2","OtherFAOFA"),all.x=T)
  
  #Major Fishing area according to FAO; EEZ according to Cheung 2018 and Seaaroundus.org; Primary production to Cheung 2018 and Seaaroundus.org
  colnames(Cheungdatamerged)[1:3] <- c("ExclEconZone","PrimProdinmgCday","ExclEconZoneAreainkm2")
  Cheungdatamerged$iso <- toolCountry2isocode(Cheungdatamerged$ExclEconZone, mapping = c(
    "andaman and nicobar islands"="IND",
    "ascenion island"="ASC",
    "iles glorieuses"="FRA",
    "azores (portugal)"="PRT",
    "bonaire"="BES",
    "british virgin island"="VGB",
    "cote d’ivoire"="CIV",
    "canada (east coast and arctic)1"="CAN1", #see correctCheung2018 arctic
    "canada (east coast and arctic)2"="CAN1", #see correctCheung2018 east coast
    "canada (pacific)"="CAN",
    "canary islands"="ESP",
    "chagos archipelago"="GBR", #unclear
    "channel islands"="XIS",
    "clipperton island"="FRA",
    "colombia (caribbean)"="COL",
    "colombia (pacific)"="COL",
    "costa rica (caribbean)"="CRI",
    "costa rica (pacific)"="CRI",
    "crozet islands (french southern and antarctic territories)"="FRA",
    "curaã§ao"="CUW", 
    "democratic people’s republic of korea1"="PRK1", #(sea of japan)
    "democratic people’s republic of korea2"="PRK1",  #(yellow sea) 
    "denmark (north sea)"="DNK",
    "desventuradas islands (chile)"="RCH",
    "easter island (chile)"="RCH",
    "france (atlantic coast)"="FRA",
    "french guyana, galapagos islands (ecuador)"="ECU",
    "germany (north sea)"="GER",
    "guatemala (pacific)"="GTM",
    "hawaii (united states of america)1"="USA1", #see correctCheung2018  main islands
    "hawaii (united states of america)2"="USA1", #see correctCheung2018  northwest islands
    "heard and mcdonald islands"="HMD",
    "honduras (caribbean)"="HND",
    "french guyana"="GUY",
    "galapagos islands (ecuador)"="ECU",
    "honduras (pacific)"="HND",
    "howland island and baker island"="USA",
    "india (mainland)"="IND",
    "indonesia (eastern)"="IDN",
    "islands in the mozambique channel"="FRA",
    "jan mayen island"="NOR",
    "japan1"="JPN1", # (daito islands)
    "japan2"="JPN1",  #(main islands)
    "japan3"="JPN1",  #(ogasawara islands)
    "jarvis island"="USA", 
    "johnston atoll"="USA",
    "kerguelen islands (french southern and antarctic territories)"="FRA",
    "kiribati (gilbert islands)"="KIR",
    "kiribati (line islands)"="KIR",
    "kiribati (phoenix islands)"="KIR",
    "macquarie island (australia)"="AUS",
    "madeira islands"="PRT",
    "malaysia1"="MAL1", #see correctCheung2018  (peninsula east)
    "malaysia2"="MAL1", #see correctCheung2018  (peninsula west)
    "marshall island"="MHL",
    "mexico (atlantic)"="MEX",
    "mexico (pacific)"="MEX",
    "micronesia (federated states of)"="FSM",
    "morocco (south)"="MAR",
    "nicaragua (caribbean)"="NIC",
    "nicaragua (pacific)"="NIC",
    "oman1"="OMN1",  #see correctCheung2018 (musandam)
    "oman2"="OMN1",  #see correctCheung2018 (musandam)
    "palmyra atoll and kingman reef"="USA",
    "panama1"="PAN1", #see correctCheung2018  caribbean
    "panama2"="PAN1", #see correctCheung2018  pacific
    "pitcairn islands"="PCN",
    "prince edward island"="ZAF",
    "réunion"="REU",
    "russian federation1"="RUS1", # see correctCheung2018- barents sea
    "russian federation2"="RUS1", # see correctCheung2018- far east
    "russian federation3"="RUS1", # see correctCheung2018- laptev to chuckchi sea
    "russian federation4"="RUS1", # see correctCheung2018- kara sea
    "sabah (malaysia)"="MAL",
    "saint-martin (french part)"="FRA",
    "saint barthélemy"="BLM",
    "saint luca"="LCA",
    "sarawak (malaysia)"="MAL",
    "sint eustatius and saba"="NLD",
    "sint maarten (partie neederlandaise)"="NLD",
    "south africa1"="ZAF1", #see correctCheung2018  atlantic and cape
    "south africa2"="ZAF1", #see correctCheung2018  indian ocean coast
    "spain (northwest)"="ESP",
    "st paul and amsterdam islands (french southern and antarctic territories)"="FRA",
    "taiwan province of china"="TWN",
    "thailand (andaman sea)"="THA",
    "thailand (gulf of thailand)"="THA",
    "trindade and martim vaz island (brazil)"="BRA",
    "tristan da cunha island"="SHN",
    "tromelin island"="FRA",
    "united states of america (alaska)1"="USA2", #see correctCheung2018  - subarctic
    "united states of america (alaska)2"="USA2", #see correctCheung2018  (alaska, arctic)
    "united states of america (east coast and gulf of mexico)1"="USA3", #see correctCheung2018  - east coast
    "united states of america (east coast and gulf of mexico)2"="USA3", #see correctCheung2018  gulf of mexiko
    "united states of america (west coast)"="USA",
    "united states virgin islands"="USA",
    "venezuela (bolivarian republic of)"="VEN",
    "wake island"="USA",
    "wallis and futuna island"="FRA" 
  ))
  #remove comma or percentage sign in respective columns
  
  Cheungdatamerged$ExclEconZone <- gsub("\\(|\\)|\\,|\\-|\\’" , "" , Cheungdatamerged$ExclEconZone)
  Cheungdatamerged$ExclEconZone <- gsub(" +" , " " , Cheungdatamerged$ExclEconZone)
  
  Cheungdatamerged$ExclEconZoneAreainkm2 <- gsub("," , "" ,Cheungdatamerged$ExclEconZoneAreainkm2)
  Cheungdatamerged$PrimProdinmgCday <- gsub("," , "" ,Cheungdatamerged$PrimProdinmgCday)
  
  Cheungdatamerged$ExclEconZoneAreainkm2 <- as.numeric(Cheungdatamerged$ExclEconZoneAreainkm2)
  Cheungdatamerged$PrimProdinmgCday <- as.numeric(Cheungdatamerged$PrimProdinmgCday)
 
  #Cheungdatamerged$FAOfishingarea <- gsub("," , "" ,Cheungdatamerged$FAOfishingarea)
  
  #as double entries in iso column cause problems when choosing this column as 
  #spatial column, I leave it for now. See correct function for further 
  #treatment of the data (abolishment of the double entries for EEZs)
  
  Cheungdatamerged <- Cheungdatamerged[,-c(5,7,9,11,13:15,17,19,21,23,24)]
  #Cheungdatamerged$isohelp <- (c(NA, Cheungdatamerged$iso) == c(Cheungdatamerged$iso, NA))[1:length(Cheungdatamerged$iso)]
  
  #Cheungdatamerged$isohelp[Cheungdatamerged$isohelp %in% "TRUE"] <- "-1"
  #CheungdatamergedDBEM <- melt(Cheungdatamerged, id.vars = c("ExclEconZone", "PrimProdinmgCday", "ExclEconZoneAreainkm2"), measure.vars =  5:8)
  #CheungdatamergedDBEM$value <- suppressWarnings(as.numeric(CheungdatamergedDBEM$value))
  
  colnames(Cheungdatamerged)[4:7] <- c("DBEMRCP2p62050","DBEMRCP8p52050","DBEMRCP2p62100","DBEMRCP8p52100") 
  colnames(Cheungdatamerged)[9:12] <- c("DynModelRCP2p62050","DynModelRCP8p52050","DynModelRCP2p62100","DynModelRCP8p52100") 
  

  
   if (subtype == "General"){ # Reference Year (e.g. BAU, 2010)
    x_General <- as.magpie(Cheungdatamerged[,c(1:3)], spatial=0, temporal=0) 
  }  else if(subtype == "ModelOutputDBEM"){
    x_ModelOutputDBEM <- as.magpie(Cheungdatamerged[,c(1,4:7)], spatial=0, temporal=0) 
  } else if(subtype == "ModelOutputDynModel"){
    x_ModelOutputDynModel <- as.magpie(Cheungdatamerged[,c(1,9:12)], spatial=0, temporal=0) 
  }}
