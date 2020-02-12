#' Read FAO fishery data 
#' 
#' Read-in a csv file as magclass object
#' in tonnes
#' @param subtype "capture" takes all fishdata into account that has been declared as capture fishery
#' "aquaculture" takes all data into account that has been listed as aquaculture fishery
#' @return magpie object of the fishery data with Capture or Aquaculture
#' @author Benjamin Leon Bodirsky, Jasmin Wehner 
#' @seealso \code{\link{readSource}}
#' 
#' @importFrom madrat toolCountry2isocode 
#' @importFrom reshape2 melt
#' @importFrom magclass as.magpie
#' @export
readFAO_fishery <- function(subtype) {
  
  if (subtype == "capture"){
    data <- read.csv("/p/projects/rd3mod/inputdata/sources/FAO_fishery/fishspeciesFAOSTAT2.csv",stringsAsFactors = FALSE)
    
    #data <- read.csv("/p/projects/landuse/users/wehner/PIK/inputdata/sources/FAO_fishery/fishspeciesFAOSTAT2.csv",stringsAsFactors = FALSE)
   
    data <- data[,-4] #removed unit column  
    
    names(data) <- gsub("X", "y", names(data)) #replace all X with y in years
    data[is.na(data)] <- 0
    colnames(data)[1:3] <- c("iso","fish_category","fishing_area")
    
    data$iso <- toolCountry2isocode(data$iso,mapping =  c(
      "bolivia (plurinat.state)" = "BOL",
      "bonaire/s.eustatius/saba" = "BES",
      "british indian ocean ter" = "IOT",
      "channel islands" = "XIS", #invented
      "china, hong kong sar" = "HKG",
      "china, macao sar" = "MAC",
      "congo, dem. rep. of the" = "COD",
      "eswatini" = "SWZ",
      "falkland is.(malvinas)" = "FLK",
      "french southern terr" = "ATF",
      "iran (islamic rep. of)" = "IRN",
      "korea, dem. people's rep" = "PRK",
      "lao people's dem. rep." = "LAO",
      "micronesia, fed.states of" ="FSM",
      "north macedonia" = "MKD",
      "northern mariana is." = "MNP",
      "Other nei" = "XON",
      "pitcairn islands" = "PCN",
      #"réunion"= "REU",
      "r\u00e9union"= "REU",
      "saint-martin"="MAF",
      #"saint barthélemy"="BLM",
      "saint barth\u00e9lemy"="BLM",
      "saint vincent/grenadines"="VCT",
      "serbia and montenegro"= "SCG", #hist 
      "sint maarten"="SXM",
      "st. pierre and miquelon"="SPM",
      "sudan (former)"= "XSF", #hist
      "taiwan province of china" = "TWN",
      "tanzania, united rep. of" = "TZA",
      "turks and caicos is." = "TCA",
      "un. sov. soc. rep." = "SUN", #hist
      "us virgin islands" = "VIR",
      "venezuela, boliv rep of" = "VEN",
      "wallis and futuna is." = "WLF",
      "yugoslavia sfr" = "YUG" #hist
      
    ))
    
    data_new <- melt(data, id.vars = c("iso", "fish_category", "fishing_area"), measure.vars =  4:71)
    
    #converting "FISH AMOUNT" from character to numeric
    data_new$value <- suppressWarnings(as.numeric(data_new$value))
    
    data_new$fish_category <- gsub( "\\." , "" , data_new$fish_category)
  
    data_new$fishing_area <- gsub("\\,|\\-", "", data_new$fishing_area)
    data_new$fishing_area <- gsub(" +"," ", data_new$fishing_area)
    
    #creating magpie object  
    x_capture <- as.magpie(data_new, spatial=1, temporal =4, tidy=TRUE)
    return(x_capture)
  }
  
  
  else if (subtype == "aquaculture"){
    data <- read.csv("/p/projects/rd3mod/inputdata/sources/FAO_fishery/FAO_fishery_aquaculture.csv",stringsAsFactors = FALSE)
    #data <- read.csv("/p/projects/landuse/users/wehner/PIK/inputdata/sources/FAO_fishery/FAO_fishery_aquaculture.csv",stringsAsFactors = FALSE)
  
    data <- data[,-5] #removed unit column 
    names(data) <- gsub("X", "y", names(data)) #replace all X with y in years
    
    colnames(data)[1:4] <- c("iso","fish_category","fishing_area","environment")
    
    data$iso <- toolCountry2isocode(data$iso, mapping = c(
      "bolivia (plurinat.state)" = "BOL",
      "bonaire/s.eustatius/saba" = "BES",
      "channel islands" = "XIS",
      "china, hong kong sar" = "HKG",
      "congo, dem. rep. of the" = "COD",
      "falkland is.(malvinas)" = "FLK",
      "eswatini" = "SWZ",
      "iran (islamic rep. of)" = "IRN",
      "korea, dem. people's rep" = "PRK",
      "lao people's dem. rep." = "LAO",
      "micronesia, fed.states of" ="FSM",
      "northern mariana is." = "MNP",
      "north macedonia" = "MKD",
      "r\u00e9union"= "REU",
      #"réunion"= "REU",
      "serbia and montenegro"= "SCG",
      "st. pierre and miquelon"="SPM",
      "saint vincent/grenadines"="VCT",
      "sudan (former)"= "XSF",
      "taiwan province of china" = "TWN",
      "tanzania, united rep. of" = "TZA",
      "turks and caicos is." = "TCA",
      "un. sov. soc. rep." = "SUN",
      "us virgin islands" = "VIR",
      "venezuela, boliv rep of" = "VEN",
      "yugoslavia sfr" = "YUG"
    ))
    
    data_new <- melt(data, id.vars = c("iso", "fish_category", "fishing_area", "environment"))
    
    data_new$fish_category <- gsub( "\\." , "" , data_new$fish_category)
    data_new$fishing_area <- gsub("\\,|\\-", "", data_new$fishing_area)
    data_new$fishing_area <- gsub(" +"," ", data_new$fishing_area)
    
    x_aqua <- as.magpie(data_new, spatial=1, temporal =5, tidy=TRUE)
}
  
  return(x_aqua)
}

