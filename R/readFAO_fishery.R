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
    data <- read.csv("fishspeciesFAOSTAT2.csv",stringsAsFactors = FALSE,encoding="UTF-8")
    data[,1] = iconv(data[,1], "UTF-8", "UTF-8",sub="")
    data[,2] = iconv(data[,2], "UTF-8", "UTF-8",sub="")
    data[,3] = iconv(data[,3], "UTF-8", "UTF-8",sub="")
   
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
      "cte d'ivoire"="CIV", 
      "curaao" = "CUW",
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
      "runion" = "REU", 
      "saint-martin"="MAF",
      "saint barthlemy"="BLM",
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
    data_new$value <- as.numeric(data_new$value)
    
    data_new$fish_category <- gsub( "\\." , "" , data_new$fish_category)
  
    data_new$fishing_area <- gsub("\\,|\\-", "", data_new$fishing_area)
    data_new$fishing_area <- gsub(" +"," ", data_new$fishing_area)
    
    #creating magpie object  
    x_capture <- as.magpie(data_new, spatial=1, temporal =4, tidy=TRUE)
    getSets(x_capture)=c("iso","year","fish_category","fishing_area")
    return(x_capture)
  }
  
  
  else if (subtype == "aquaculture"){
    
    data <- read.csv("FAO_fishery_aquaculture.csv",stringsAsFactors = FALSE,encoding="UTF-8")
    data[,1] = iconv(data[,1], "UTF-8", "UTF-8",sub="")
    data[,2] = iconv(data[,2], "UTF-8", "UTF-8",sub="")
    data[,3] = iconv(data[,3], "UTF-8", "UTF-8",sub="")
    data[,4] = iconv(data[,4], "UTF-8", "UTF-8",sub="")
    
    data <- data[,-5] #removed unit column 
    names(data) <- gsub("X", "y", names(data)) #replace all X with y in years
    
    colnames(data)[1:4] <- c("iso","fish_category","fishing_area","environment")
    
    data$iso <- toolCountry2isocode(data$iso,mapping =  c(
      "bolivia (plurinat.state)" = "BOL",
      "bonaire/s.eustatius/saba" = "BES",
      "british indian ocean ter" = "IOT",
      "channel islands" = "XIS", #invented
      "china, hong kong sar" = "HKG",
      "china, macao sar" = "MAC",
      "congo, dem. rep. of the" = "COD",
      "cte d'ivoire"="CIV", 
      "curaao" = "CUW",
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
      "runion" = "REU", 
      "saint-martin"="MAF",
      "saint barthlemy"="BLM",
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
    
    data_new <- melt(data, id.vars = c("iso", "fish_category", "fishing_area", "environment"))
    
    data_new$fish_category <- gsub( "\\." , "" , data_new$fish_category)
    data_new$fishing_area <- gsub("\\,|\\-", "", data_new$fishing_area)
    data_new$fishing_area <- gsub(" +"," ", data_new$fishing_area)
    
    x_aqua <- as.magpie(data_new, spatial=1, temporal =5, tidy=TRUE)
    getSets(x_aqua)=c("iso","year","fish_category","fishing_area","environment")
}
  
  return(x_aqua)
}

