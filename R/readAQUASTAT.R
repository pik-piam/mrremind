#' @title readAQUASTAT
#' @description Read in data based on AQUASTAT database (http://www.fao.org/nr/water/aquastat/data/query/index.html?lang=en)
#'  
#' @param subtype "Conservation_agriculture_area_4454": multicropping factor on cropped (excluding fallow) land, 
#' \itemize{ 
#' \item \code{aquastat}:      4454|Conservation agriculture area (1000 ha) 
#'                             (4454_conservation_agriculture_area_in_1000_ha.csv)
#' \item \code{aquastatShare}: 4455|Commoditiy Balance LivestockConservation agriculture area as % of arable land area (%) 
#'                             (4455_conservation_agriculture_area_as_share_of_arable_land_areas.csv)
#' }
#' 
#' @return magpie objects with results on contury level
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#'  readSource("AQUASTAT", subtype="ConsAgri", convert=TRUE)
#' }
#'
#' @importFrom utils read.csv

readAQUASTAT <- function(subtype="ConsAgri"){
  
  files <- c(ConsAgri      = "4454_conservation_agriculture_area_in_1000_ha.csv",
             ConsAgriShare = "4455_conservation_agriculture_area_as_share_of_arable_land_areas.csv")
  
  file <- toolSubtypeSelect(subtype,files)
  
  aquastat   <- read.csv(file , nrow=600, blank.lines.skip = TRUE, stringsAsFactors = FALSE)
  aquastat$X <- substr(aquastat$X, 1, 3)
  aquastat   <- aquastat[aquastat$X!="",]
  
  colyears <- grep("X[0-9]{4}\\.[0-9]{4}", names(aquastat), value=TRUE)
  years    <- paste("y",c(1958:2017),sep="")
  
  mag <- array(NA, dim=c(length(aquastat$X),length(years),1), dimnames=list(aquastat$X,years,subtype))
  
  for(iso3 in 1:length(aquastat$X)){

    value_cols <- setdiff(which(!is.na(aquastat[iso3,]) & aquastat[iso3,]!="" & !grepl("[a-zA-Z]", aquastat[iso3,])), c(1,2))
    
    for(i in value_cols[value_cols %% 2 !=0]){
      mag[iso3,paste("y", aquastat[iso3,i], sep=""),] <- as.numeric(aquastat[iso3, i+1]) 
    }
  }
  
  if(subtype=="ConsAgri") mag <- mag/1000    # unit transform 1000 ha <=> 1 Mha

  return(as.magpie(mag))
}