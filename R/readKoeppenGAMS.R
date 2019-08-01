#' @title readKoeppenGAMS
#' @description Reads in the koeppen geiger climate data compatible to GAMS usage.
#' 
#' @return Koeppen geiger climate zone classsification data
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' readSource("readKoeppenGAMS")
#' }
#' @importFrom madrat toolGetMapping

readKoeppenGAMS <-function(){
  
    kg <- read.table("KG_1976-2000_ASCII.txt.gz",header = T,skip = 1,stringsAsFactors = F)

    # readKoeppenGAMS returns data on cells which are not exactly harmonized with magpie cells
    
    # Look for common coordinates as magpie in the data received
    coord <- toolGetMapping("coordinates.rda", where="moinput")
    names(coord) <- c("Lon","Lat")  
    d <- merge(coord,kg,all.x=TRUE,sort=FALSE)
    
    index <- which(is.na(d$Cls))
    for(i in index){
    d[i,"Cls"] <- "Dfc"
    }
    colnames(d) <- c("lon","lat","Cls")
    
    mappingFile <- toolMappingFile("cell","CountryToCellMapping.csv",error.missing=TRUE,readcsv = T)
    
    d <- merge(d,mappingFile,by = c("lon","lat"))
    
    d <- d[,c("Cls","cell")]
    
    # conversion <- data.frame(Cls=sort(unique(t$Cls)),id=1:length(sort(unique(t$Cls))))
    
    # d <- merge(d,conversion,all.x = T)
    
    # The "Cls" column here is of type "character" which will not work when we try to convert this data frame into magpie object
    # The idea will be to first convert this into factors and then into numbers
    d$Cls <- as.factor(d$Cls)
    # Now change to numeric
    d$Cls <- as.numeric(d$Cls)

    m <- as.magpie(d)
    # str(m)
    if(length(which(is.na(m)))!=0){
      m[which(is.na(m))] <- 0
      vcat(paste("NAs observed in data are replaced by 0."),verbosity = 2)
    } else {vcat(paste("No NAs found in data. Yayy!"),verbosity = 2)}
    
    vcat(paste0("\n","In case you have the following error:","\n","Sourcefolder does not contain data for the requested source xxxxx and there is no download script which could provide the missing data. Please check your settings!","\n\n","Then try updating your source folder from the input/source data repository."),verbosity = 2)
    
    return(m)
}

