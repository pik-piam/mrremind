#' Read GPFADB_2005
#' 
#' Read-in an Global plantation forest database (GPFD) data from 2005 (FAO)
#' 
#' @param subtype data subtype. Available subtypes are Plantations, SNForest, PlantedSNForest, TotPlanted, GroParInit, AgeClassInit, Ownership and EndUse
#' @return magpie object 
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}}
#' @seealso \url{http://foris.fao.org/static/pdf/ipc/GPFADB_2005.zip}
#' @seealso \url{http://www.fao.org/forestry/plantedforests/67507/en/}
#' @examples
#' 
#' \dontrun{ a <- readSource("GPFADB_2005","Plantations")
#' }
#' 
#' @importFrom magclass as.magpie
#' @importFrom readxl read_excel
#' @importFrom stringi stri_trans_general

readGPFADB_2005 <- function(subtype){

  datafiles <- grep("XCEL Tab ",dir())
  filestoread <- dir()[datafiles]
  filestoread <- filestoread[-5]
  x <- list()
  for(i in 1:length(filestoread)){
    data <- read_excel(filestoread[i],col_names = TRUE)
    name <- gsub(pattern = "XCEL Tab ",replacement = "",x = filestoread[i])
    name <- gsub(pattern = ".xls",replacement = "",x = name)
    name <- gsub(pattern = "_ ",replacement = ": ",x = name)
    x[[name]] <- data
  }
 
  gc()          ## Garbage collector tries to close RODBC handle
 
  regmap <- read.csv2(system.file("extdata","country2iso.csv",package = "madrat"),row.names=NULL,encoding="UTF-8")
  colnames(regmap) <- c("COUNTRY","CountryCode")
  regmap$COUNTRY <- stri_trans_general(regmap$COUNTRY,"Latin-ASCII")
  regmap$COUNTRY <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", regmap$COUNTRY, perl=TRUE)
  regmap$COUNTRY <- gsub("Of","of",regmap$COUNTRY)
  regmap$COUNTRY <- gsub(" D "," d ",regmap$COUNTRY)
  regmap$COUNTRY <- gsub(" And "," and ",regmap$COUNTRY)
  regmap$COUNTRY <- gsub(" S "," s ",regmap$COUNTRY)
  regmap$COUNTRY <- gsub("Saint Vincent and The Grenadines","Saint Vincent and the Grenadines",regmap$COUNTRY)


  for (i in 1:length(x)){
    colnames(x[[i]]) <- gsub(" ","",colnames(x[[i]]))
  } 
  
  for(i in 1:length(x)){
    colnames(x[[i]]) <- gsub("Country","COUNTRY",colnames(x[[i]]))
    x[[i]]$COUNTRY <- stri_trans_general(x[[i]]$COUNTRY,"Latin-ASCII")
  }

  if(subtype == "Plantations"){
    Plantations <- x[["1 FRA 2005 Plantations CTY"]]
    Plantations <- Plantations[,c(-1,-2)]
    Plantations$COUNTRY <- as.character(Plantations$COUNTRY)
    Plantations$COUNTRY[Plantations$COUNTRY=="Serbia and Montenegro"] <- "Serbia"
    Plantations$COUNTRY[Plantations$COUNTRY=="The former Yugoslav Republic of Macedonia"] <- "Macedonia, The Former Yugoslav Republic of"
    Plantations$COUNTRY[Plantations$COUNTRY=="Venezuela (Bolivarian Republic of)"] <- "Venezuela, Bolivarian Republic of"
    Plantations$COUNTRY[Plantations$COUNTRY=="Democratic Republic of the Congo"] <- "Congo, The Democratic Republic of The"

    Plantations <- merge(x = Plantations,y = regmap,by = "COUNTRY")
    Plantations$COUNTRY <- Plantations$CountryCode
    Plantations <- Plantations[,1:(ncol(Plantations)-1)]
    
    colnames(Plantations) <- gsub("90","1990",colnames(Plantations))
    colnames(Plantations) <- gsub("2K","2000",colnames(Plantations))
    colnames(Plantations) <- gsub("05","2005",colnames(Plantations))
    
    out <- NULL
    for(i in 2:ncol(Plantations)){
      tempdf <- Plantations[,c(1,i)]
      tempdf$var <- gsub('[[:digit:]]+', '', colnames(tempdf)[2])
      extract_year <- regmatches(colnames(tempdf), gregexpr("[[:digit:]]+", colnames(tempdf)))
      tempdf$Year <- as.numeric(unlist(extract_year))
      colnames(tempdf) <- c("iso","value","var","Year")
      out <- rbind(out,tempdf)
    }
    Plantations <- as.magpie(out[,c(1,3,4,2)],tidy=TRUE,spatial=c("iso"),temporal="Year")
    return(Plantations)
  }
  else if(subtype == "SNForest"){
    SNForest <- x[["2 Semi Natural Forest Area 61 Sampled Countries"]]
    SNForest <- SNForest[,c(-1,-2,-3)]
    SNForest$COUNTRY_FRA2005 <- as.character(SNForest$COUNTRY_FRA2005)
    names(SNForest)[1] <- "COUNTRY"
    SNForest$COUNTRY[SNForest$COUNTRY=="Venezuela (Bolivarian Republic of)"] <- "Venezuela, Bolivarian Republic of"
    SNForest$COUNTRY[SNForest$COUNTRY=="Democratic Republic of the Congo"] <- "Congo, The Democratic Republic of The"
    
    SNForest <- merge(x = SNForest,y = regmap,by = "COUNTRY")
    SNForest$COUNTRY <- SNForest$CountryCode
    SNForest <- SNForest[,1:(ncol(SNForest)-1)]
    
    colnames(SNForest) <- gsub("-","",colnames(SNForest))
    colnames(SNForest) <- gsub("90","1990",colnames(SNForest))
    colnames(SNForest) <- gsub("2K","2000",colnames(SNForest))
    colnames(SNForest) <- gsub("05","2005",colnames(SNForest))
    
    out <- NULL
    for(i in 2:ncol(SNForest)){
      tempdf <- SNForest[,c(1,i)]
      tempdf$var <- gsub('[[:digit:]]+', '', colnames(tempdf)[2])
      extract_year <- regmatches(colnames(tempdf), gregexpr("[[:digit:]]+", colnames(tempdf)))
      tempdf$Year <- as.numeric(unlist(extract_year))
      colnames(tempdf) <- c("iso","value","var","Year")
      out <- rbind(out,tempdf)
    }
    SNForest <- as.magpie(out[,c(1,3,4,2)],tidy=TRUE,spatial=c("iso"),temporal="Year")
    return(SNForest)
  }
  else if(subtype == "PlantedSNForest"){
    PlantedSNForest <- x[["3 Planted semi-natural forests area: pd pt"]]
    PlantedSNForest <- PlantedSNForest[,c(-1,-2)]
    PlantedSNForest$COUNTRY <- as.character(PlantedSNForest$COUNTRY)
    PlantedSNForest$COUNTRY[PlantedSNForest$COUNTRY=="Algerie"] <- "Algeria"

    PlantedSNForest <- merge(x = PlantedSNForest,y = regmap,by = "COUNTRY")
    PlantedSNForest$COUNTRY <- PlantedSNForest$CountryCode
    PlantedSNForest <- PlantedSNForest[,1:(ncol(PlantedSNForest)-1)]
    
    PlantedSNForest <- PlantedSNForest[,-grep("ch|Ch",names(PlantedSNForest))]
    
    colnames(PlantedSNForest) <- gsub("90","1990",colnames(PlantedSNForest))
    colnames(PlantedSNForest) <- gsub("2K","2000",colnames(PlantedSNForest))
    colnames(PlantedSNForest) <- gsub("05","2005",colnames(PlantedSNForest))
    
    out <- NULL
    for(i in 2:ncol(PlantedSNForest)){
      tempdf <- PlantedSNForest[,c(1,i)]
      tempdf$var <- gsub('[[:digit:]]+', '', colnames(tempdf)[2])
      extract_year <- regmatches(colnames(tempdf), gregexpr("[[:digit:]]+", colnames(tempdf)))
      tempdf$Year <- as.numeric(unlist(extract_year))
      colnames(tempdf) <- c("iso","value","var","Year")
      out <- rbind(out,tempdf)
    }
    PlantedSNForest <- as.magpie(out[,c(1,3,4,2)],tidy=TRUE,spatial=c("iso"),temporal="Year")
    return(PlantedSNForest)
  }
  else if(subtype == "TotPlanted"){
    TotPlanted <- x[["4 Total Planted Forest Area: Productive and Protect"]]
    TotPlanted <- TotPlanted[,c(-1,-2)]
    TotPlanted$COUNTRY <- as.character(TotPlanted$COUNTRY)
    TotPlanted$COUNTRY[TotPlanted$COUNTRY=="Venezuela (Bolivarian Republic of)"] <- "Venezuela, Bolivarian Republic of"
    TotPlanted$COUNTRY[TotPlanted$COUNTRY=="Democratic Republic of the Congo"] <- "Congo, The Democratic Republic of The"
    
    TotPlanted <- merge(x = TotPlanted,y = regmap,by = "COUNTRY")
    TotPlanted$COUNTRY <- TotPlanted$CountryCode
    TotPlanted <- TotPlanted[,1:(ncol(TotPlanted)-1)]
    
    TotPlanted <- TotPlanted[,-grep("ch|Ch",names(TotPlanted))]
    
    colnames(TotPlanted) <- gsub("90","1990",colnames(TotPlanted))
    colnames(TotPlanted) <- gsub("2K","2000",colnames(TotPlanted))
    colnames(TotPlanted) <- gsub("05","2005",colnames(TotPlanted))
    
    out <- NULL
    for(i in 2:ncol(TotPlanted)){
      tempdf <- TotPlanted[,c(1,i)]
      tempdf$var <- gsub('[[:digit:]]+', '', colnames(tempdf)[2])
      extract_year <- regmatches(colnames(tempdf), gregexpr("[[:digit:]]+", colnames(tempdf)))
      tempdf$Year <- as.numeric(unlist(extract_year))
      colnames(tempdf) <- c("iso","value","var","Year")
      out <- rbind(out,tempdf)
    }
    TotPlanted <- as.magpie(out[,c(1,3,4,2)],tidy=TRUE,spatial=c("iso"),temporal="Year")
    return(TotPlanted)
  }
  else if(subtype == "GroParInit"){
    GroParInit <- x[["6a PD Tree species by Growth Parameters"]]
    GroParInit <- GroParInit[,c(12,1:11)]
    GroParInit$TypeofPlantedForest <- as.character(GroParInit$TypeofPlantedForest)
    GroParInit$COUNTRY <- as.character(GroParInit$COUNTRY)
    GroParInit$Taxa <- as.character(GroParInit$Taxa)
    
    GroParInit <- merge(x = GroParInit,y = regmap,by = "COUNTRY")
    GroParInit$COUNTRY <- GroParInit$CountryCode
    GroParInit <- GroParInit[,1:(ncol(GroParInit)-1)]
    
    GroParInit$TypeofPlantedForest <- gsub("Productive", "", GroParInit$TypeofPlantedForest)
    
    GroParInit$Taxa <- gsub(" [A-z ]*", '' , GroParInit$Taxa)
    GroParInit$Taxa <- gsub("\\.","",GroParInit$Taxa)
    
    genusmap <- read_excel("genus.xls",sheet = 1)
    colnames(genusmap)[1] <- "Taxa"
    GroParInit <- merge(x = GroParInit,y = genusmap,by = "Taxa",all = TRUE)
    
    GroParInit[which(is.na(GroParInit$type)),"type"] <- "hardwood"
    GroParInit <- GroParInit[complete.cases(GroParInit), ]
    
    GroParInit$type <- as.character(GroParInit$type)
    GroParInit <- GroParInit[,c(2,1,3,13,4:12)]
    
    out <- NULL
    for(i in 5:ncol(GroParInit)){
      tempdf <- GroParInit[,c(1,2,3,4,i)]
      temp_aggr <- NULL
      for (j in unique(tempdf$COUNTRY)) {
        countryset <- subset(tempdf,tempdf$COUNTRY==j)
        aggregatedDF <- aggregate(countryset[,5]~countryset$TypeofPlantedForest+countryset$type,FUN = sum)
        colnames(aggregatedDF) <- c("TypeF","TypeW",colnames(tempdf)[5])
        aggregatedDF$COUNTRY <- j
        aggregatedDF <- aggregatedDF[,c(4,1,2,3)]
        temp_aggr <- rbind(temp_aggr,aggregatedDF)  
      }
      temp_aggr$var <- colnames(temp_aggr)[4]
      # extract_year <- regmatches(colnames(tempdf), gregexpr("[[:digit:]]+", colnames(tempdf)))
      # tempdf$Year <- as.numeric(unlist(extract_year))
      temp_aggr <- temp_aggr[,c(1,2,3,5,4)]
      colnames(temp_aggr) <- c("iso","typeF","typeW","GrowthParameter","value")
      out <- rbind(out,temp_aggr)
    }
    out$Year <- 1995
    GroParInit <- as.magpie(out[,c(1,2,3,4,6,5)])
    return(GroParInit)
  }
  else if(subtype == "AgeClassInit"){
    AgeClassInit <- x[["7 Age class"]]
    AgeClassInit <- AgeClassInit[,c(-3,-4)]
    AgeClassInit$TypeofPlantedForest <- as.character(AgeClassInit$TypeofPlantedForest)
    AgeClassInit$COUNTRY <- as.character(AgeClassInit$COUNTRY)
    AgeClassInit <- merge(x = AgeClassInit,y = regmap,by = "COUNTRY")
    AgeClassInit$COUNTRY <- AgeClassInit$CountryCode
    AgeClassInit <- AgeClassInit[,1:(ncol(AgeClassInit)-1)]
    
    colnames(AgeClassInit) <- gsub("-","to",colnames(AgeClassInit))
    
    AgeClassInit$TypeofPlantedForest <- gsub(' |-', '', AgeClassInit$TypeofPlantedForest)
    
    AgeClassInit$Treespecies <- as.character(AgeClassInit$Treespecies)
    AgeClassInit$Treespecies <- gsub(" [A-z ]*", '' , AgeClassInit$Treespecies)
    AgeClassInit$Treespecies <- gsub("\\.","",AgeClassInit$Treespecies)
    
    genusmap <- read_excel("genus.xls",sheet = 1)
    colnames(genusmap)[1] <- "Treespecies"
    AgeClassInit <- merge(x = AgeClassInit,y = genusmap,by = "Treespecies",all = TRUE)
    
    AgeClassInit[which(is.na(AgeClassInit$type)),"type"] <- "hardwood"
    AgeClassInit <- AgeClassInit[complete.cases(AgeClassInit), ]
    
    AgeClassInit$type <- as.character(AgeClassInit$type)
    AgeClassInit <- AgeClassInit[,c(2,1,3,14,4:13)]
    AgeClassInit <- AgeClassInit[,-2]
    
    indextoremove <- grep("Protective",AgeClassInit$TypeofPlantedForest)
    AgeClassInit <- AgeClassInit[-indextoremove,]
    AgeClassInit$TypeofPlantedForest <- gsub("Productive","",AgeClassInit$TypeofPlantedForest)
    
    out <- NULL
    for(i in 4:ncol(AgeClassInit)){
      tempdf <- AgeClassInit[,c(1,2,3,i)]
      temp_aggr <- NULL
      for (j in unique(tempdf$COUNTRY)) {
        countryset <- subset(tempdf,tempdf$COUNTRY==j)
        aggregatedDF <- aggregate(countryset[,4]~countryset$TypeofPlantedForest+countryset$type,FUN = sum)
        colnames(aggregatedDF) <- c("TypeF","TypeW",colnames(tempdf)[4])
        aggregatedDF$COUNTRY <- j
        aggregatedDF <- aggregatedDF[,c(4,1,2,3)]
        temp_aggr <- rbind(temp_aggr,aggregatedDF)  
      }
      temp_aggr$var <- colnames(temp_aggr)[4]
      # extract_year <- regmatches(colnames(tempdf), gregexpr("[[:digit:]]+", colnames(tempdf)))
      # tempdf$Year <- as.numeric(unlist(extract_year))
      temp_aggr <- temp_aggr[,c(1,2,3,5,4)]
      colnames(temp_aggr) <- c("iso","typeF","typeW","AgeClassInit","value")
      temp_aggr$AgeClassInit <- gsub("AreaAgeClassInit","AC",temp_aggr$AgeClassInit)
      out <- rbind(out,temp_aggr)
    }
    out$Year <- 1995
    AgeClassInit <- as.magpie(out[,c(1,2,3,4,6,5)])
    return(AgeClassInit)
  }
  else if(subtype == "Ownership"){
    Ownership <- x[["8 Ownership"]]
    Ownership <- Ownership[,c(-2,-3)]
    Ownership$COUNTRY[Ownership$COUNTRY=="Algerie"] <- "Algeria"
    
    Ownership <- merge(x = Ownership,y = regmap,by = "COUNTRY")
    Ownership$COUNTRY <- Ownership$CountryCode
    Ownership <- Ownership[,1:(ncol(Ownership)-1)]
    
    indextoremove <- grep("Protective",Ownership$TypeofPlantedForest)
    Ownership <- Ownership[-indextoremove,]
    Ownership$TypeofPlantedForest <- gsub("Productive","",Ownership$TypeofPlantedForest)
    
    names(Ownership)[2] <- "TypeF"
    
    out <- NULL
    for(i in 3:ncol(Ownership)){
      tempdf <- Ownership[,c(1,2,i)]
      tempdf$var <- colnames(tempdf)[3]
      extract_year <- regmatches(colnames(tempdf), gregexpr("[[:digit:]]+", colnames(tempdf)))
      tempdf$Year <- as.numeric(unlist(extract_year))
      tempdf$var <- gsub('[[:digit:]]+', '',tempdf$var)
      tempdf$var <- gsub("area", '',tempdf$var)
      colnames(tempdf) <- c("iso","TypeF","value","var","Year")
      out <- rbind(out,tempdf)
    }
    colnames(out) <- gsub(" ","",colnames(out))
    Ownership <- as.magpie(out[,c(1,2,4,5,3)],tidy=TRUE,spatial=c("iso"),temporal="Year")
    return(Ownership)
  }
  else if(subtype == "EndUse"){
    EU_Industrial <- x[["9a End Use Industrial purpose"]]
    EU_Industrial$EndUseType <- "Industrial"
    EU_NonIndustrial <- x[["9b End Use Non-Industrial purpose"]]
    EU_NonIndustrial$EndUseType <- "NonIndustrial"
    
    EndUse <- rbind(EU_Industrial,EU_NonIndustrial)
    rm(EU_Industrial)
    rm(EU_NonIndustrial)
    
    EndUse <- EndUse[,c(-1,-2)]
    EndUse$COUNTRY[EndUse$COUNTRY=="Algerie"] <- "Algeria"
    
    EndUse <- merge(x = EndUse,y = regmap,by = "COUNTRY")
    EndUse$COUNTRY <- EndUse$CountryCode
    EndUse <- EndUse[,1:(ncol(EndUse)-1)]
    
    colnames(EndUse) <- gsub("90","1990",colnames(EndUse))
    colnames(EndUse) <- gsub("2K","2000",colnames(EndUse))
    colnames(EndUse) <- gsub("05","2005",colnames(EndUse))
    
    EndUse$EndUse <- gsub(" ","",EndUse$EndUse)
    EndUse$EndUse <- gsub("d/F","dorF",EndUse$EndUse)
    EndUse <- EndUse[,c(1,2,6,3,4,5)]
    
    out <- NULL
    for(i in 4:ncol(EndUse)){
      tempdf <- EndUse[,c(1,2,3,i)]
      extract_year <- regmatches(colnames(tempdf), gregexpr("[[:digit:]]+", colnames(tempdf)))
      tempdf$Year <- as.numeric(unlist(extract_year))
      colnames(tempdf) <- c("iso","EndUse","EndUseType","Area","Year")
      out <- rbind(out,tempdf)
    }
    EndUsage <- as.magpie(out[,c(1,2,3,5,4)],tidy=TRUE,spatial=c("iso"),temporal="Year",data="Area")
    return(EndUsage)
  }
  else {stop("Invalid subtype ", subtype)}

  
  # ############ Species ############
  # Species <- x[["5  Species by Area Covered Pd Pt"]]
  # Species$Category <- as.character(Species$Category)
  # 
  # Species$Taxa <- gsub(" [A-z ]*", '' , Species$Taxa)
  # Species$Taxa <- gsub("\\.","",Species$Taxa)
  # 
  # genusmap <- read_excel("C:/Users/mishra/Desktop/genus.xls",sheet = 1)
  # colnames(genusmap)[1] <- "Taxa"
  # Species <- merge(x = Species,y = genusmap,by = "Taxa",all = TRUE)
  # 
  # Species[which(is.na(Species$type)),"type"] <- "hardwood"
  # Species <- Species[complete.cases(Species), ]
  
}
