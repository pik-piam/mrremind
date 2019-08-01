#' Read European Energy Datasheets
#' 
#' Read European Energy Datasheets .xlsx file as magpie object
#' 
#' @return magpie object of aggregated energy market data by country. Units in Mtoe, unless otherwise specified.
#' @author Atreya Shankar
#' @source European Energy Datasheets public database https://ec.europa.eu/energy/en/data-analysis/country
#' @examples
#' \dontrun{a <- readEuropeanEnergyDatasheets()}
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_excel
#' @importFrom reshape2 melt

readEuropeanEnergyDatasheets <- function(){
  
  sheets <- readxl::excel_sheets("countrydatasheets.xlsx")
  sheets2 <- sheets[!nchar(sheets) > 2]
  str <- sheets2[1]
  str2 <- sheets2[length(sheets2)]
  start <- which(sheets == str)
  end <- which(sheets == str2)
  sheetCollection = list()
  
  for (i in start:end) {
    mySheet <- readxl::read_excel("countrydatasheets.xlsx", sheet=i, col_types = "text")
    mySheet <- mySheet[-c(1:6),-c(1,2)]
    mySheet <- mySheet[!rowSums(is.na(mySheet)) == ncol(mySheet),]
    sheetCollection[[i-(start-1)]] <- mySheet
    sheetCollection[[i-(start-1)]] <- cbind(sheets[i], sheetCollection[[i-(start-1)]])
  }
  names(sheetCollection) <- c(sheets2)
  
  # combining and refining with mapping
  
  mapping <- read.csv2("mappingRead.csv", stringsAsFactors = FALSE, encoding="UTF-8")
  dimOld <- mapping[ncol(mapping)-1]
  
  dimNew <- sheetCollection[[1]][,2]
  dimNew <- dimNew[-which(dimNew == "Mtoe (unless otherwise specified)")]
  
  if(all.equal(as.character(dimNew), lapply(dimOld, as.character)$dimName) == TRUE){
    mapping <- mapping[,-c(1,2,11)]
    mapping <- rbind(NA, mapping)
    
    for(i in 1:length(sheetCollection)){
      sheetCollection[[i]] <- cbind(sheetCollection[[i]], mapping)
      names(sheetCollection[[i]])[1:2] <- c("Country", "dimName")
      sheetCollection[[i]] <- sheetCollection[[i]][c(1,30:34,2,3:29,35:38)]
      
      names(sheetCollection[[i]])[8:34] <- sheetCollection[[i]][1,8:34]
      sheetCollection[[i]] <- sheetCollection[[i]][-1,]
      rownames(sheetCollection[[i]]) <- NULL
      
      # remove symbols
      for(z in 1:nrow(sheetCollection[[i]])){
        tochange <- which(names(sheetCollection[[i]])=="dimName") + which(sapply(sheetCollection[[i]][z,c(8:34)], function(x) is.na(as.numeric(x))) == TRUE)
        
        if(length(tochange) >= 1){
          sheetCollection[[i]][z, tochange] <- as.numeric(gsub("\\D", "", sheetCollection[[i]][z, tochange]))
        }
      }
      
      # convert all to numeric
      sheetCollection[[i]][,8:34] <- sapply(c(8:34), function(x) as.numeric(sheetCollection[[i]][,x]))
      
      # process workflows
      a <- sheetCollection[[i]]$new1[which(sheetCollection[[i]]$new1 != " " & sheetCollection[[i]]$new1 != "")]
      b <- which(sheetCollection[[i]]$new1 != " " & sheetCollection[[i]]$new1 != "")
      
      for(j in 1:length(a)){
        if(a[j] != "Mult 245" & a[j] != "Make NA"){
          if(nrow(sheetCollection[[i]][(strsplit(a[j], ", ")[[1]]),8:34]) == 1){
            sheetCollection[[i]][b[j],8:34] <- sheetCollection[[i]][b[j],8:34] - sheetCollection[[i]][(strsplit(a[j], ", ")[[1]]),8:34]
          } else if(nrow(sheetCollection[[i]][(strsplit(a[j], ", ")[[1]]),8:34]) > 1){
            sheetCollection[[i]][b[j],8:34] <- sheetCollection[[i]][b[j],8:34] - colSums(sheetCollection[[i]][(strsplit(a[j], ", ")[[1]]),8:34], na.rm = TRUE)
          }
          sheetCollection[[i]][b[j],4] <- "Other"
        } else if(a[j] == "Mult 245"){
          sel <- sapply(sheetCollection[[i]][245,8:34], is.na) & sapply(sheetCollection[[i]][b[j],8:34], is.na)
          sel <- !sel
          sheetCollection[[i]][b[j],8:34][,sel] <- sheetCollection[[i]][245,8:34][,sel] * sheetCollection[[i]][b[j],8:34][,sel]
        }
      }
    
      sheetCollection[[i]][115,8:34] <- sheetCollection[[i]][114,8:34] - colSums(sheetCollection[[i]][c(116:119,123:125),8:34], na.rm = TRUE)
      sheetCollection[[i]][115,4] <- "Other"
      
      sheetCollection[[i]][174,8:34] <- sheetCollection[[i]][174,8:34] + sheetCollection[[i]][189,8:34]
      sheetCollection[[i]][189,8:34] <- NA
      
      sheetCollection[[i]][115:125,4] <- sapply(sheetCollection[[i]][115:125,4], function(x) paste(x, " (by Fuel/Product)", sep=""))
      sheetCollection[[i]][127:152,4] <- sapply(sheetCollection[[i]][127:152,4], function(x) paste(x, " (by Sector)", sep=""))
      
      for(j in 1:length(a)){
        if(a[j] == "Make NA"){
          sheetCollection[[i]][b[j],8:34] <- NA
        }
      }
      
      a <- sheetCollection[[i]]$new2[which(sheetCollection[[i]]$new2 != " " & sheetCollection[[i]]$new2 != "")]
      b <- which(sheetCollection[[i]]$new2 != " " & sheetCollection[[i]]$new2 != "")
      
      for(k in 1:length(a)){
        if(a[k] != "Mult 245" & a[k] != "Make NA"){
          if(nrow(sheetCollection[[i]][(strsplit(a[k], ", ")[[1]]),8:34]) == 1){
            sheetCollection[[i]][b[k],8:34] <- sheetCollection[[i]][b[k],8:34] - sheetCollection[[i]][(strsplit(a[k], ", ")[[1]]),8:34]
          } else if(nrow(sheetCollection[[i]][(strsplit(a[k], ", ")[[1]]),8:34]) > 1){
            sheetCollection[[i]][b[k],8:34] <- sheetCollection[[i]][b[k],8:34] - colSums(sheetCollection[[i]][(strsplit(a[k], ", ")[[1]]),8:34], na.rm = TRUE)
          }
          sheetCollection[[i]][b[k],5] <- "Other"
        }
      }
      
      sheetCollection[[i]][50,5] <- "Other"
      
      sheetCollection[[i]][335,8:34] <- sheetCollection[[i]][335,8:34] + sheetCollection[[i]][312,8:34]
      sheetCollection[[i]][312,8:34] <- NA
      
      sheetCollection[[i]][366,8:34] <- sheetCollection[[i]][366,8:34] + sheetCollection[[i]][343,8:34]
      sheetCollection[[i]][343,8:34] <- NA
      
      a <- sheetCollection[[i]]$new3[which(sheetCollection[[i]]$new3 != " " & sheetCollection[[i]]$new3 != "")]
      b <- which(sheetCollection[[i]]$new3 != " " & sheetCollection[[i]]$new3 != "")
      
      for(l in 1:length(a)){
        if(a[l] != "Mult 245" & a[l] != "Make NA"){
          if(nrow(sheetCollection[[i]][(strsplit(a[l], ", ")[[1]]),8:34]) == 1){
            sheetCollection[[i]][b[l],8:34] <- sheetCollection[[i]][b[l],8:34] - sheetCollection[[i]][(strsplit(a[l], ", ")[[1]]),8:34]
          } else if(nrow(sheetCollection[[i]][(strsplit(a[l], ", ")[[1]]),8:34]) > 1){
            sheetCollection[[i]][b[l],8:34] <- sheetCollection[[i]][b[l],8:34] - colSums(sheetCollection[[i]][(strsplit(a[l], ", ")[[1]]),8:34], na.rm = TRUE)
          }
          sheetCollection[[i]][b[l],6] <- "Other"
        }
      }
      
      sheetCollection[[i]][which(sheetCollection[[i]][,which(names(sheetCollection[[i]]) == "Units")] == "%"),8:34] <- sheetCollection[[i]][which(sheetCollection[[i]][,which(names(sheetCollection[[i]]) == "Units")] == "%"),8:34]*(1/100)
      sheetCollection[[i]][,which(names(sheetCollection[[i]]) == "Units")][which(sheetCollection[[i]][,which(names(sheetCollection[[i]]) == "Units")] == "%")] <- "proportion ranging from 0-1 (not %)"
      
      sheetCollection[[i]] <- sheetCollection[[i]][,-c(35:37)]
      sheetCollection[[i]] <- sheetCollection[[i]][,c(1:7,35,8:34)]
      
      # remove NA rows
      if(i==1){
        var <- as.numeric(rownames(sheetCollection[[1]][which(rowSums(is.na(sheetCollection[[1]][,c(9:35)])) == ncol(sheetCollection[[1]][,c(9:35)])),]))
      }
      
      sheetCollection[[i]] <- sheetCollection[[i]][-var,]
      rownames(sheetCollection[[i]]) <- NULL
      
      sheetCollection[[i]] <- melt(sheetCollection[[i]], id.vars=colnames(sheetCollection[[i]])[1:8], variable.name="years", value.name="value")
    }
    
    output <- do.call("rbind",lapply(sheetCollection, function(x) x))
    output <- output[c(9,1:6,8,10)]
    
    # generate magpie and return object
    x <- as.magpie(output,temporal=1,spatial=2,datacol=9)

    getNames(x) <- gsub("incl.", "incl", getNames(x))
    return(x)

  } else stop("dimOld and dimNew do not match, need to look at mapping again for compatibility...")
}