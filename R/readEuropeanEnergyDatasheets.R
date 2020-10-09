#' Read European Energy Datasheets
#' 
#' Read European Energy Datasheets .xlsx file as magpie object
#' 
#' @return magpie object of aggregated energy market data by country. Units follow REMIND report conventions and conversion factor is defined in mapping2REMIND.xlsx file.
#' @author Renato Rodrigues and Atreya Shankar
#' @source European Energy Datasheets public database https://ec.europa.eu/energy/en/data-analysis/energy-statistical-pocketbook
#' @examples
#' \dontrun{test <- readSource("EuropeanEnergyDatasheets",convert=FALSE)}
#' @importFrom readxl excel_sheets read_excel
#' @importFrom reshape2 melt
#' @importFrom stats aggregate

readEuropeanEnergyDatasheets <- function(){
  # European datasheets are updated biannually. The last update date was 04.06.2019.
  # Please update the datasheet/mapping again in January 2020
  # load mapping and find mappable parameters
  mapping <- read_excel("eurostat2REMIND.xlsx")
  #mapping <- read.csv("mapping2REMIND.csv",sep=";",na.strings = c("NA",""))
  mapping <- mapping[-which(is.na(mapping$Original)),]
  # creating vector with mapping
  indices_1 <- which(!(is.na(mapping$REMIND)))
  hold <- mapping[,c("REMIND","factor")]
  hold <- hold[which(!is.na(hold$REMIND)),]
  indices_2 <- which(!(is.na(mapping$REMIND_2)))
  hold_2 <- mapping[,c("REMIND_2","factor")]
  hold_2 <- hold_2[which(!is.na(hold_2$REMIND_2)),]
  indices_3 <- which(!(is.na(mapping$REMIND_3)))
  hold_3 <- mapping[,c("REMIND_3","factor")]
  hold_3 <- hold_3[which(!is.na(hold_3$REMIND_3)),]
  # extract data from excel sheet
  sheets <- excel_sheets("energy_statistical_countrydatasheets.xlsx")
  sheets <- sheets[which(nchar(sheets)==2)]
  data <- lapply(sheets, function(region){
    countrySheet <- suppressMessages(read_excel("energy_statistical_countrydatasheets.xlsx",sheet=region))
    countrySheet <- countrySheet[, colSums(is.na(countrySheet)) != nrow(countrySheet)]
    # find correct column to search for names, most likely should be column 3
    nameColumn <- grep("Energy Balance", countrySheet)
    # remove NAs, remove "Energy statistics for", then get exact matches by row numbers
    countrySheet <- countrySheet[-which(is.na(countrySheet[,nameColumn])),]
    countrySheet <- countrySheet[-grep("Energy Statistics for:",countrySheet[,nameColumn]),]
    # replace with remind_1 mapping
    countrySheet_1 <- countrySheet[indices_1,nameColumn:ncol(countrySheet)]
    countrySheet_1 <- cbind(hold,countrySheet_1)
    countrySheet_1[,-c(1,2,3)] <- sapply(countrySheet_1[,-c(1,2,3)], as.numeric) # making sure the data is numeric
    countrySheet_1[,-c(1,2,3)] <- countrySheet_1[,-c(1,2,3)]*countrySheet_1[,c("factor")] #converting unit to REMIND unit
    countrySheet_1 <- countrySheet_1[,-c(2,3)] # removing extra columns
    countrySheet_1 <- cbind(region,countrySheet_1) # adding region column
    countrySheet_1 <- aggregate(. ~ REMIND + region, data=countrySheet_1, FUN=sum) # merge repeated items 
    colnames(countrySheet_1) <- c("variable","region",1990:(1990+ncol(countrySheet_1)-3))
    # replace with remind_2 mapping
    countrySheet_2 <- countrySheet[indices_2,nameColumn:ncol(countrySheet)]
    countrySheet_2 <- cbind(hold_2,countrySheet_2)
    countrySheet_2[,-c(1,2,3)] <- sapply(countrySheet_2[,-c(1,2,3)], as.numeric) # making sure the data is numeric
    countrySheet_2[,-c(1,2,3)] <- countrySheet_2[,-c(1,2,3)]*countrySheet_2[,c("factor")] #converting unit to REMIND unit
    countrySheet_2 <- countrySheet_2[,-c(2,3)] # removing extra columns
    countrySheet_2 <- cbind(region,countrySheet_2) # adding region column
    countrySheet_2 <- aggregate(. ~ REMIND_2 + region, data=countrySheet_2, FUN=sum) # merge repeated items 
    colnames(countrySheet_2) <- c("variable","region",1990:(1990+ncol(countrySheet_2)-3))
    # replace with remind_3 mapping
    countrySheet_3 <- countrySheet[indices_3,nameColumn:ncol(countrySheet)]
    countrySheet_3 <- cbind(hold_3,countrySheet_3)
    countrySheet_3[,-c(1,2,3)] <- sapply(countrySheet_3[,-c(1,2,3)], as.numeric) # making sure the data is numeric
    countrySheet_3[,-c(1,2,3)] <- countrySheet_3[,-c(1,2,3)]*countrySheet_3[,c("factor")] #converting unit to REMIND unit
    countrySheet_3 <- countrySheet_3[,-c(2,3)] # removing extra columns
    countrySheet_3 <- cbind(region,countrySheet_3) # adding region column
    countrySheet_3 <- aggregate(. ~ REMIND_3 + region, data=countrySheet_3, FUN=sum) # merge repeated items 
    colnames(countrySheet_3) <- c("variable","region",1990:(1990+ncol(countrySheet_3)-3))
    # merge both REMIND mappings
    countrySheet <- rbind(countrySheet_1,countrySheet_2,countrySheet_3)
    return(countrySheet)
  })
  # merge into single dataframe
  data <- do.call("rbind",data)
  # long format
  data <- melt(data,id.vars=1:2)
  colnames(data) <- c("variable","region","period","value")
  # dump contents into magpie
  x <- as.magpie(data,spatial=2,datacol=4,temporal=3)
  return(x)
}