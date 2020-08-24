#' Read EU Reference Scenario
#' 
#' Read EU Reference Scenario .xlsx file as magpie object
#' 
#' @return magpie object of EU reference scenario data by country. Units follow REMIND report conventions and conversion factor is defined in EUReferenceScenario2REMIND.xlsx file.
#' @author Renato Rodrigues
#' @source EU Reference Scenario public database http://data.europa.eu/euodp/en/data/dataset/energy-modelling
#' @examples
#' \dontrun{test <- readSource("EU_ReferenceScenario",convert=FALSE)}
#' @importFrom readxl excel_sheets read_excel
#' @importFrom reshape2 melt
#' @importFrom stats aggregate
 
readEU_ReferenceScenario <- function(){
  
  #load mapping
  mapping <- NULL
  mapping$A <- suppressMessages(read_excel("EUReferenceScenario2REMIND.xlsx",sheet="A"))
  mapping$B <- suppressMessages(read_excel("EUReferenceScenario2REMIND.xlsx",sheet="B"))
  #loading data
  sheets <- excel_sheets("AppendixRefSce.xls")
  sheets <- sheets[-c(1,2,3,length(sheets))]
  # looping through regions, filtering and converting values
  data <- lapply(sheets, function(sheet){
    type <- substr(sheet, nchar(sheet), nchar(sheet)) #A or B
    region <- substr(sheet, start = 1, stop = 2)
    countrySheet <- suppressMessages(read_excel("AppendixRefSce.xls",sheet=sheet))
    #cleaning sheet
    countrySheet <- countrySheet[,seq(1,12)] 
    colnames(countrySheet) <- c("name",seq(2000,2050,5)) 
    #replace with remind mapping
    countrySheet$REMIND <- mapping[[type]]$REMIND
    countrySheet[,as.character(seq(2000,2050,5))] <- sapply(countrySheet[,as.character(seq(2000,2050,5))], as.numeric) # making sure the data is numeric
    countrySheet[,as.character(seq(2000,2050,5))] <- countrySheet[,as.character(seq(2000,2050,5))]*mapping[[type]]$factor #converting unit to REMIND unit
    countrySheet <- countrySheet[,-c(1)] # removing extra name column
    countrySheet <- countrySheet[-which(is.na(countrySheet$REMIND)),] # remove NAs
    countrySheet <- cbind(region,countrySheet) # adding region column
    countrySheet <- aggregate(. ~ REMIND + region, data=countrySheet, FUN=sum) # merge repeated items 
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