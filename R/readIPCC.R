#' Read in IPCC emissions
#' 
#' Read in IPCC data: \itemize{ \item Read in IPCC emissions from livestock and
#' manure management. Source: IPCC Guidelines for National Greenhouse Gas
#' Inventories (2006); Chapter 10: Emissions from Livestock and Manure
#' Management.  \item Read in IPCC emissions from Lime and urea application.
#' Source: IPCC Guidelines for National Greenhouse Gas Inventories (2006);
#' Chapter 11: N2O Emissions from managed Soils and Co2 Emissions from Lime and
#' Urea Application.  \item Read in IPCC efficiency factors for burning of
#' residue. Source: IPCC Guidelines for Natinal Greenhouse Gas Inventories
#' (2006); Chapter 02: Generic Methodologies applicable to multiple Land-use
#' Categories. }
#' 
#' 
#' @param subtype data subtype. Either "awmsShr", "awmsEfCh4", "awmsParCh4",
#' "nExcrRate", "awmsconfef3, "fracgasms", "fraclossms",
#' "emissionfactors","rescombusteff", "efnsoil"
#' @return magpie object of the IPCC data
#' @author Nele Steinmetz, Stephen Wirth
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' a <- readSource("IPCC","awmsShr")
#' a <- readSource("IPCC","awmsEfCh4")
#' a <- readSource("IPCC","awmsParCh4")
#' a <- readSource("IPCC","nExcrRate")
#' a <- readSource("IPCC","awmsconfef3", convert=F)
#' a <- readSource("IPCC","fracgasms", convert=F)
#' a <- readSource("IPCC","fraclossms", convert=F)
#' a <- readSource("IPCC","emissionfactors", convert=F)
#' a <- readSource("IPCC","rescombusteff", convert=F)
#' a <- readSource("IPCC","efnsoil", convert=F)
#' }
#' @importFrom reshape2 melt
readIPCC <- function(subtype) {
  
  # read in files
  files <- c(awmsShr="awmsShr.csv",
             awmsEfCh4="awmsEfCh4.csv",
             awmsParCh4="awmsParCh4.csv",
             nExcrRate="nExcrRate.csv",
             awmsconfef3= "ch10_awms_conf_ef3.csv",
             fracgasms="ch10_Frac_GasMS.csv",
             fraclossms="ch10_Frac_LossMS.csv",
             emissionfactors="emission_factors.csv",
             rescombusteff="res_combust_eff.csv",
             efnsoil="ef_n_soil.csv",
             ch10_table10a9="ch10_table10a9.csv")
  
  file <- toolSubtypeSelect(subtype,files)
  if(subtype=="awmsShr"||subtype=="awmsEfCh4"||subtype=="awmsParCh4"||subtype=="nExcrRate"|| subtype=="ch10_table10a9")
    {
  data <- read.csv(file, sep=";", stringsAsFactors=FALSE)
  }
  else if(subtype=="fraclossms")
    {
    data <-  read.csv(file,sep = ",", stringsAsFactors = F, header = T, skip=1)
  }
  else if(subtype=="efnsoil")
  {
    data <-  read.csv(file,sep = ",", stringsAsFactors = F, header = T, skip=3)
  }
  else if (subtype=="emissionfactors")
  {
    data <-  read.csv(file,sep = ",", stringsAsFactors = F, header = F, skip=2)
  }
  else if (subtype=="rescombusteff")
  {
    data <-  read.csv(file,sep = ",", stringsAsFactors = F, header = F)
  }
  else 
  {
    data <-  read.csv(file,sep = ",", stringsAsFactors = F, header = T)
  }
  if(subtype=="awmsShr"){
    data <- read.csv("awmsShr.csv", sep=";", header=TRUE, skip=1)
    data$groups <- paste(data$Livestock, data$Manure.Management.System.Usage, sep=".") # merge first 2 columns
    data$Livestock <- NULL
    data$Manure.Management.System.Usage <- NULL
    data <- data[, c(10,1,2,3,4,5,6,7,8,9)] # reorder columns
    
    regions <- c("groups"="groups", "North America"="NOA", "Western Europe"="WER", "Eastern Europe"="EER", 
                 "Oceania"="OCA", "Latin America"="LAM",
                 "Africa"="AFR", "Middle East"="MDE", "Asia"="ASI", "Indian Subcontinent"="ISC")
  }
  
  if(subtype=="awmsEfCh4"){
    data <- read.csv("awmsEfCh4.csv", sep=";", header=TRUE)
    data$groups <- paste(data$Livestock, data$Temperature, sep=".") # merge first 3 columns
    data$Livestock <- NULL
    data$Temperature.category <- NULL
    data$Temperature <- NULL
    data <- data[, c(10,1:9)] # reorder columns
    
    regions <- c("groups"="groups", "North America"="NOA", "Western Europe"="WER", "Eastern Europe"="EER", 
                 "Oceania"="OCA", "Latin America"="LAM",
                 "Africa"="AFR", "Middle East"="MDE", "Asia"="ASI", "Indian Subcontinent"="ISC")
  }
  
  if(subtype=="awmsParCh4"){
    data <- read.csv("awmsParCh4.csv", sep=";", header=TRUE)
    data$groups <- paste(data$Livestock, data$Characteristics, sep=".") # merge first 2 columns
    data$Livestock <- NULL
    data$Characteristics <- NULL
    data <- data[, c(10,1,2,3,4,5,6,7,8,9)] # reorder columns
    
    regions <- c("groups"="groups", "North America"="NOA", "Western Europe"="WER", "Eastern Europe"="EER", 
                 "Oceania"="OCA", "Latin America"="LAM",
                 "Africa"="AFR", "Middle East"="MDE", "Asia"="ASI", "Indian Subcontinent"="ISC")
  }
  
  if(subtype=="nExcrRate"){
    data <- read.csv("nExcrRate.csv", sep=";", header=TRUE)
    data$groups <- as.character(data$Livestock)
    data$Livestock <- NULL
    data <- data[, c(9,1:8)]
    
    regions <- c("groups"="groups", "North America"="NOA", "Western Europe"="WER", "Eastern Europe"="EER", 
                 "Oceania"="OCA", "Latin America"="LAM",
                 "Africa"="AFR", "Middle East"="MDE", "Asia"="ASI")
  }
  
  if(subtype=="awmsconfef3"){
    n <- data$awms
    value <- data[,2]
    d <- new.magpie(years = "y2005", names = n,  sets = c("region", "years", "data") )
    d[,,] <- value
    return(d)
  }
  if(subtype=="fracgasms"||subtype=="fraclossms"|| subtype=="efnsoil"|| subtype=="ch10_table10a9"){
    molten <- melt(data, id.vars = "dummy")
    #create vector for variable names
    rows <- molten$dummy
    cols <- molten$variable
    n <- paste(rows, cols, sep = ".")
    #get values
    value <- molten$value
    d <- new.magpie(years = "y2005", names = n,  sets = c("region", "years", "data") )
    if(subtype=="efnsoil"){getYears(d) <- "y2005"}
    d[,,] <- value

    return(d)
  }
  
  if(subtype=="emissionfactors"|| subtype=="rescombusteff"){
    d <- new.magpie(years = "y2005", names = data[,1], sets = c("region", "year", "data"))
    
    d[,,] <- data[,2]
    
    return(d)
  }
  
  dimnames(data)[[2]] <- regions
  d <- as.magpie(data)
  getYears(d) <- "y2005"
  return(d)
} 
