#' @importFrom dplyr %>%

calcPE <- function() {
  
  data <- calcOutput("IO",subtype="input",aggregate=FALSE)
  
  mapping <- toolMappingFile("sectoral","structuremappingIO_reporting.csv")
  target = c("input")
  
  ### calculate data
  map <- read.csv2(mapping, stringsAsFactors = FALSE, na.strings ="" )
  #delete NAs rows
  map = map[c("io",target)] %>% na.omit()
  
  # select data that have names
  map = map[map$io %in% getNames(data),]
  x <- data[,,map$io]
  #aggregate from the IO names to the reporting names.
  x <- speed_aggregate(x, map, dim = 3, from = "io", to = "input")
  # rename entries of data to match the rporting names
  getNames(x) <- paste0(getNames(x)," (EJ/yr)")
  
  # add loss to eletricity
  x[,,"PE|Coal|Electricity (EJ/yr)"]    <- x[,,"PE|Coal|Electricity (EJ/yr)"]    + x[,,"PE|Coal|Electricity|Loss (EJ/yr)"]
  x[,,"PE|Biomass|Electricity (EJ/yr)"] <- x[,,"PE|Biomass|Electricity (EJ/yr)"] + x[,,"PE|Biomass|Electricity|Loss (EJ/yr)"]
  x[,,"PE|Gas|Electricity (EJ/yr)"]     <- x[,,"PE|Gas|Electricity (EJ/yr)"]     + x[,,"PE|Gas|Electricity|Loss (EJ/yr)"]
  x <- x[,,c("PE|Coal|Electricity|Loss (EJ/yr)","PE|Biomass|Electricity|Loss (EJ/yr)","PE|Gas|Electricity|Loss (EJ/yr)"),invert=TRUE] 
  
  # add more variables
  x <- mbind(x,setNames(dimSums(x[,,"PE|",pmatch=TRUE],dim=3),"PE (EJ/yr)"))
  x <- mbind(x,setNames(dimSums(x[,,"PE|Coal",pmatch=TRUE],dim=3),"PE|Coal (EJ/yr)"))
  x <- mbind(x,setNames(dimSums(x[,,"PE|Oil",pmatch=TRUE],dim=3),"PE|Oil (EJ/yr)"))
  x <- mbind(x,setNames(dimSums(x[,,"PE|Gas",pmatch=TRUE],dim=3),"PE|Gas (EJ/yr)"))
  x <- mbind(x,setNames(dimSums(x[,,"PE|Biomass",pmatch=TRUE],dim=3),"PE|Biomass (EJ/yr)"))

  
  
  return(list(x=x,weight=NULL,unit="EJ",
              description="IEA Primary Energy Data based on 2014 version of IEA Energy Balances"))
}
