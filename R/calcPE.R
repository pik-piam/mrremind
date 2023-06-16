calcPE <- function(subtype = "IEA") {
  
  if (subtype=="IEA"){
  
  data <- calcOutput("IO",subtype="input",aggregate=FALSE)
  
  mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_reporting.csv", returnPathOnly = TRUE, where = "mappingfolder")
  target = c("input")
  
  ### calculate data
  map <- read.csv2(mapping, stringsAsFactors = FALSE, na.strings ="" )
  #delete NAs rows
  map = map[c("io",target)] %>% na.omit()
  
  # select data that have names
  map = map[map$io %in% getNames(data),]
  x <- data[,,map$io]
  #aggregate from the IO names to the reporting names.
  x <- luscale::speed_aggregate(x, map, dim = 3, from = "io", to = "input")
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
  if (subtype=="IEA_WEO"){
  
  data <- readSource(type = "IEA_WEO",subtype = "PE")
  data <- collapseNames(data)
  regions <- toolGetMapping(getConfig()[1], where = "mappingfolder",type = "regional")
  #regions <- unique(regions$RegionCode)
  
  # gdp of all countries in 2015
  gdp <- calcOutput("GDPPast",aggregate = F)
  gdp <- gdp[,"y2015",]
  
  # if 2015 gdp of a country is 90% of the GDP of the region to which it belongs
  # include result. If not, display it as NA

  var <- getNames(data)[1]
  data_new <- new.magpie(getRegions(data),years = getYears(data),names = getNames(data),fill=NA)
  for (i in regions$CountryCode){
    if (!is.na(data[i,"y2010",var]) & gdp[i,,]> 0.9*dimSums(gdp[regions[regions$RegionCode==regions[regions$CountryCode==i,]$RegionCode,]$CountryCode,,],dim = 1))
       { data_new[i,,] <- data[i,,]
        countries <- regions[regions$RegionCode==regions[regions$CountryCode==i,]$RegionCode,]$CountryCode
        data_new[setdiff(countries,i),,] <- 0 # countries other than the "main" country
        # get zero value so that aggregation can be done
         }
    }
  
  data <- data_new
  data <- data[,,]*4.1868e-2 # Mtoe to EJ
  #data <- collapseNames(data)
  #vars <- c("Coal","Oil","Gas")
  #data <- data[,,vars,pmatch=T]
  # converting to remind convention
  getNames(data) <- gsub(pattern = "Primary Energy",replacement = "PE",x = getNames(data))
  getNames(data) <- gsub(pattern = "PE\\|Electricity\\|Gas",replacement = "PE|Gas|Electricity",getNames(data))
  getNames(data) <- gsub(pattern = "PE\\|Electricity\\|Coal",replacement = "PE|Coal|Electricity",getNames(data))
  getNames(data) <- gsub(pattern = "PE\\|Electricity\\|Oil",replacement = "PE|Oil|Electricity",getNames(data))
  getNames(data) <- paste0(getNames(data)," (EJ/yr)")
 # data <- collapseNames(data)
  }
    
  return(list(x=data,weight=NULL,unit="EJ",
              description="IEA Primary Energy Data based on IEA WEO 2019"))
  
  }
