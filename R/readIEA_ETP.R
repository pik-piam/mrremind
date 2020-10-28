#' @author Falk Benke
#' @importFrom readxl read_excel
#' @importFrom reshape2 melt
#' @param subtype data subtype. Either "main" or "industry_subsectors"

readIEA_ETP <- function(subtype){

  if(!subtype %in% c("main", "industry_subsectors")){
    stop('Not a valid subtype!')
  }
  
  colEnd <- NULL 
  colStart <- NULL 
  name <- NULL 
  regionETP <- NULL 
  regionREMIND <- NULL 
  variable <- NULL 
  
  yearETP <- c(2014, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060)
  
  map.scenarios <- data.table(
    colStart = c("C", "P", "AC"),
    colEnd = c("K", "X", "AK"),
    name= c("RTS", "2DS", "B2DS")
  )
  
  if(subtype == "industry_subsectors"){
    map.regions = data.table(regionETP = c("OECD", "Non-OECD"), regionREMIND = c("OECD", "Non-OECD"))
    sections <- c("industry_subsectors")
  } else {
    map.regions = data.table(regionETP = c("Brazil", "European Union", "United States", "China", 
                                           "India", "Russia", "Mexico", "South Africa", "ASEAN"), 
                             regionREMIND = c("BRA", "EUR", "USA", "CHN", "IND", "RUS", "MEX", "ZAF", "ASEAN"))
    sections <- c("buildings", "industry", "scenario", "transport")
  }
  
  tmp1 <- NULL
  
  for(section in sections){
    
    file.name <- paste0("ETP2017_", if(section == "industry_subsectors") "industry" else section, "_summary.xlsx")
    map.variables <- read.csv(file="IEA_ETP_variables_mapping.csv", stringsAsFactors=F, sep=";", strip.white=T) %>%
      subset(nchar(variable) > 0 & sub == section)
    for(s in map.scenarios[,name]){
      for(r in map.variables[,"row"]){
        for(region in map.regions[,regionETP]) {
          tmp = data.table(
            variable = map.variables[map.variables$row == r, "variable"], 
            region = map.regions[regionETP == region, regionREMIND],
            unit = map.variables[map.variables$row == r, "unit"],
            model = paste0("IEA_ETP_",s),
            read_excel(
              path=file.name,
              sheet=region, 
              paste0(map.scenarios[name == s, colStart],r,":",map.scenarios[name == s, colEnd],r), 
              col_names = as.character(yearETP)
            )
          )
          tmp1 = rbind(tmp1, tmp)
        }
      }
    }
  }
  projections <- melt(tmp1, id.vars = c("model", "variable", "region", "unit"))
  colnames(projections) <- c('model', 'variable', 'region', 'unit', 'period', 'value')
  
  # convert PJ/yr to EJ/yr
  projections[projections$unit == 'PJ/yr',]$value <- projections[projections$unit == 'PJ/yr',]$value / 1000
  projections[projections$unit == 'PJ/yr',]$unit <- 'EJ/yr'
  
  projections$variable <- paste0(projections$variable, " (", projections$unit ,")")
  projections$unit <- NULL
  
  projections <- aggregate(projections$value, by=list('model'=projections$model, 'region'=projections$region, 
                                                      'period'=projections$period, 'variable'=projections$variable), sum)
  x <- as.magpie(projections,spatial=2,datacol=5,temporal=3)
  
  return(x)
}