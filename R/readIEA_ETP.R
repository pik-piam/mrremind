#' @author Falk Benke
#' @importFrom readxl read_excel
#' @importFrom reshape2 melt
#' @param subtype data subtype. Either "buildings", "industry", "scenario" or "transport"

readIEA_ETP <- function(subtype){
  
  yearETP <- c(2014, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060)

  map.regions = data.table(regionETP = c("Brazil", "European Union", "United States", "China", 
                                         "India", "Russia", "Mexico", "South Africa", "ASEAN"), 
                           regionREMIND = c("BRA", "EUR", "USA", "CHN", "IND", "RUS", "MEX", "ZAF", "ASEAN"))

  if(!subtype %in% c("buildings", "industry", "scenario", "transport")){
    stop('Not a valid subtype!')
  }
  
  map.variables <- read.csv(file="IEA_ETP_variables_mapping.csv", stringsAsFactors = FALSE, sep=";") %>%
    subset(subtype == sub)
  
  map.scenarios <- data.table(
    colStart = c("C", "P", "AC"),
    colEnd = c("K", "X", "AK"),
    name= c("RTS", "2DS", "B2DS")
  )

  file.name <- paste0("ETP2017_",subtype,"_summary.xlsx")
  tmp1 <- NULL
  
  for(s in map.scenarios[,name]){
    for(r in map.variables[,"row"]){
      for(region in map.regions[,regionETP]) {
        tmp = data.table(
          variable = map.variables[map.variables$row == r, "variable"], 
          region = map.regions[regionETP == region, regionREMIND],
          unit = map.variables[map.variables$row == r, "unit"],
          model = paste0("IEA ETP", sep=" ", s),
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
  
  projections <- melt(tmp1, id.vars = c("model", "variable", "region", "unit"))
  colnames(projections) <- c('model', 'variable', 'region', 'unit', 'period', 'value')
  
  # convert PJ to EJ
  projections[projections$unit == 'PJ/yr',]$value <- projections[projections$unit == 'PJ/yr',]$value / 1000
  projections[projections$unit == 'PJ/yr',]$unit <- 'EJ/yr'
  
  projections$variable <- paste0(projections$variable, " (", projections$unit ,")", sep=" ")
  projections$unit <- NULL

  x <- as.magpie(projections,spatial=3,datacol=5,temporal=4)
  
  return(x)
}