#' Read European Environment Agency (EEA) data
#' 
#' Read-in European Environment Agency (EEA) data on ETS emissions as magclass object
#' 
#' 
#' @param subtype data subtype. Either "ETS", "ES", "total", "sectoral", or "projections"
#' @return magpie object of European Environment Agency (EEA) ETS emissions (GtCO2) 
#' @author Renato Rodrigues, Falk Benke
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="EEA_EuropeanEnvironmentAgency",subtype="ETS")
#' }
#'  
#' @importFrom dplyr left_join
#' @importFrom magclass as.magpie
#' @importFrom quitte calc_addVariable
#' @importFrom readxl read_excel
#' @importFrom reshape2 melt
#' @importFrom madrat toolCountry2isocode

readEEA_EuropeanEnvironmentAgency <- function(subtype) {
   
  if (subtype == "ETS"){
    data <- read.csv("ETS_Database_v38.csv", sep="\t")
    data <- data[,-c(2,5)]
    data$year <- as.numeric(data$year)
    data <- data[(!(is.na(data$year))),]
    colnames(data) <- c("region","ETS_info","sector","value","period")
    data$region <- toolCountry2isocode(data$region, warn=F)
    data <- data[(!(is.na(data$region))),]
    data$ETS_info <- gsub(pattern="\\.", replacement="_", data$ETS_info)
    data$sector <- gsub(pattern="\\.", replacement="", data$sector)
    data$value <- as.numeric(gsub(" ","",data$value))/1000000
    data <- data[,c(1,5,2,3,4)]
    x <- as.magpie(data,spatial=1,temporal=2,datacol=5)
  }
  else if (subtype == "ES") {
    data <- read_excel(path='ESD-GHG_2019_revised.xlsx', trim_ws=T, skip=11)
    data <- data[,c(1,17:30)]
    data <- melt(data, id.vars=1)
    colnames(data) <- c("region","period","value")
    data$region <- toolCountry2isocode(data$region)
    data <- data[(!(is.na(data$region))),]
    data$variable <- "Emi|GHG|ES (Mt CO2-equiv/yr)"
    data <- data[,c(1,2,4,3)]
    x <- as.magpie(data,spatial=1,temporal=2,datacol=4)
  } 
  else if (subtype == "total") {
    data <- read_excel(path='GHG_Total_historical.xlsx', trim_ws=T)
    data$...1 <- NULL
    eur <- c('AUT','BEL','BGR','HRV','CYP','CZE','DNK','EST','FIN','FRA','DEU','GRC','HUN', 'ISL', 'IRL',
                'ITA','LVA','LIE','LTU','LUX','MLT','NLD','NOR','POL','PRT','ROU','SVK','SVN','ESP','SWE',
                'CHE','TUR','GBR')
    colnames(data) <- c("year", eur)
    data <- melt(data, id.vars=1)
    colnames(data) <- c("year", "region", "value")
    data <- cbind(data,variable=c('Emi|GHGtot'))
    data$variable <- paste0(data$variable, sep=" ", "(Mt CO2-equiv/yr)")
    data <- data[,c(1,2,4,3)]
    x <- as.magpie(data,spatial=2,temporal=1,datacol=4)
  }
  else if (subtype == "sectoral") {

    sheets <- excel_sheets('GHG_ETS_ES_Projections_by_sector.xlsx')
    historical <- NULL
    timeframe <- seq(2005,2017) # excluding WEM projections

    for (s in sheets){
      tmp <- read_excel(path='GHG_ETS_ES_Projections_by_sector.xlsx', sheet=s, skip=1, trim_ws=T)
      tmp <- melt(tmp, id.vars=1)
      colnames(tmp) <- c("label", "period", "value")
      tmp <- cbind(tmp[!is.na(tmp$value) & tmp$period %in% timeframe,], region=s)
      historical <- rbind(historical, tmp)
    }

    mapping.variable <- as.data.frame(
      cbind(
        variable=c(
          "Emi|GHG|Industry|ETS",
          "Emissions|CO2|Energy|Demand|Transportation",
          "Emissions|CO2|Energy|Demand|Residential and Commercial",
          "Emi|GHG|Industry|ESD"
        ), 
        label=c(
          "Other stationary installations",
          "Transport",
          "Buildings",
          "Industry and other"
        )
      )
    )    
    
    historical <-  left_join(mapping.variable, historical, by=c('label'))
    historical$variable <- paste0(historical$variable, sep=" ", "(Mt CO2-equiv/yr)")
    historical$value <- as.double(historical$value)
    historical$label <- NULL
    historical <- historical[,c(1,4,2,3)]
    x <- as.magpie(historical,spatial=2,datacol=4,temporal=3)

  } else if (subtype == "projections"){
    
    projections <- read.csv(file='GHG_projections_2019_EEA.csv', stringsAsFactors = FALSE, strip.white=TRUE)
    projections <- projections[projections$CountryCode != '', ]
    projections <- projections[projections$CountryCode != 'EU', ]
    projections <- projections[projections$Gapfilled != as.double(0), ]
    projections <- projections[c(1,2,5,6,7,10)]
    projections <- na.omit(projections, cols=projections$Gapfilled)
    
    mapping.variable <- as.data.frame(
      cbind(
        Variable=c(
          'Emi|GHGtot',
          'Emi|GHG|ES',
          'Emi|GHG|ETS',
          'Emi|GHG|Supply|ETS',
          'Emi|GHG|Supply|ESD',
          'Emi|GHG|Supply|FugitiveEmifromFuels|ETS',
          'Emi|GHG|Demand|Industry|Energy|ETS',
          'Emi|GHG|Industrial Processes|ETS',
          'Emi|GHG|Demand|Industry|Energy|ESD',
          'Emi|GHG|Demand|Transport|Energy|ESD',
          'Emi|GHG|Industrial Processes|ESD',
          'Emi|GHG|Demand|Industry|Energy',
          'Emi|GHG|Industrial Processes',
          'Emi|GHG|Agriculture|ESD',
          'Emi|GHG|Supply|FugitiveEmifromFuels|ESD',
          'Emi|GHG|Demand|Other Sectors(Residential+X)|ESD',
          'Emi|GHG|Waste|ESD',
          'Emi|GHG|Intl aviation in ETS|ETS',
          'Emi|GHG|Bunkers|Aviation',
          'Emi|GHG|Bunkers|Navigation',
          'Emi|GHGtot|w/o LULUCF'
        ),
        Category_name=c(
          'Total w.out LULUCF',
          'Total w.out LULUCF',
          'Total w.out LULUCF',
          'Energy industries',
          'Energy industries',
          'Fugitive emissions from fuels',
          'Manufacturing industries and construction',
          'Industrial processes',
          'Manufacturing industries and construction',
          'Transport',
          'Industrial processes',
          'Manufacturing industries and construction',
          'Industrial processes',
          'Agriculture',
          'Fugitive emissions from fuels',
          'Other sectors',
          'Waste',
          'M.Intl. aviation EU ETS',
          'M.IB.Aviation',
          'M.IB.Navigation',
          'Total w.out LULUCF'
        ),
        Gas=c(
          'Total GHGs (ktCO2e)',
          'Total ESD GHGs (ktCO2e)',
          'Total ETS GHGs (ktCO2e)',
          'Total ETS GHGs (ktCO2e)',
          'Total ESD GHGs (ktCO2e)',
          'Total ETS GHGs (ktCO2e)',
          'Total ETS GHGs (ktCO2e)',
          'Total ETS GHGs (ktCO2e)',
          'Total ESD GHGs (ktCO2e)',
          'Total ESD GHGs (ktCO2e)',
          'Total ESD GHGs (ktCO2e)',
          'Total GHGs (ktCO2e)',
          'Total GHGs (ktCO2e)',
          'Total ESD GHGs (ktCO2e)',
          'Total ESD GHGs (ktCO2e)',
          'Total ESD GHGs (ktCO2e)',
          'Total ESD GHGs (ktCO2e)',
          'Total ETS GHGs (ktCO2e)',
          'Total GHGs (ktCO2e)',
          'Total GHGs (ktCO2e)',
          'Total GHGs (ktCO2e)'
        )
      )
    )
    
    projections <- left_join(mapping.variable, projections, by=c('Category_name', 'Gas'))
    projections <- projections[,c(6,1,4,5,7)]
    colnames(projections) <- c('scenario', 'variable', 'region', 'period', 'value')

    projections <- projections %>% calc_addVariable(
      "`Emi|GHG|Industry|ETS`" = "`Emi|GHG|Industrial Processes|ETS` + `Emi|GHG|Demand|Industry|Energy|ETS`",
      "`Emi|GHG|Industry|ESD`" = "`Emi|GHG|Industrial Processes|ESD` + `Emi|GHG|Demand|Industry|Energy|ESD`",
      "`Emi|GHG|Industry`" = "`Emi|GHG|Industry|ETS` + `Emi|GHG|Industry|ESD`"
    )
    
    projections$value<- projections$value / 1000
    projections$scenario <- paste0('EEA_', projections$scenario, '_2019')
    projections$variable <- paste0(projections$variable, sep=" ", "(Mt CO2-equiv/yr)")
    
    x <- as.magpie(projections,spatial=3,temporal=4,datacol=5)

    return(x)

  } else {
    stop("Not a valid subtype!")
  }
  
  return(x)
 }  
