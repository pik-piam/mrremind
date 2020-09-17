#' @author Falk Benke
#' @importFrom readxl read_excel
#' @importFrom reshape2 melt
#' @importFrom dplyr left_join
#' @importFrom stringr str_trim
#' @importFrom quitte calc_addVariable

readEEA_GHGProjections <- function(){

  projections <- read.csv(file='GHG_projections_2019_EEA.csv', stringsAsFactors = FALSE)
  projections <- projections[projections$CountryCode != '', ]
  projections <- projections[projections$CountryCode != 'EU', ]
  projections <- projections[projections$Gapfilled != as.double(0), ]
  projections <- projections[c(1,2,5,6,7,10)]
  projections <- na.omit(projections, cols=projections$Gapfilled)
  projections$Category_name <- str_trim(projections$Category_name, side = "right")

  mapping.variable <- as.data.frame(cbind(Variable=c(
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
  ))
  
  projections <- left_join(mapping.variable, projections, by=c('Category_name', 'Gas'))
  projections <- projections[,c(6,1,4,5,7)]

  colnames(projections) <- c('scenario', 'variable', 'region', 'period', 'value')

  projections <- projections %>% calc_addVariable(
    "`Emi|GHG|Industry|ETS`" = "`Emi|GHG|Industrial Processes|ETS` + `Emi|GHG|Demand|Industry|Energy|ETS`",
    "`Emi|GHG|Industry|ESD`" = "`Emi|GHG|Industrial Processes|ESD` + `Emi|GHG|Demand|Industry|Energy|ESD`",
    "`Emi|GHG|Industry`" = "`Emi|GHG|Industry|ETS` + `Emi|GHG|Industry|ESD`"
  )

  projections$value<- projections$value / 1000
  projections$scenario <- paste(projections$scenario, '_2019', sep='')
  projections$variable <- paste(projections$variable, "(Mt CO2-equiv/yr)", sep=" ")

  x <- as.magpie(projections,spatial=3,temporal=4,datacol=5)
  return(x)
}
