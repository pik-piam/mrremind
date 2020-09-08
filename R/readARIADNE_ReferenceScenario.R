#' Read ARIADNE Reference Scenario
#' 
#' Read ARIADNE Reference Scenario data from various .xls files as magpie object
#' 
#' @param subtype data subtype. Either "population", "gdp", or "gdp_corona"
#' @return magpie object of ARIADNE reference scenario data by country
#' @author Falk Benke
#' @importFrom readxl excel_sheets read_excel
#' @importFrom reshape2 melt

readARIADNE_ReferenceScenario <- function(subtype){
  if (subtype == 'population') {

    populationSheet <- suppressMessages(
      read_excel('POP_EU-27_Eurostat.xlsx', range='B12:T46', sheet='Pop_Total',)
    )
    populationSheet <- populationSheet[c(seq(1,27), seq(31,34)),]
    colnames(populationSheet)[1] <- 'Region'
    populationSheet$Region <- c(populationSheet$Region[1:8], 'GR', populationSheet$Region[10:27], 'IS', 'LI', 'NO', 'CH')
    populationSheet[,seq(2,19)] <- sapply(populationSheet[,seq(2,19)], as.numeric)
    populationSheet[,seq(2,19)] <- populationSheet[,seq(2,19)] / 1000000
    populationSheet <- melt(populationSheet, id.vars=1)
    populationSheet <- cbind(c('Population (million)'), populationSheet)
    colnames(populationSheet) <- c('variable', 'region','period','value')
    x <- as.magpie(populationSheet,spatial=2,datacol=4,temporal=3)

  } else if (subtype %in% c('gdp', 'gdp_corona')) {
    gdpSheet <- suppressMessages(
      read_excel('GDP_Base_Corona_EU-28_V02.xlsx', range='A2:AL29', sheet='GDP_comparison')
    )
    # remove empty columns
    gdpSheet <- gdpSheet[,-which(is.na(gdpSheet[1,]))]
    # correct country code for Greece EL -> GR
    gdpSheet$Regions[9] <- 'GR'
    gdpSheet <- melt(gdpSheet, id.vars=1)
    colnames(gdpSheet) <- c('region','period','value')

    # add variable column
    gdpSheet <- cbind(variable=c('GDP|MER (billion US$2005/yr)'), gdpSheet)
    
    # select rows matching the subtype
    coronaCols <- unique(gdpSheet$period)[seq(6,length(unique(gdpSheet$period)),2)]
    if(subtype == 'gdp'){
      gdpSheet <- gdpSheet[-which(gdpSheet$period %in% coronaCols),]
    } else {
      gdpSheet <- gdpSheet[which(gdpSheet$period %in% coronaCols),]
    }
    gdpSheet$period <- sapply(substr(gdpSheet$period,0,4), as.numeric)
    
    # convert US$2010/yr to US$2005/yr
    gdpSheet$value <- gdpSheet$value / 1000 * 0.9

    
    x <- as.magpie(gdpSheet,spatial=2,datacol=4,temporal=3)

  } else {
    stop('Not a valid subtype!')
  }

  return(x)
}
