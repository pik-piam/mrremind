#' Convert SSP data
#' 
#' Convert SSP data on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing SSP data mixed country-region resolution
#' @param subtype data subtype. Either "all" or "ratioPM"
#' @return SSP data as MAgPIE object aggregated to country level, all models,
#' all SSP-scenarios
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{ a <- convertSSP(x,subtype="all")
#' }
 
convertSSP <- function(x,subtype) {
  if(subtype=="all") {

    #---------------------- add TWN data to population --------------------------------------------
    TWN_pop <- new.magpie("TWN",getYears(x[,,"Population"][,,"IIASA-WiC POP"]),getNames(x[,,"Population"][,,"IIASA-WiC POP"]))
    aged <- c( "Population|Female|Aged15-19",
               "Population|Female|Aged20-24",
               "Population|Female|Aged25-29",
               "Population|Female|Aged30-34",
               "Population|Female|Aged35-39",
               "Population|Female|Aged40-44",
               "Population|Female|Aged45-49",
               "Population|Female|Aged50-54",
               "Population|Female|Aged55-59",
               "Population|Female|Aged60-64",
               "Population|Male|Aged15-19",
               "Population|Male|Aged20-24",
               "Population|Male|Aged25-29",
               "Population|Male|Aged30-34",
               "Population|Male|Aged35-39",
               "Population|Male|Aged40-44",
               "Population|Male|Aged45-49",
               "Population|Male|Aged50-54",
               "Population|Male|Aged55-59",
               "Population|Male|Aged60-64" )
    TWN_lab <- new.magpie("TWN",getYears(x[,,aged][,,"IIASA-WiC POP"]),getNames(x[,,aged][,,"IIASA-WiC POP"]))
    # read in population data
    twn_pop_medium <- dimSums(readSource("PopulationTWN",subtype="medium",convert=FALSE)[,,"Year-end_Population",pmatch=TRUE]
                                                                          [,,c("0-14 years",
                                                                                "15-64 years",
                                                                                "65+ years"),pmatch=TRUE],3)
    twn_pop_high   <- dimSums(readSource("PopulationTWN",subtype="high",convert=FALSE)[,,"Year-end_Population",pmatch=TRUE]
                                                                        [,,c("0-14 years",
                                                                             "15-64 years",
                                                                             "65+ years"),pmatch=TRUE],3)
    twn_pop_low    <- dimSums(readSource("PopulationTWN",subtype="low",convert=FALSE)[,,"Year-end_Population",pmatch=TRUE]
                                                                       [,,c("0-14 years",
                                                                            "15-64 years",
                                                                            "65+ years"),pmatch=TRUE],3)
    # read in labour data
    twn_lab_medium <- readSource("PopulationTWN",subtype="medium",convert=FALSE)[,,"Year-end_Population",pmatch=TRUE][,,"15-64 years",pmatch=TRUE] 
    twn_lab_high   <- readSource("PopulationTWN",subtype="high",convert=FALSE)[,,"Year-end_Population",pmatch=TRUE][,,"15-64 years",pmatch=TRUE] 
    twn_lab_low    <- readSource("PopulationTWN",subtype="low",convert=FALSE)[,,"Year-end_Population",pmatch=TRUE][,,"15-64 years",pmatch=TRUE] 
      
    # transfer data until 2060      
    years <- intersect(getYears(TWN_pop),getYears(twn_pop_medium))
    TWN_pop[,years,"SSP1",pmatch=TRUE] <- twn_pop_low[,years,]
    TWN_pop[,years,"SSP2",pmatch=TRUE] <- twn_pop_medium[,years,]
    TWN_pop[,years,"SSP3",pmatch=TRUE] <- twn_pop_high[,years,]
    TWN_pop[,years,"SSP4d",pmatch=TRUE] <- twn_pop_medium[,years,]
    TWN_pop[,years,"SSP5",pmatch=TRUE] <- twn_pop_low[,years,]
    
    TWN_lab[,years,"SSP1",pmatch=TRUE] <- twn_lab_low[,years,]/length(aged)
    TWN_lab[,years,"SSP2",pmatch=TRUE] <- twn_lab_medium[,years,]/length(aged)
    TWN_lab[,years,"SSP3",pmatch=TRUE] <- twn_lab_high[,years,]/length(aged)
    TWN_lab[,years,"SSP4d",pmatch=TRUE] <- twn_lab_medium[,years,]/length(aged)
    TWN_lab[,years,"SSP5",pmatch=TRUE] <- twn_lab_low[,years,]/length(aged)
    
    ####### projecting population and labour until 2150
    gr_TWN <- new.magpie("TWN",getYears(TWN_pop),getNames(TWN_pop))
    # calculate growht rate from 2055 to 2060
    gr_TWN[,2060,] <- (TWN_pop[,2060,] / setYears(TWN_pop[,2055,],NULL)) - 1
    # calculating grow rates from 2060 to 2150 assuming zero growth in 2200
    for (t in seq(2065,2100,5)) {
      gr_TWN[,t,] <- setYears(gr_TWN[,2060,],NULL) - (t-2060)/5 * setYears(gr_TWN[,2060,],NULL)/28    
    }  
    # applying assumed growth rates to population and labour matrices
    for (t in seq(2065,2100,5)) {
      TWN_pop[,t,] <- setYears( TWN_pop[,t-5,],t) * (1 + gr_TWN[,t,]) 
      TWN_lab[,t,] <- setYears( TWN_lab[,t-5,],t) * (1 + gr_TWN[,t,])
    }
    TWN_pop[,2010,] <- 23162.123
    TWN_lab[,2010,] <- 16774.919195/length(aged)
    x[,,"IIASA-WiC POP"]["TWN",,"Population"] <- TWN_pop/1000      
    x[,,"IIASA-WiC POP"]["TWN",,aged]         <- TWN_lab/1000      
    #--------------------------------------------------------------------------------
    
    
    x[is.na(x)] <- 0    # substitute NA by 0
  
    #--------------------------------------------------------------------------------
    # check whether the country list agrees with the list of countries in the moinput library
    # remove unrequired data, add missing data 
    x <- toolCountryFill(x, fill=0)
   
      } else if(subtype=="ratioPM") {
    # fill all the rest with 1
    x <- toolCountryFill(x,fill=1)
  }  
  return(x)
}  
