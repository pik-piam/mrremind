#' Convert IIASApop
#' 
#' Convert population data to data on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing population data mixed country-region
#' resolution
#' @return population data as MAgPIE object aggregated to country level
#' @author Lavinia Baumstark

convertIIASApop <- function(x) {
  
  #-------------------- allocation of aggretations -------------------------------   
  # allocate Channel Islands, cc 830, XAA
  jey <- x["XAA",,] 
  ggy <- jey
  getRegions(jey)  <- "JEY"
  jey <- jey * 99000/(99000+65228)   # population data from wikipedia as weight
  getRegions(ggy) <- "GGY"
  ggy <- ggy * 65228/(99000+65228)   # population data from wikipedia as weight
    
  # allocate Netherland ANtills, cc 530, ANT
  cuw <- x["ANT",,] 
  bes <- cuw
  getRegions(cuw)  <- "CUW"
  cuw <- cuw * 150563/(150563+18012)  # population data from wikipedia as weight
  getRegions(bes) <- "BES"
  bes <- bes * 18012/(150563+18012)   # population data from wikipedia as weight (18012)
                                      # Bonaire-13389, Saba-1737, Sint Eustatius-2886
  
  # delete XAA und ANT entry
  delete_ISO <- setdiff(getRegions(x),c("XAA","ANT"))
  x <- x[delete_ISO,,]
  # add data for Jersey(JEY), Guernsey(GGY), Curacao(CUW) and BES
  x <- mbind(x,jey,ggy,cuw,bes)
  #--------------------------------------------------------------------------------
  
  #---------------------- add TWN data --------------------------------------------
  TWN <- new.magpie("TWN",getYears(x),getNames(x))
  twn_data_medium <- dimSums(readSource("PopulationTWN",subtype="medium")[,,"Year-end_Population",pmatch=TRUE]
                                                                         [,,c("0-14 years",
                                                                              "15-64 years",
                                                                              "65+ years"),pmatch=TRUE],3)
  twn_data_high <- dimSums(readSource("PopulationTWN",subtype="high")[,,"Year-end_Population",pmatch=TRUE]
                                                                     [,,c("0-14 years",
                                                                          "15-64 years",
                                                                          "65+ years"),pmatch=TRUE],3)
  twn_data_low <- dimSums(readSource("PopulationTWN",subtype="low")[,,"Year-end_Population",pmatch=TRUE]
                                                                   [,,c("0-14 years",
                                                                        "15-64 years",
                                                                        "65+ years"),pmatch=TRUE],3)
  years <- intersect(getYears(TWN),getYears(twn_data_medium))
  TWN[,years,"pop_SSP1"] <- twn_data_low[,years,]
  TWN[,years,"pop_SSP2"] <- twn_data_medium[,years,]
  TWN[,years,"pop_SSP3"] <- twn_data_high[,years,]
  TWN[,years,"pop_SSP4d"] <- twn_data_medium[,years,]
  TWN[,years,"pop_SSP5"] <- twn_data_low[,years,]
  
  ####### projecting population until 2150
  gr_TWN <- new.magpie("TWN",getYears(TWN),getNames(TWN))
  # calculate growht rate from 2055 to 2060
  gr_TWN[,2060,] <- (TWN[,2060,] / setYears(TWN[,2055,],NULL)) - 1
  # calculating grow rates from 2060 to 2150 assuming zero growth in 2200
  for (t in seq(2065,2100,5)) {
    gr_TWN[,t,] <- setYears(gr_TWN[,2060,],NULL) - (t-2060)/5 * setYears(gr_TWN[,2060,],NULL)/28    
  }  
  # applying assumed growth rates to population matrices
  for (t in seq(2065,2100,5)) {
    TWN[,t,] <- setYears( TWN[,t-5,],t) * (1 + gr_TWN[,t,]) 
  }
  TWN[,2010,] <- 23162.123
  x <- mbind(x,TWN)      # FIXME woher die Daten für 2010??? historische Quelle
  #--------------------------------------------------------------------------------
  
  #check whether the country list agrees with the list of countries in the moinput library
  #remove unrequired data, add missing data 
  x <- toolCountryFill(x,fill=0)
  return(x)
}  
