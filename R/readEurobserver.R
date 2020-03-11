#' @title readEurobserver
#' @description Read in Job numbers (direct and indirect) and Share of total world export (Wind, Solar PV, Hydro)
#' for EU countries from Eurobserver reports in 2014, 2016, and 2018
#' @author Aman Malik
#' @param subtype "employment" or "share"
#' @importFrom dplyr bind_rows
#' @importFrom mgsub mgsub
#' @importFrom readxl read_excel
#' @importFrom tidyr gather


readEurobserver <- function(subtype){
  
  
  if (subtype=="employment")
  { 
    ## For Year 2014
    input_2014 <- readxl::read_excel("Eurobserver.xlsx",sheet="2014",n_max = 28)
    input_2014 <- input_2014[-2] # removing column "country totals"
    # removing asterisks and empty spaces in colnames
    colnames(input_2014) <- gsub(pattern = "\\*||\\*\\**||",replacement = "",x = colnames(input_2014))
    # renaming certain techs
    colnames(input_2014) <- mgsub::mgsub(pattern = c("Solid Biomass","Wind power","Photovoltaic","Geothermal energy","Waste "),
                                        c("Biomass","Wind","Solar|PV","Geothermal","Waste"),string = colnames(input_2014))
    # assuming all numbers "< (less)" than a number to be equal to that number. 
    # Therefore <50 becomes 50
    input_2014[c(1:28),c(2:11)] <- mgsub(pattern =  c(" ","\\*","<"),
                                        replacement =c("","",""),string =  as.matrix(input_2014[c(1:28),c(2:11)]))
    # converting all numbers to numeric
    input_2014[,2:11] <- as.numeric(unlist(input_2014[,2:11]))
    colnames(input_2014)[1] <- "country"
    # adding column period
    input_2014$period <- 2014
    # reordering data frame to make it easier to convert into magpie oject  
    input_2014 <- input_2014[c(1,12,2:11)]

    ## For year 2015
    input_2015 <- readxl::read_excel("Eurobserver.xlsx",sheet="2015",n_max = 28)
    input_2015 <- input_2015[-2]
    colnames(input_2015)[1] <- "country"
    colnames(input_2015) <- mgsub::mgsub(pattern = c("Photovoltaic","Waste\\*","Hydro"),
                                         c("Solar|PV","Waste","Hydropower"),string = colnames(input_2015))
    input_2015[c(1:28),c(2:11)] <- mgsub(pattern = c(" ","\\*","\\*\\*","n\\.a\\.","<"),
                                         replacement =c("","","","0",""),string =  as.matrix(input_2015[c(1:28),c(2:11)]))
    input_2015[,2:11] <- as.numeric(unlist(input_2015[,2:11]))
    input_2015$period <- 2015
    input_2015 <- input_2015[c(1,12,2:11)]

    ## For year 2016
    input_2016 <- readxl::read_excel("Eurobserver.xlsx",sheet="2016",n_max = 28)
    input_2016 <- input_2016[-2]
        colnames(input_2016)[1] <- "country"
    colnames(input_2016) <- mgsub::mgsub(pattern = c("PV","Hydro"),
                                         c("Solar|PV","Hydropower"),string = colnames(input_2016))
    input_2016[c(1:28),c(2:11)] <- mgsub(pattern = c(" ","\\*","<"),
                                         replacement =c("","",""),string =  as.matrix(input_2016[c(1:28),c(2:11)]))
    input_2016[,2:11] <- as.numeric(unlist(input_2016[,2:11]))
    input_2016$period <- 2016
    input_2016 <- input_2016[c(1,12,2:11)]

   x <- dplyr::bind_rows(input_2014,input_2015,input_2016)
   x <- as.magpie(x,spatial=1,temporal=2,datacol=3)
   
   return (x)
  }
    
  if (subtype=="share")  
  {
    input_wind <- readxl::read_excel("Eurobserver.xlsx",sheet = "Wind_exports",na = "n.a.")
    input_wind <- gather(input_wind,2:5,key = "period",value="value")
    input_wind$variable="Wind"
    colnames(input_wind)[1] <- "country"
    
    input_solar <- readxl::read_excel("Eurobserver.xlsx",sheet = "Solar_PV_exports",na = "n.a.")
    input_solar <- gather(input_solar,2:5,key = "period",value="value")
    input_solar$variable="Solar"
    colnames(input_solar)[1] <- "country"
    
    input_hydro <- readxl::read_excel("Eurobserver.xlsx",sheet = "Hydropower_exports",na = "n.a.")
    input_hydro <- gather(input_hydro,2:5,key = "period",value="value")
    input_hydro$variable="Hydropower"
    colnames(input_hydro)[1] <- "country"
    
    input <- dplyr::bind_rows(input_wind,input_solar,input_hydro)
    input <- input[c(1,2,4,3)]
    x <- as.magpie(input,spatial=1,temporal=2,datacol=4)
    
    return (x)
  }
  
  
  
}