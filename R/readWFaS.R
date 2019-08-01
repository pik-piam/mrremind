#' @title readWFaS
#' @description Read in IPCC emissions
#' 
#' Read in Water Consumption data from WFaS: 
#' \strong{Historical} and data from \strong{PCRGLOBWOB} are available
#' \itemize{ 
#' \item  Industrial Water Use
#' \item Industrial Waste Water
#' \item Domestic Water Use
#' \item Domestic Waste Water
#'  }
#' Data is available \strong{with and without technological change} and for the following SSP and RCP Scenarios:
#' \itemize{
#'  \item SSP1RCP4.5
#'  \item SSP2RCP2.6
#'  \item SSP2RCP4.5
#'  \item SSP2RCP6.0
#'  \item SSP3RCP6.0
#' }
#' Data is available spacially explicit and countrywise (not for all RCPs and not with technological change).
#' All available subtypes are listed in the param section.
#' SUbtypes are structured as follows:
#' "Sector"_"use or ww"_"tech, historic or nothing"_ssp Scenarios"_rcp Scenario"_"gridded or nothing"_"2100 or nothing"
#'  Examples:
#'  \itemize{
#'  \item dom_ww_ssp1_rcp4.5 : Returns data for \strong{dom}estic \strong{w}aste \strong{w}ater volume \strong{for all countries}, from 2000 to 2050 for \strong{SSP1} and \strong{RCP4.5} without technological change
#'  \item dom_ww_ssp1_rcp4.5_gridded : Returns \strong{dom}estic \strong{w}aste \strong{w}ater volume on a \strong{0.5?x0.5? grid} from 2000 to 2050 for \strong{SSP1} and \strong{RCP4.5} without technological change
#'  \item dom_ww_tech_ssp2_rcp2.6_gridded_2100 : Returns \strong{dom}estic \strong{w}aste \strong{w}ater volume on a \strong{0.5 Degree x0.5 Degree grid} from 2000 to \strong{2100}, for \strong{SSP2} and \strong{RCP2.6} with \strong{tech}nological change
#'  \item ind_use_historic_gridded : Returns \strong{ind}ustrial water \strong{use} on a \strong{0.5 Degreex0.5 Degree grid}, from \strong{1960 to 1999}
#'  }
#' 
#' 
#' @note Data is missing for domestic use SSP2RCP6.0 and industrial Wastewater with technological change for SSP2RCP4.5.
#' 
#' @param subtype data subtype. available subtypes are:
#' \itemize{
#' \item dom_ww_ssp1_rcp4.5
#' \item dom_ww_ssp2_rcp6.0
#' \item dom_ww_ssp3_rcp6.0
#' \item dom_use_ssp1_rcp4.5
#' \item dom_use_ssp2_rcp6.0
#' \item dom_use_ssp3_rcp6.0
#' \item ind_ww_ssp1_rcp4.5
#' \item ind_ww_ssp2_rcp6.0
#' \item ind_ww_ssp3_rcp6.0
#' \item ind_use_ssp1_rcp4.5
#' \item ind_use_ssp2_rcp6.0
#' \item ind_use_ssp3_rcp6.0
#' \item dom_ww_ssp1_rcp4.5_gridded
#' \item dom_ww_tech_ssp1_rcp4.5_gridded
#' \item dom_use_ssp1_rcp4.5_gridded
#' \item dom_use_tech_ssp1_rcp4.5_gridded
#' \item ind_ww_ssp1_rcp4.5_gridded
#' \item ind_ww_tech_ssp1_rcp4.5_gridded
#' \item ind_use_ssp1_rcp4.5_gridded
#' \item ind_use_tech_ssp1_rcp4.5_gridded
#' \item dom_ww_ssp2_rcp2.6_gridded_2100
#' \item dom_ww_tech_ssp2_rcp2.6_gridded_2100
#' \item dom_use_ssp2_rcp2.6_gridded_2100
#' \item dom_use_tech_ssp2_rcp2.6_gridded_2100
#' \item ind_ww_ssp2_rcp2.6_gridded_2100
#' \item ind_ww_tech_ssp2_rcp2.6_gridded_2100
#' \item ind_use_ssp2_rcp2.6_gridded_2100
#' \item ind_use_tech_ssp2_rcp2.6_gridded_2100
#' \item dom_ww_ssp2_rcp4.5_gridded_2100
#' \item dom_ww_tech_ssp2_rcp4.5_gridded_2100
#' \item dom_use_ssp2_rcp4.5_gridded_2100
#' \item dom_use_tech_ssp2_rcp4.5_gridded_2100
#' \item ind_ww_ssp2_rcp4.5_gridded_2100
#' \item ind_use_ssp2_rcp4.5_gridded_2100
#' \item ind_use_tech_ssp2_rcp4.5_gridded_2100
#' \item dom_ww_ssp2_rcp6.0_gridded_2100
#' \item dom_ww_tech_ssp2_rcp6.0_gridded_2100
#' \item dom_use_tech_ssp2_rcp6.0_gridded_2100
#' \item ind_ww_ssp2_rcp6.0_gridded_2100
#' \item ind_ww_tech_ssp2_rcp6.0_gridded_2100
#' \item ind_use_ssp2_rcp6.0_gridded_2100
#' \item ind_use_tech_ssp2_rcp6.0_gridded_2100
#' \item dom_ww_ssp2_rcp6.0_gridded
#' \item dom_ww_tech_ssp2_rcp6.0_gridded
#' \item dom_use_ssp2_rcp6.0_gridded
#' \item dom_use_tech_ssp2_rcp6.0_gridded
#' \item ind_ww_ssp2_rcp6.0_gridded
#' \item ind_ww_tech_ssp2_rcp6.0_gridded
#' \item ind_use_ssp2_rcp6.0_gridded
#' \item ind_use_tech_ssp2_rcp6.0_gridded
#' \item dom_ww_ssp3_rcp6.0_gridded
#' \item dom_ww_tech_ssp3_rcp6.0_gridded
#' \item dom_use_ssp3_rcp6.0_gridded
#' \item dom_use_tech_ssp3_rcp6.0_gridded
#' \item ind_ww_ssp3_rcp6.0_gridded
#' \item ind_ww_tech_ssp3_rcp6.0_gridded
#' \item ind_use_ssp3_rcp6.0_gridded
#' \item ind_use_tech_ssp3_rcp6.0_gridded
#' \item dom_use_historic_gridded
#' \item dom_ww_historic_gridded
#' \item ind_use_historic_gridded
#' \item ind_ww_historic_gridded
#' }
#' @return magpie object of the WFas data
#' @author Stephen Wirth
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' a <- readSource("WFaS","dom_ww_ssp1_rcp4.5")
#' a <- readSource("WFaS","dom_ww_ssp1_rcp4.5_gridded")
#' a <- readSource("WFaS","dom_use_tech_ssp2_rcp6.0_gridded_2100")
#' }
#' @importFrom reshape2 melt 
#' @importFrom magclass read.magpie
readWFaS <- function(subtype=NULL)
{
  files <- c(dom_ww_ssp1_rcp4.5="PDOM_WW_SSP1.csv",
             dom_ww_ssp2_rcp6.0="PDOM_WW_SSP2.csv",
             dom_ww_ssp3_rcp6.0="PDOM_WW_SSP3.csv",
             dom_use_ssp1_rcp4.5="PDOM_USE_SSP1.csv",
             dom_use_ssp2_rcp6.0="PDOM_USE_SSP2.csv",
             dom_use_ssp3_rcp6.0="PDOM_USE_SSP3.csv",
             ind_ww_ssp1_rcp4.5="PIND_WW_SSP1.csv",
             ind_ww_ssp2_rcp6.0="PIND_WW_SSP2.csv",
             ind_ww_ssp3_rcp6.0="PIND_WW_SSP3.csv",
             ind_use_ssp1_rcp4.5="PIND_USE_SSP1.csv",
             ind_use_ssp2_rcp6.0="PIND_USE_SSP2.csv",
             ind_use_ssp3_rcp6.0="PIND_USE_SSP3.csv",
             dom_ww_ssp1_rcp4.5_gridded="pcrglobwb_rcp4p5_ssp1_PDomWW_monthly_2000_2050.nc4",
             dom_ww_tech_ssp1_rcp4.5_gridded="pcrglobwb_rcp4p5_ssp1_PDomWWTech_monthly_2000_2050.nc4",
             dom_use_ssp1_rcp4.5_gridded="pcrglobwb_rcp4p5_ssp1_PDomUse_monthly_2000_2050.nc4",
             dom_use_tech_ssp1_rcp4.5_gridded="pcrglobwb_rcp4p5_ssp1_PDomUseTech_monthly_2000_2050.nc4",
             ind_ww_ssp1_rcp4.5_gridded="pcrglobwb_rcp4p5_ssp1_PIndWW_monthly_2000_2050.nc4",
             ind_ww_tech_ssp1_rcp4.5_gridded="pcrglobwb_rcp4p5_ssp1_PIndWWTech_monthly_2000_2050.nc4",
             ind_use_ssp1_rcp4.5_gridded="pcrglobwb_rcp4p5_ssp1_PIndUse_monthly_2000_2050.nc4",
             ind_use_tech_ssp1_rcp4.5_gridded="pcrglobwb_rcp4p5_ssp1_PIndUseTech_monthly_2000_2050.nc4",
             dom_ww_ssp2_rcp2.6_gridded_2100="pcrglobwb_rcp2p6_ssp2_PDomWW_monthly_2000_2100.nc4",
             dom_ww_tech_ssp2_rcp2.6_gridded_2100="pcrglobwb_rcp2p6_ssp2_PDomWWTech_monthly_2000_2100.nc4",
             dom_use_ssp2_rcp2.6_gridded_2100="pcrglobwb_rcp2p6_ssp2_PDomUse_monthly_2000_2100.nc4",
             dom_use_tech_ssp2_rcp2.6_gridded_2100="pcrglobwb_rcp2p6_ssp2_PDomUseTech_monthly_2000_2100.nc4",
             ind_ww_ssp2_rcp2.6_gridded_2100="pcrglobwb_rcp2p6_ssp2_PIndWW_monthly_2000_2100.nc4",
             ind_ww_tech_ssp2_rcp2.6_gridded_2100="pcrglobwb_rcp2p6_ssp2_PIndWWTech_monthly_2000_2100.nc4",
             ind_use_ssp2_rcp2.6_gridded_2100="pcrglobwb_rcp2p6_ssp2_PIndUse_monthly_2000_2100.nc4",
             ind_use_tech_ssp2_rcp2.6_gridded_2100="pcrglobwb_rcp2p6_ssp2_PIndUseTech_monthly_2000_2100.nc4",
             dom_ww_ssp2_rcp4.5_gridded_2100="pcrglobwb_rcp4p5_ssp2_PDomWW_monthly_2000_2100.nc4",
             dom_ww_tech_ssp2_rcp4.5_gridded_2100="pcrglobwb_rcp4p5_ssp2_PDomWWTech_monthly_2000_2100.nc4",
             dom_use_ssp2_rcp4.5_gridded_2100="pcrglobwb_rcp4p5_ssp2_PDomUse_monthly_2000_2100.nc4",
             dom_use_tech_ssp2_rcp4.5_gridded_2100="pcrglobwb_rcp4p5_ssp2_PDomUseTech_monthly_2000_2100.nc4",
             ind_ww_ssp2_rcp4.5_gridded_2100="pcrglobwb_rcp4p5_ssp2_PIndWW_monthly_2000_2100.nc4",
             ind_use_ssp2_rcp4.5_gridded_2100="pcrglobwb_rcp4p5_ssp2_PIndUse_monthly_2000_2100.nc4",
             ind_use_tech_ssp2_rcp4.5_gridded_2100="pcrglobwb_rcp4p5_ssp2_PIndUseTech_monthly_2000_2100.nc4",
             dom_ww_ssp2_rcp6.0_gridded_2100="pcrglobwb_rcp6p0_ssp2_PDomWW_monthly_2000_2100.nc4",
             dom_ww_tech_ssp2_rcp6.0_gridded_2100="pcrglobwb_rcp6p0_ssp2_PDomWWTech_monthly_2000_2100.nc4",
             dom_use_tech_ssp2_rcp6.0_gridded_2100="pcrglobwb_rcp6p0_ssp2_PDomUseTech_monthly_2000_2100.nc4",
             ind_ww_ssp2_rcp6.0_gridded_2100="pcrglobwb_rcp6p0_ssp2_PIndWW_monthly_2000_2100.nc4",
             ind_ww_tech_ssp2_rcp6.0_gridded_2100="pcrglobwb_rcp6p0_ssp2_PIndWWTech_monthly_2000_2100.nc4",
             ind_use_ssp2_rcp6.0_gridded_2100="pcrglobwb_rcp6p0_ssp2_PIndUse_monthly_2000_2100.nc4",
             ind_use_tech_ssp2_rcp6.0_gridded_2100="pcrglobwb_rcp6p0_ssp2_PIndUseTech_monthly_2000_2100.nc4",
             dom_ww_ssp2_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp2_PDomWW_monthly_2000_2050.nc4",
             dom_ww_tech_ssp2_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp2_PDomWWTech_monthly_2000_2050.nc4",
             dom_use_ssp2_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp2_PDomUse_monthly_2000_2050.nc4",
             dom_use_tech_ssp2_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp2_PDomUseTech_monthly_2000_2050.nc4",
             ind_ww_ssp2_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp2_PIndWW_monthly_2000_2050.nc4",
             ind_ww_tech_ssp2_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp2_PIndWWTech_monthly_2000_2050.nc4",
             ind_use_ssp2_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp2_PIndUse_monthly_2000_2050.nc4",
             ind_use_tech_ssp2_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp2_PIndUseTech_monthly_2000_2050.nc4",
             dom_ww_ssp3_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp3_PDomWW_monthly_2000_2050.nc4",
             dom_ww_tech_ssp3_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp3_PDomWWTech_monthly_2000_2050.nc4",
             dom_use_ssp3_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp3_PDomUse_monthly_2000_2050.nc4",
             dom_use_tech_ssp3_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp3_PDomUseTech_monthly_2000_2050.nc4",
             ind_ww_ssp3_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp3_PIndWW_monthly_2000_2050.nc4",
             ind_ww_tech_ssp3_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp3_PIndWWTech_monthly_2000_2050.nc4",
             ind_use_ssp3_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp3_PIndUse_monthly_2000_2050.nc4",
             ind_use_tech_ssp3_rcp6.0_gridded="pcrglobwb_rcp6p0_ssp3_PIndUseTech_monthly_2000_2050.nc4",
             dom_use_historic_gridded="pcrglobwb_historical_PDomUse_monthly_1960_2010.nc4",
             dom_ww_historic_gridded="pcrglobwb_historical_PDomWW_monthly_1960_2010.nc4",
             ind_use_historic_gridded="pcrglobwb_historical_PIndUse_monthly_1960_2010.nc4",
             ind_ww_historic_gridded="pcrglobwb_historical_PIndWW_monthly_1960_2010.nc4"
             )
  
  file <- toolSubtypeSelect(subtype,files)
  
  
  
  if(grepl("gridded", subtype))
    {
    if(grepl("2100|historic", subtype))
      {
      years <- c(paste0("y",as.character(c(2000:2100))))
      if(grepl("historic", subtype))
      {
        years <- c(paste0("y",as.character(c(1960:2010))))
      }
      a<- read.magpie(file_name = file, file_type= "nc")
      b <- unwrap(a)
      months <-c(paste0("0", as.character(c(1:9))), as.character(10:12)) 
      c <- array(b, dim=c(59199,length(years),1212/101,1),dimnames = list(getCells(a),years,months,getNames(a, dim=2)))
      d <- as.magpie(c)
      
      return(d)
    }
    d<- read.magpie(file_name = file, file_type= "nc")
    tmp <- unwrap(d)
    d<- as.magpie(tmp,temporal=3)
    
    
    years <- c(paste0("y",as.character(c(2000:2050))))
    getYears(d) <- years
    getNames(d) <- paste0(subtype, "[km^3/month]")
    return(d)
  }
  
  data <- read.csv(file, header = T, sep=";", stringsAsFactors = F)
  data <- data[1:232,]
  
  countries <- read.csv("countrylist.csv", header = T, sep=";", stringsAsFactors = F)
  countries <- countries[1:232,]
  
  data[,1] <- countries[,2]
names(data) <- gsub("X", "y",names(data))

 molten <- melt(data)
 years <- unique(molten$variable)
d <- as.magpie(molten, spatial=1 ,datacol=3) 
getNames(d) <- paste0(subtype, " [km^3/year]")

message("Please contact Anne Biewald regarding application of this Dataset!")

  return(d)
 #  
}