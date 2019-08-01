#' @title {readGMIA }
#' 
#' @description Read Global Irrigation Map 
#' 
#' Read the Data from Siebert et.al on Irrigated Areas for each Country. Data contains total valuesas well as values from groundwater, surface water and non convential water sources for the following Categories:
#' \itemize{
#' \item Area equipped for Irrigation (AEI)
#' \item Area acutally irrigated (AAI)
#' \item Consumption Irrigatoin Water use (ICU)
#' }
#' The following Data is also available spatialy explicit with a resolution of 5 arcmin:
#' \itemize{
#' \item AEI in Percent
#' \item AEI in Ha
#' \item AAI as percent of AEI
#' \item AEI from groundwater sources as percent of total AEI
#' \item AEI from surface water sources as percent of total AEI
#' \item AEI from non conventional sources as percent of total AEI
#' }
#' There also exists a correct function to aggregate the data to 0.5 degree resolution, set convert="correctonly" to run.
#' 
#' 
#' @param subtype : Available subtypes are:
#' \itemize{
#' \item all_data_national : National Data on AEI (including differentiation by source (Groundwater, Surface water and nonconventional)), AAI
#' \item aei_pct : AEI in Percent
#' \item aei_ha : AEI in ha
#' \item aai_pct_aei : AAI as percentage of AEI
#' \item aeigw_pct_aei AEI from Groundwater sources as Percentage of total AEI
#' \item aeisw_pct_aei : AEI from Surface water sources as Percentage of total AEI
#' \item aeinc_pct_aei : AEI from nonconventional sources as Percentage total AEI
#' }
#' @return magpie object of the Irrigated Area  data
#' @author Stephen Wirth
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' a <- readSource("GlobalIrrigationMap", "all_data_national")
#' a <- readSource ("GMIA", "aei_pct", convert=F)
#' a <- readSource ("GMIA", "aei_pct", convert="correctonly")

#' }


readGMIA <- function(subtype=NULL){
  
  files=c(all_data_national="HESS_2010_159_Supplement_S2.csv",
          aei_pct = "gmia_v5_aei_pct.asc",
          aei_ha = "gmia_v5_aei_ha.asc",
          aai_pct_aei = "gmia_v5_aai_pct_aei.asc",
          aeigw_pct_aei = "gmia_v5_aeigw_pct_aei.asc",
          aeisw_pct_aei = "gmia_v5_aeisw_pct_aei.asc",
          aeinc_pct_aei = "gmia_v5_aeinc_pct_aei.asc")
  
  file <- toolSubtypeSelect(subtype, files)
  
  #check wether data is gridded for countries
  if(subtype=="all_data_national"){
    #read data for countries
a <-  read.csv("HESS_2010_159_Supplement_S2.csv", sep=";",header=TRUE,skip=3, stringsAsFactors = FALSE)
a <- subset(a,select=c(-(2:5)))
a <- a[1:(length(a[,1])-1),]
#change dots in columnames to underscores
colnames(a) <- gsub("\\.{2}",".",colnames(a))
colnames(a) <- gsub("\\.", "_", colnames(a))
#convert to magpieobject
b <- as.magpie(a,spatial=1)
getRegions(b) <- a[,1]
# since data range from 2000 to 2008 all data is used for every year
c <- new.magpie(cells_and_regions = getRegions(b),years = c(2000:2008),names = getNames(b), sets = getSets(b), fill=0)
getRegions(c) <- a[,1]
c[,c(2000:2008),] <- b
return(c)
  }
  # if data is gridded
else
{
  #read asci file and do some restructuring
  a <- read.table(file, skip=6)
  a <- unlist(a)
  a <- as.data.frame(a)
  b <- as.magpie(a, spatial=1)
  # new magpieobject with the correct dimensions
  c <- array(NA,dim= c(length(b),9,1))
  c[,1:9,] <- b
  c <- as.magpie(c, spatial=1, temporal=2)
  #change years and names
  getYears(c) <- c(2000:2008)
  getNames(c) <- paste0(subtype, " in 5 arcmin resolution")
  getCells(c) <- paste0("GLO.",1:dim(c)[1])

}
  
 
  return(c)
  
  
   
}