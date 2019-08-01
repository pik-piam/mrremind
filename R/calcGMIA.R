#'@title calcGMIA
#' 
#' @description Filling gaps in the Historical area equipped for irrigation from GMIA.
#' 
#' @return list of magpie object with data and weight
#' @author Stephen Wirth, Anne Biewald
#' @examples
#' 
#' \dontrun{ 
#' a <- calcOutput("GMIA",aggregate="regglo")
#' }
calcGMIA <- function(){
  x <- readSource("GMIA", "all_data_national", convert=TRUE)
  
  
  small_island_states <- c("ATG","BRB", "COK","NRU", "TUV","NIU", "MDV", "MLT", "GRD", 
                           "VCT","PLW", "LCA", "SGP", "FSM", "BHR", "DMA", "TON",
                           "KIR", "STP", "SYC")
  
  
  #small_non_island_states <- c("VAT", "MCO", "SMR", "LIE", "AND")
  antarctica_or_arctic <- c("ATA", "ATF", "GRL", "SJM")
  #citiestates <-  c("HKG", "MAC")
  desserts_no_agg <-  c("ESH", "GIB")
  
  
  area <- readSource("FAO", "Land")
  sudan <- area[c("SDN","SSD"),1,"6600|Country area.area"]
  sudan["SSD",,] <- 64432900
  sudan["SDN",,] <-  sudan["SDN",,] - as.numeric(sudan["SSD",,])
  area_small_island_states <- area[small_island_states,length(getYears(area)),"6600|Country area.area"]
  #china <- area[c("CHN", "TWN", "HKG"),length(getYears(area)),"6600|Country area.area"]
  x[antarctica_or_arctic,,] <- 0
  x[desserts_no_agg,,] <- 0
  #x[citiestates,,] <- 0
  proportion_sudan <- sudan/as.numeric(as.numeric(sudan["SDN",,])+as.numeric(sudan["SSD",,]))
  #proportion_china <-  china/as.numeric(as.numeric(china["CHN",,])+as.numeric(china["TWN",,])+as.numeric(china["HKG",,]))
  x["SSD",,] <- as.numeric(x["SDN",,])*as.numeric(proportion_sudan["SSD",,])
  x["SDN",,] <- as.numeric(x["SDN",,])*as.numeric(proportion_sudan["SDN",,])
  #x["TWN",,] <- as.numeric(x["CHN",,])*as.numeric(proportion_china["TWN",,])
  #x["CHN",,] <- as.numeric(x["CHN",,])*as.numeric(proportion_china["CHN",,])

  #Dissaggregate values for China: Mainland, Hong Kong, Tiwan, and Macao
  #Based on AEI shares the information from (http://www.fao.org/nr/water/aquastat/irrigationmap/CHN/index.stm)
  
  x["HKG",,]<- as.numeric(x["CHN",,]) * 0.00003
  x["TWN",,]<- as.numeric(x["CHN",,]) * 0.0078
  x["CHN",,]<- as.numeric(x["CHN",,]) * 0.992
  x["MAC",,]<- 0
  
  
  value_small_islands <- as.magpie(apply(x[small_island_states,,],MARGIN = c(2,3),FUN = "mean"))
  
  # missingvalues <- c()
  # for(j in getRegions(x))
  # {
  #   tmp <- dimSums(x[j,,], dim=c(2,3))
  #   if(is.na(tmp))
  #   {
  #     missingvalues <- append(missingvalues, getRegions(tmp)) 
  #   }
  # }
  # area_missingvalues <- area[missingvalues,length(getYears(area)),"6600|Country area.area"]
  # 
  for(i in getRegions(x))
  {
    tmp <- dimSums(x[i,,], dim=c(2,3))
    if(is.na(tmp))
    {
      area_tmp <- area[i,length(getYears(area)),"6600|Country area.area"]
      if(area_tmp!=0)
        {
    x[i,,] <- value_small_islands 
      }
      else
      {
        x[i,,] <- 0
      }
        
    }
  }
  
  return(list(x=x,
              weight=NULL,
              unit="ha",
              min=0,
              description="Area equipped for Irrigation in ha")
         )
  
  }
