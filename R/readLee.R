#' readLee
#' Read in Aviation emission data from Lee
#' 
#' @return magpie object of Aviation emission / emission factors data
#' @author Julian Oeser
#' @param subtype Either 'emi' or 'ef'
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="", subtype="Waste")
#' }
#' 
#'
#' @importFrom readxl read_excel
#' @importFrom reshape2 melt
#' @importFrom data.table dcast


readLee <- function(subtype) {
  
  avi <- read_excel("Lee_Global_Aviation.xlsx", sheet="output_to_R") # data only contains BC and NOx emissions from aircraft
  
  # restructure dataframe
  avi <- melt(avi,id=c("Year","Tg")) # from wide to long format, keep only Year and Tg
  avi <- dcast(avi,Year + variable ~ Tg) # from long to wide format, bring contents of Tg to columns and differentiate by Year and variable
  avi <- avi[,c(1,2,4,3,5)] # reorder columns
  tmp_A1 <- as.magpie(avi[avi$variable=="A1", !names(avi) == "variable"])
  tmp_A1 <- add_dimension(tmp_A1,dim=3.2,add="scenario",nm="A1")
  avi <- mbind(tmp_A1)
  
  avi <- time_interpolate(avi, interpolated_year=seq(2005,2100,5), integrate_interpolated_years=TRUE, extrapolation_type="constant")
  avi <- avi[,"y2000",invert=TRUE]
  
  # Add sector name
  getNames(avi) <- paste0("Aviation.",getNames(avi))
  
  avi_act <- collapseNames(avi[,,"Fuel"]) # activity until 2020 is scenario independent
  avi_emi <- avi[,,"Fuel",invert=TRUE]
  
  # add empty SSP dimensions
  getSets(avi_emi) <- c("region","year","sector","emi","scenario")
  avi_emi <- add_columns(avi_emi, addnm=c("SSP1","SSP2","SSP5"),dim=3.3)
  
  # fill with scenrio data (SSP5 = A1, SSP1 and SSP2 need to be confirmed)
  avi_emi[,,"SSP1"] <- avi_emi[,,"A1"]
  avi_emi[,,"SSP5"] <- avi_emi[,,"SSP1"]
  avi_emi[,,"SSP2"] <- avi_emi[,,"A1"]
  avi_ef  <- avi_emi/avi_act
  getSets(avi_ef) <- c("region","year","sector","emi","scenario")
  
  if (subtype == "ef") {
    out <- avi_ef
  } else if (subtype == "emi"){
    out <- avi_emi
  } else {
    stop("Invalid subtype.")
  }
  
  return(out)
  
}
