#' calcMacBase
#' calculate MacBase
#' 
#' @param subtype Source of subset of emissions
#' @return magpie object
#' @author David Klein, Julian Oeser
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ a <- calcOutput(type="MacBaseLandUse")
#' }
#' 
#' @importFrom magclass dimSums setCells


calcMacBaseLandUse <- function(subtype){
 
  x <- readSource("MAgPIE", subtype = "macBase")
  # merge all dimensions
  getNames(x) <- gsub("\\.","",getNames(x))
  # split up the fourth dimension again
  getNames(x) <- gsub("SSP","\\.SSP",getNames(x))
  getNames(x) <- gsub("SDP","\\.SDP",getNames(x))
  
  # Add missing rcp dimension (data only exists for Baseline=none, use Baseline data for RCPs)
  x <-add_dimension(x,dim=3.3,add="rcp",nm=c("rcp20","rcp26","rcp45","none"))
  getSets(x) <- c("region","year","type","c_LU_emi_scen","rcp")

  if (subtype == "MAgPIE") {
    
    # emission types that are updated with new MAgPIE 4 data
    emi_mag <- c("co2luc",
                 "n2oanwstm",
                 "n2ofertin",
                 "n2oanwstc",
                 "n2ofertcr",
                 "n2ofertsom",
                 "n2oanwstp",
                 "ch4rice",  
                 "ch4anmlwst",
                 "ch4animals")
    
    x <- x[,,emi_mag]

    # Replace CO2 LUC baseline for SSP2, since there is newer data from MAgPIE 4.0 (the tax000,0 scenario from the CO2MAC-2018 runs)
    x_co2 <- readSource("MAgPIE", subtype = "macBaseCO2luc")
    x_co2 <- time_interpolate(x_co2,getYears(x))
    
    # write co2 baseline to all RCPs
    x[,,"co2luc.SSP2"] <- x_co2
    
    # Replace CH4 and N2O LUC baseline for SSP2 and SSP1, since there is newer data from a coupled REMIND-MAgPIE 4.0 Baseline run
    # Read ch4 and n2o emissions from MAgpIE Base scenario
    x_ch4_n2o <- calcOutput("MAgPIEReport",subtype="ch4n2o",aggregate=FALSE)
    # change the order of the entries in the 3. dimension
    getNames(x_ch4_n2o) <- sub("^([^\\.]*)\\.([^\\.]*)\\.([^\\.]*)$","\\3.\\1.\\2",getNames(x_ch4_n2o))  
   
    # convert Mt N2O/yr -> Mt N/yr
    n2o_entities <- getNames(x_ch4_n2o[,,"n2o",pmatch=TRUE])
    x_ch4_n2o[,,n2o_entities] <- x_ch4_n2o[,,n2o_entities] * 28/44
    
    x_ch4_n2o <- time_interpolate(x_ch4_n2o,getYears(x))
    getSets(x_ch4_n2o) <- c("region","year","type","c_LU_emi_scen","rcp") # use same names as x

    x[,,getNames(x_ch4_n2o)] <- x_ch4_n2o

  } else if (subtype == "Exogenous") {
    
    # emission subtype that are not updated with new MAgPIE 4 data  
    emi_exo <- c("n2oforest",
                 "n2osavan",
                 "n2oagwaste",
                 "ch4forest",
                 "ch4savan",
                 "ch4agwaste")
    
    # select Baseline (all other RCPs are only copies of Baseline anyway, see above)
    x <- collapseNames(x[,,emi_exo][,,"none"],collapsedim = 3.3)
    
  } else {
    stop("Unkown subtype: ",subtype)
  }
  
  # FS: Australia-specific, adjust 2005-2015 MacBase for lucco2 to NGGI data for LULUCF CO2 
  # to catch recent trend from afforestation/forest management:
  # http://ageis.climatechange.gov.au/, graph: http://ageis.climatechange.gov.au/Chart_KP.aspx?OD_ID=79041341749&TypeID=2
  # does not agree with EDGAR data which shows even increasing trend, yet I trust NGGI more also because of this article:
  # https://www.researchgate.net/publication/301942515_Deforestation_in_Australia_Drivers_trends_and_policy_responses
  # long-term solution for new regions not in Magpie still needed
  
  if (subtype == "MAgPIE") {
    GtC_2_MtCO2 <- 3666.667
    
    x_old_co2luc <- x[,c("y2005","y2010","y2015"),"co2luc"]
    
    # adjust Australia historic co2luc emissions
    x["AUS",c("y2005"),"co2luc"] <- 70/GtC_2_MtCO2
    x["AUS",c("y2010"),"co2luc"] <- 5/GtC_2_MtCO2
    x["AUS",c("y2015"),"co2luc"] <- -20/GtC_2_MtCO2
    
    # adjust Canada historic co2luc emissions s.t. CAZ has same emissions than before (from MagPIE runs)
    x["CAN",c("y2005","y2010","y2015"), "co2luc"] <- setCells(dimSums(x_old_co2luc[c("AUS","CAN"),,], dim = 1)
                                                              - x["AUS",c("y2005","y2010","y2015"),"co2luc"], "CAN")
      
  }
  
  return(list(x           = x,
              weight      = NULL,
              unit        = "unit",
              description = "baseline emissions of N2O and CH4 from landuse based on data from Magpie"))
  
}
