#' @importFrom utils download.file tail unzip

downloadEDGAR <- function(subtype=NULL) {
  
  links <- c(CO="ftp://edgar.jrc.ec.europa.eu/v431_v2_grids/CO/v431_v2_REFERENCE_CO_1970-2010.xls",
             NH3="ftp://edgar.jrc.ec.europa.eu/v431_v2_grids/NH3/v431_v2_REFERENCE_NH3_1970-2010.xls",
             NOX="ftp://edgar.jrc.ec.europa.eu/v431_v2_grids/NOx/v431_v2_REFERENCE_NOx_1970-2010.xls",
             PM10="ftp://edgar.jrc.ec.europa.eu/v431_v2_grids/PM10/v431_v2_REFERENCE_PM10_1970-2010.xls",
             SO2="ftp://edgar.jrc.ec.europa.eu/v431_v2_grids/SO2/v431_v2_REFERENCE_SO2_1970-2010.xls",
             ch4waste="ftp://edgar.jrc.ec.europa.eu/v42/CH4/v42_CH4_2008-1970_timeseries.zip",
             ch4_history="ftp://edgar.jrc.ec.europa.eu/v42/CH4/v42_CH4_2008-1970_timeseries.zip",
             n2owaste="ftp://edgar.jrc.ec.europa.eu/v42/N2O/v42_N2O_2008-1970_timeseries.zip",
             co2="ftp://edgar.jrc.ec.europa.eu/v42/CO2_excl_short-cycle_org_C/v42_CO2_excl_short-cycle_org_C_2008-1970_timeseries.zip",
             VOC="ftp://edgar.jrc.ec.europa.eu/v42/NMVOC/v42_NMVOC_2008-1970_timeseries.zip")
  
  fnames <- sapply(links, function(x){tail(strsplit(x, split = "/")[[1]], 1)})
  
  ### execute downloading
  if (is.null(subtype)) {
    lapply(1:length(links), FUN = function(x){ download.file(links[x], destfile=fnames[x], mode="wb")})
  } else {
    download.file(links[[subtype]], destfile=fnames[[subtype]], mode="wb")
  }
  
  ### tables wanted as output
  tables <- c(ch4waste = "v4.2_CH4_2005.csv",  
              n2owaste = "v4.2_N2O_2005.csv", 
              co2 = "v4.2_CO2_excl_scc_2005.csv",
              ch4_history = "v4.2_CH4_tot_1970_2008.xls",
              CO = "v431_v2_REFERENCE_CO_1970-2010.xls", 
              NOx = "v431_v2_REFERENCE_NOx_1970-2010.xls",  
              VOC = "v4.2_NMVOC_2005.csv",
              NH3 = "v431_v2_REFERENCE_NH3_1970-2010.xls", 
              SO2 = "v431_v2_REFERENCE_SO2_1970-2010.xls",  
              PM10 = "v431_v2_REFERENCE_PM10_1970-2010.xls", 
              HFC = "v4.2_HFC_1970_2008.xls")
  
  ###  unzip files
  zipfiles <- list.files(pattern=".zip$")
  lapply(zipfiles, unzip)
  lapply(zipfiles, unlink)
  
  ### delete unwanted files
  allfiles <- list.files()
  unlink(allfiles[which(!(allfiles %in% tables))])
  
}