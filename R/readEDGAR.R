#' @importFrom readxl read_excel

readEDGAR<- function(subtype) {
  
  files <- c(ch4waste = "v4.2_CH4_2005.xlsx",  
             n2owaste = "v4.2_N2O_2005.xlsx", 
             co2 = "v4.2_CO2_excl_scc_2005.xlsx",
             ch4_history = "v4.2_CH4_tot_1970_2008.xlsx",
             CO = "v4.2_EM_CO_2005.xlsx",
             NOx = "v4.2_EM_NOX_2005.xlsx",
             VOC = "v4.2_NMVOC_2005.xlsx",
             NH3 = "v4.2_NH3_2005.xlsx",
             SO2 = "v4.2_SO2_2005.xlsx",
             PM10 = "v4.2_PM10_2005.xlsx",
             HFC = "v4.2_HFC_1970_2008.xlsx",
             GHG = "EDGARv42FT2012_GHG.xlsx")

  
  skip <- c(ch4waste = 10,
            n2owaste = 10,
            co2 = 19,
            ch4_history = 9,
            CO = 10,
            NOx = 10,
            VOC = 10,
            NH3 = 10,
            SO2 = 10,
            PM10 = 10,
            HFC = 9,
            GHG = 8)   
  
  file <- toolSubtypeSelect(subtype,files)
  
  # add read function that copes with different data input types
  ed <- as.data.frame(read_excel(file,  skip=skip[subtype]))
  ed <- ed[!is.na(ed[[1]]),]
  
  if(subtype=="co2") {
    names(ed)[names(ed)=="TOTAL (IPCC)"]  <- "TOTAL"
  }
  
  if(subtype=="ch4waste" | subtype=="n2owaste" | subtype=="co2" | subtype=="CO" | subtype=="NOx" | subtype=="VOC" | subtype=="NH3" | subtype=="SO2" | subtype=="PM10") {
    ed <- ed[-nrow(ed),]
    ed$"World Region" <- NULL
    ed$Country      <- NULL
    #    row.names(ed) <- ed$ISO_A3
    x <- as.magpie(ed, spatial=1)
    getYears(x) <- "y2005"
    if ("E27" %in% getRegions(x)) x <- x["E27",,,invert=TRUE]
  } else if(subtype=="ch4_history" | subtype=="HFC") {
    ed$Name          <- NULL
    ed$"Remind region" <- NULL
    ed$"World Region"  <- NULL
    ed$`IPCC-Annex`  <- NULL
    ed$IPCC_description <- NULL
    ed               <- ed[!ed$ISO_A3=="ISO_A3",]
    ed               <- ed[!is.na(ed$ISO_A3),]
    x <- as.magpie(ed)
  } else if (subtype == "GHG") {
    ed$`Country name` <- NULL
    x <- as.magpie(ed,spatial=1)
    getNames(x)  <- "GHG total in kton (Gg) CO2eq /yr"
  } 
  getNames(x) <- sub(" *$","",getNames(x))
  return(x)
}
