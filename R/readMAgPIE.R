
#' Read MAgPIE data
#' 
#' Read-in MAgPIE data
#' 
#' @param subtype Either "EmiAirPoll", "macBase" or "co2tax"
#' @importFrom madrat regionscode
#' @return magpie object
#' @author Julian Oeser
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="MAgPIE",subtype="EmiAPExo")
#' }

readMAgPIE<- function(subtype) {
  
  if (subtype == "EmiAirPoll") {
    x <- read.csv("emiAPexo.csv", row.names = 1)
    # reorder the data frame
    x$var   <- x$value
    x$value <- NULL
    # convert into a magpie object
    x <- as.magpie(x, datacol=6)
    
  } else if (subtype == "macBase") {
    x <- read.csv("macBaseMagpie.csv", row.names = 1)
    # reorder the data frame
    x$var   <- x$value
    x$value <- NULL
    # convert into a magpie object
    x <- as.magpie(x, datacol=6)
    
  } else if (subtype == "macBaseCO2luc") {
    x <- read.magpie("p_macBaseMagpie_co2luc_SSP2.cs4r")

  } else if (subtype == "co2tax") {
    x <- read.magpie("p_magpietax200.cs4r")

  } else if (subtype == "abatparam_co2") {
    x <- read.magpie("p_abatparam_CO2.cs4r")
    
  } else if (subtype == "MAgPIEReport_extensive") {
    
    # /p/projects/remind/runs/magpie_40-develop-2019-02-25-macfix/output/r8375-C_*/report.mif
    
    # !!! ATTENTION !!! 
    # Please update scenario names in calcMAgPIEReport.R and calcMacBaseLandUse.R ("none") if necessary

    file_list <- c("report-r8481-C_SSP1-Base.mif",
                   "report-r8481-C_SSP1-NDC.mif",
                   "report-r8481-C_SSP1-Budg1300.mif",
                   "report-r8481-C_SSP1-Budg600.mif",
                   "report-r8423-C_Base-mag-7.mif",
                   "report-r8423-C_Budg1300-mag-7.mif",
                   "report-r8423-C_Budg600-mag-7.mif",
                   "report-r8423-C_NDC-mag-7.mif")
    
    x <- NULL
    for(f in file_list) {
      x <- mbind(x,read.report(f,as.list = FALSE))
    }

  } else if (subtype == "supplyCurve_magpie_40") {
    regcode <- regionscode(toolMappingFile("regional",getConfig("regionmapping")))
    
    # !!! ATTENTION !!! 
    # Please update scenario names in calcBiomassPrice.R if necessary
    
    scenario_names <- c("f30_bioen_price_SSP1-BASE-Base_",
                        "f30_bioen_price_SSP1-NDC-NDC_",
                        "f30_bioen_price_SSP1-NDC-Budg1300_replaced_flat_",
                        "f30_bioen_price_SSP1-NDC-Budg600_",
                        "f30_bioen_price_SSP2-BASE-Base_",
                        "f30_bioen_price_SSP2-NDC-NDC_",
                        "f30_bioen_price_SSP2-NDC-Budg1300_replaced_flat_",
                        "f30_bioen_price_SSP2-NDC-Budg600_replaced_flat_",
                        "f30_bioen_price_SSP5-BASE-Base_replaced_flat_",
                        "f30_bioen_price_SSP5-NDC-NDC_replaced_flat_",
                        "f30_bioen_price_SSP5-NDC-Budg1300_replaced_flat_",
                        "f30_bioen_price_SSP5-NDC-Budg600_")

    file_list <- paste0(scenario_names,regcode,".cs4r")
    setnames  <- c("region","year","scenario","char")
    
    if(!all(file.exists(file_list))) {
      vcat(1,"Could not find ",file_list[!file.exists(file_list)],"\n")
      vcat(1,"MAgPIE supplycurve input is not available for all policy cases for the regioncode",regcode,".\nUsing fallback input files.\n")
      # if emulators with current regional resolution are not available for ALL scenarios, use fallback files
      regcode_H12 <- "690d3718e151be1b450b394c1064b1c5"
      file_list   <- paste0(scenario_names,regcode_H12,".cs4r")
      setnames    <- c("region_fallback","year","scenario","char")
    }

    x <- NULL
    for(f in file_list) {
      x <- mbind(x,read.magpie(f))
    }
    getSets(x) <- setnames

  } else {
    stop("Not a valid subtype!")
  }
  return(x)
}

