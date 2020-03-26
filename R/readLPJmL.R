#' @title readLPJmL
#' @description Read LPJmL content
#' @param subtype Switch between different input
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens, Abhijeet Mishra, Felicitas Beier
#' @seealso
#' \code{\link{readLPJ}}
#' @examples
#'
#' \dontrun{
#' readSource("LPJmL", subtype="LPJmL5:CRU4p02.soilc", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom lpjclass readLPJ
#' @importFrom lucode path

readLPJmL <- function(subtype="LPJmL5:CRU4p02.soilc"){

  if(grepl("\\.",subtype)){
    
    subtype     <- strsplit(gsub(":", "/", subtype), split="\\.")
    folder      <- unlist(subtype)[1]
    subtype     <- unlist(subtype)[2]
    
  } else {
    
    natveg <- c("soilc", "soilc_layer", "litc", "vegc", "alitterfallc", 
    "transpiration", "discharge", "runoff", "evaporation")

    if(subtype %in% natveg){
    
      folder <- "LPJmL4/CRU_4"  
    } else{
      
      folder <- "LPJmL5/CRU_4"
    }
      
    cat(paste0("Set input folder to default climate data set: ", folder))
  }
  
  files <- c(soilc           = "soilc_natveg.bin",
             soilc_layer     = "soilc_layer_natveg.bin",
             litc            = "litc_natveg.bin",
             vegc            = "vegc_natveg.bin",
             alitfallc       = "alitfallc_natveg.bin",
             alitterfallc    = "alitterfallc_natveg.bin",
             alitfalln       = "alitfalln_natveg.bin",
             harvest         = "pft_harvest.pft.bin",
             irrig           = "cft_airrig.pft.bin",
             sdate           = "sdate.bin",
             hdate           = "hdate.bin",
             transpiration   = "mtransp_natveg.bin",
             discharge       = "mdischarge_natveg.bin",
             runoff          = "mrunoff_natveg.bin",
             evaporation     = "mevap_natveg.bin",
             mtranspiration  = "mtransp_natveg.bin",
             mdischarge      = "mdischarge_natveg.bin",
             mrunoff         = "mrunoff_natveg.bin",
             mevaporation    = "mevap_natveg.bin",
             vegc_grass      = "mean_vegc_mangrass.bin",
             litc_grass      = "litc_mangrass.bin",
             soilc_grass     = "soilc_mangrass.bin"
  )

  file_name <- toolSubtypeSelect(subtype,files)

  if(tmp <- file.exists(path(folder,"tmp.out"))){
  
    tmp        <- readLines(path(folder,"tmp.out"))
    years      <- as.numeric(unlist(regmatches(tmp, gregexpr("\\d{4}", tmp))))
    start_year <- years[1]
    years      <- seq(years[1],years[2],1)
  
  } else {
  
    # default
    start_year  <- 1901
    years      <- seq(start_year,2017,1)
  }

  unit_transform <-0.01               # Transformation factor gC/m^2 --> t/ha

  if(subtype%in%c("soilc","litc","vegc","alitfallc","alitterfallc","alitfalln","vegc_grass","litc_grass","soilc_grass")){

    start_year  <- start_year           # Start year of data set
    years       <- years                # Vector of years that should be exported
    nbands      <- 1                    # Number of bands in the .bin file
    avg_range   <- 1                    # Number of years used for averaging

    x <- readLPJ(
      file_name=path(folder,file_name),
      wyears=years,
      syear=start_year,
      averaging_range=avg_range,
      bands=nbands,
      soilcells=TRUE)

    x <- collapseNames(as.magpie(x))
    x <- x*unit_transform
    getNames(x) <- subtype

  } else if(grepl("*date*", subtype)){

    start_year  <- start_year           # Start year of data set
    years       <- years                # Vector of years that should be exported
    nbands      <- 24                   # Number of bands in the .bin file
    avg_range   <- 1                    # Number of years used for averaging

    x <- readLPJ(
      file_name=path(folder,file_name),
      wyears=years,
      syear=start_year,
      averaging_range=avg_range,
      bands=nbands,
      datatype=integer(),
      bytes=2,
      soilcells = TRUE,
      ncells = 67420)

    x <- collapseNames(as.magpie(x))

  } else if(subtype%in%c("soilc_layer")){

    start_year  <- start_year           # Start year of data set
    years       <- years                # Vector of years that should be exported
    nbands      <- 5                    # Number of bands in the .bin file
    avg_range   <- 1                    # Number of years used for averaging

    x <- readLPJ(
      file_name=path(folder,file_name),
      wyears=years,
      syear=start_year,
      averaging_range=avg_range,
      bands=nbands,
      soilcells=TRUE)

    x <- collapseNames(as.magpie(x))
    x <- x*unit_transform

    getNames(x)     <- paste0("soilc.",getNames(x))
    getSets(x)[4:5] <- c("data" ,"layer")

  } else if(grepl("transpiration|discharge|runoff|evaporation", subtype)){

    start_year  <- start_year         # Start year of data set
    years       <- years              # Vector of years that should be exported
    nbands      <- 12                 # Number of bands in the .bin file
    avg_range   <- 1                  # Number of years used for averaging

    x <- readLPJ(
      file_name=path(folder,file_name),
      wyears=years,
      syear=start_year,
      averaging_range = avg_range,
      monthly=TRUE,
      soilcells=TRUE)

    x <- collapseNames(as.magpie(x))
    x <- x*unit_transform
    
    if(grepl("layer", subtype)){
      subtype     <- gsub("_", "\\.", subtype)       # Expand dimension to layers
      getNames(x) <- paste0(subtype,".",getNames(x))
      getSets(x)[4:6]  <- c("data" ,"layer","month")
    } else{
      getNames(x) <- paste0(subtype,".",getNames(x))
      getSets(x)[4:5]  <- c("data" , "month")
    }
    
    # annual value as total over all month
    if(!grepl("^m", subtype)){
      x <- dimSums(x, dim="month")  
    }

  } else if(grepl("*harvest*", subtype)){

    start_year  <- start_year           # Start year of data set
    years       <- years                # Vector of years that should be exported
    nbands      <- 32                   # Number of bands in the .bin file
    avg_range   <- 1                    # Number of years used for averaging

    x <- readLPJ(
      file_name=path(folder,file_name),
      wyears=years,
      syear=start_year,
      averaging_range=avg_range,
      bands=nbands,
      soilcells=TRUE)

    # Transformation factor gC/m^2 --> t/ha
    yield_transform <- 0.01/0.45
    x <- collapseNames(as.magpie(x))
    x <- x*yield_transform

  } else if(subtype%in%c("irrig")){
    
    start_year  <- start_year           # Start year of data set
    years       <- years                # Vector of years that should be exported
    nbands      <- 32                   # Number of bands in the .bin file
    avg_range   <- 1                    # Number of years used for averaging
    
    x <- readLPJ(
      file_name=path(folder,file_name),
      wyears=years,
      syear=start_year,
      averaging_range=avg_range,
      bands=nbands,
      soilcells=TRUE,
      ncells=67420)
    
    x <- collapseNames(as.magpie(x))
    # Transform units (transform from: mm per year = liter per m^2 transform to: m^3 per ha)
      # 1 000 liter   = 1 m^3
      # 10 000 m^2    = 1 ha
      # 1 liter/m^2   = 10 m^3/ha
      # -> mm/yr * 10 = m^3/ha
    irrig_transform <- 10
    x[,,"irrigated"] <- x[,,"irrigated"]*irrig_transform # units are now: m^3 per ha per yr
    
  } else {stop(paste0("subtype ",subtype," is not existing"))}

  return(x)

}
