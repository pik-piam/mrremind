#' @title readLPJml_rev21
#' @description Read LPJmL contant
#' @param subtype Switch between diffrent input
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{readLPJml_rev21}},
#' @examples
#' 
#' \dontrun{ 
#' readSource("LPJml_rev21", subtype="maize_mrh")
#' }
#'
#' @importFrom magclass read.magpie
#' @importFrom lpjclass readLPJ
#' @importFrom lpjclass read.LPJ_input 
#' @importFrom lpjclass as.lpj 
#' @importFrom utils tail data

readLPJml_rev21<-function(subtype="soilc"){
  
  files <- c(soilc="soilc_natveg.bin",
             soilc_layer="soilc_layer_natveg.bin",
             litc="litc_natveg.bin",
             vegc="vegc_natveg.bin",
             cshare="cshare_released_0.5.mz",
             calibrated_area="calibrated_area_0.5.mz",
             koeppen="koeppen_geiger_0.5.mz",
             harvest_lai4="pft_harvest.pft_lai4.bin",
             transpiration="mtransp.bin",
             evaporation="mevap.bin",
             precipitation="cru_ts3.21.1901.2012.pre.clm",
             temperature="cru_ts3.21.1901.2012.tmp.clm",
             soilc_maiz_rf="soilc.mai.wresidues.rf.bin",
             soilc_maiz_ir="soilc.mai.wresidues.ir.bin",
             soilc_soy_rf="soilc.soy.wresidues.rf.bin",
             soilc_soy_ir="soilc.soy.wresidues.ir.bin",
             soilc_grass_rf="soilc.grass.bin",
             litc_maiz_rf="litc.mai.wresidues.rf.bin",
             litc_maiz_ir="litc.mai.wresidues.ir.bin",
             litc_soy_rf="litc.soy.wresidues.rf.bin",
             litc_soy_ir="litc.soy.wresidues.ir.bin",
             litc_grass_rf="litc.grass.bin",
             mrh_maiz_rf="mrh.mai.wresidues.rf.bin",
             mrh_maiz_ir="mrh.mai.wresidues.ir.bin",
             mrh_soy_rf="mrh.soy.wresidues.rf.bin",
             mrh_soy_ir="mrh.soy.wresidues.ir.bin",
             mrh_grass_rf="mrh.grass.bin",
             cftshare="cftfrac.bin",
             cftshareInput="lpjLUout.bin",
             cellarea = "grid.bin",
             oceanshare = "oceanfrac.bin",
             lakeshare = "glwd_lakes_and_rivers.bin",
             soilc_pnv = "soilc_pnv.bin",
             soilc_lu = "soilc_lu.bin",
             soilc_hist = "soilc_hist.bin"
             )
  
  file <- toolSubtypeSelect(subtype,files)
  
  if (subtype%in%c("soilc","litc","vegc")){
    
    start_year  <- 1901                 #Start year of data set
    years       <- 1901:2095        #Vector of years that should be exported          
    nbands      <- 1                     # Number of bands in the .bin file
    avg_range   <- 1                     #Number of years used for averaging
    # Transformation factor gC/m^2 --> t/ha
    unit_transform <-0.01
    
    x<-readLPJ(
        file_name=file,
        wyears=years,
        syear=start_year,
        averaging_range=avg_range,
        bands=nbands,
        soilcells=TRUE)
    
    x<-collapseNames(as.magpie(x))
    getNames(x)<-subtype
    x<-x*unit_transform
  
  } else if (subtype%in%c("harvest_lai4")){
      
      start_year  <- 1950                 #Start year of data set
      years       <- 1961:2010        #Vector of years that should be exported          
      nbands       <-32                     # Number of bands in the .bin file
      avg_range   <- 8                     #Number of years used for averaging
      # Transformation factor gC/m^2 --> t/ha
      unit_transform <-0.01/0.45
      
      x<-readLPJ(
        file_name=file,
        wyears=years,
        syear=start_year,
        averaging_range=avg_range,
        bands=nbands,
        soilcells=TRUE)
      
      x<-collapseNames(as.magpie(x))
      x<-x*unit_transform    
    
  } else if (subtype%in%c("soilc_layer")){
    
    start_year  <- 1951                 #Start year of data set
    years       <- 1951:2095        #Vector of years that should be exported          
    nbands       <-5                     # Number of bands in the .bin file
    avg_range   <- 1                     #Number of years used for averaging
    # Transformation factor gC/m^2 --> t/ha
    unit_transform <-0.01
    
    x<-readLPJ(
      file_name=file,
      wyears=years,
      syear=start_year,
      averaging_range=avg_range,
      bands=nbands,
      soilcells=TRUE)
    
    # Anders als soilc.bin hat das 5 layer, also die ersten 67420 Werte sind für layer 1 (0-200mm) und Jahr 1 (1951), die nächsten 67420 für layer 2 (201-500mm) und Jahr eins, dann layer 3 (501-1000mm), dann layer 4 (1001-2000mm), dann layer 5 (2001-3000mm), danach dann layer 1 für das 2. Jahr etc.
    # Wir benötigen nur die obersten 2 layer
    x<-x[,,1:2,1]
    x<-collapseNames(as.magpie(x))
    getNames(x)<-c("mm0_200","mm201_500")
    x<-x*unit_transform
    
  } else if (subtype%in%c("transpiration","evaporation")){
    
    past<-findset("past")
    start_year  <- 1901                 #Start year of data set
    years       <- as.numeric(substring(past,2))  #Vector of years that should be exported          
    nbands        <-12                     # Number of bands in the .bin file
    avg_range   <- 1                     #Number of years used for averaging
    ncells=67420
    # unit: liter/square meter
    
    x<-readLPJ(
      file_name=file,
      wyears=years,
      syear=start_year,
      averaging_range=avg_range,
      bands=nbands,
      ncells=ncells,
      soilcells=TRUE)
    x<-collapseNames(as.magpie(x))
    
  } else if (subtype%in%c("precipitation","temperature")){
   
     x<-read.LPJ_input(
      file_name=file,
      out_years=paste0("y",1951:2012))
    dimnames(x)[[3]]<-c("jan","feb","mar","apr","mai","jun","jul","aug","sep","oct","nov","dez")
    x<-collapseNames(as.magpie(x))
    
  } else if (subtype%in%c("cshare","koeppen","calibrated_area")){
   
     x<-read.magpie(file) 
     
  } else if (grepl("soilc_[ms]|litc_[ms]", subtype)){
    
    start_year  <- 1951                 #Start year of data set
    years       <- 1951:2009            #Vector of years that should be exported          
    nbands      <- 1                    #Number of bands in the .bin file
    avg_range   <- 1                    #Number of years used for averaging
    unit_transform <-0.01               #Transformation factor gC/m^2 --> t/ha
    
    x <- readLPJ(
      file_name=file,
      wyears=years,
      syear=start_year,
      averaging_range=avg_range,
      bands=nbands,
      soilcells=TRUE)
    
    x <- collapseNames(as.magpie(x))
    x <- x*unit_transform
    getNames(x) <- subtype
    
  } else if (grepl("soilc_grass|litc_grass", subtype)){
    
    start_year  <- 1901                 #Start year of data set
    years       <- 1951:2009            #Vector of years that should be exported          
    nbands      <- 1                    #Number of bands in the .bin file
    avg_range   <- 1                    #Number of years used for averaging
    unit_transform <-0.01               #Transformation factor gC/m^2 --> t/ha
    
    x <- readLPJ(
      file_name=file,
      wyears=years,
      syear=start_year,
      averaging_range=avg_range,
      bands=nbands,
      soilcells=TRUE)
    
    x <- collapseNames(as.magpie(x))
    x <- x*unit_transform
    getNames(x) <- subtype
    
  } else if (grepl("mrh_[ms]", subtype)){
    
    start_year  <- 1951                 #Start year of data set
    years       <- 1951:2009            #Vector of years that should be exported          
    nbands      <- 12                   #Number of bands in the .bin file
    avg_range   <- 1                    #Number of years used for averaging
    unit_transform <-0.01               #Transformation factor gC/m^2 --> t/ha   
    
    x <- readLPJ(
      file_name=file, 
      wyears=years, 
      syear=start_year,
      averaging_range = avg_range,
      monthly=TRUE, 
      soilcells=TRUE)
    
    x <- collapseNames(as.magpie(x))
    x <- x*unit_transform
    #getNames(x) <- subtype
    
  } else if (grepl("mrh_grass", subtype)){
    
    start_year  <- 1901                 #Start year of data set
    years       <- 1951:2009            #Vector of years that should be exported          
    nbands      <- 12                   #Number of bands in the .bin file
    avg_range   <- 1                    #Number of years used for averaging
    unit_transform <-0.01               #Transformation factor gC/m^2 --> t/ha
    
    x <- readLPJ(
      file_name=file, 
      wyears=years, 
      syear=start_year,
      averaging_range = avg_range,
      monthly=TRUE, 
      soilcells=TRUE)
    
    x <- collapseNames(as.magpie(x))
    x <- x*unit_transform
    #getNames(x) <- subtype
 
   } else if (subtype=="cellarea"){
      
      unit_transform <- 10^-10
      ncells         <- 67420
      
      ### Read input
      #zz    <- file(paste(file,sep=""),"rb")
      #seek(zz, where = 43, origin="start")
      x     <- readBin(con=file, what=integer(), n=2*ncells, size=2)/100
      
      ### Calculate area
      lon   <- x[c(1:ncells)*2-1]
      lat   <- x[c(1:ncells)*2]
      x     <- array((111194.9*0.5)*(111194.9*0.5)*cos(lat/180*pi), dim=c(67420,1,1,1))
      
      ### Name spatial dimension
      lpjclassdata <- NULL
      data("lpjclassdata", envir = environment(), package="lpjclass")
      land <- lpjclassdata$cellbelongings[,c("LPJ.Index","country.code")]
      x <- x[which(lpjclassdata$grid_67420_59199==1),,,,drop=FALSE]
      dimnames(x)[[1]] <- paste(land$countryname,1:59199,sep=".")
      
      ### Transform to magpie-pbject
      x           <- as.magpie(as.lpj(x))
      
      getNames(x) <- subtype
      x           <- x*unit_transform
      
    } else if (subtype%in%c("oceanshare","lakeshare")){
      
      unit_transform <- 10^-2
      ncells         <- 67420
      
      if(subtype=="oceanshare") {
        header_bytes <- 43
      } else if(subtype=="lakeshare"){
        header_bytes <- 48
      }
      
      
      ### Read input
      zz    <- file(paste(file,sep=""),"rb")
      seek(zz, where = header_bytes, origin="start")
      x     <- array(readBin(con=zz, what=integer(), n=ncells, size=1), dim=c(67420,1,1,1))
      
      ### Name spatial dimension
      lpjclassdata <- NULL
      data("lpjclassdata", envir = environment(), package="lpjclass")
      land <- lpjclassdata$cellbelongings[,c("LPJ.Index","country.code")]
      x <- x[which(lpjclassdata$grid_67420_59199==1),,,,drop=FALSE]
      dimnames(x)[[1]] <- paste(land$countryname,1:59199,sep=".")
      
      ### Transform to magpie-pbject
      
      x           <- as.magpie(as.lpj(x))
      
      getNames(x) <- subtype
      x           <- x*unit_transform
      
      
    } else if (subtype%in%c("cftshare")){
      
      start_year  <- 2015                 #Start year of data set
      years       <- 2015:2100        #Vector of years that should be exported          
      nbands      <- 32                     # Number of bands in the .bin file
      avg_range   <- 1                     #Number of years used for averaging
      ncells      <- 67420
      
      x<-readLPJ(
        file_name=file,
        wyears=years,
        syear=start_year,
        averaging_range=avg_range,
        bands=nbands,
        ncells=ncells,
        soilcells=TRUE)
      
      x<-collapseNames(as.magpie(x))
      
    } else if (subtype%in%c("cftshareInput")){
      
      out_years  <- "y2015"                 #Start year of data set
      
  
      x<-read.LPJ_input(
        file_name=file,
        out_years=out_years)
      
      x<-collapseNames(as.magpie(x))
      
      band_names_cfts <- c("tece","rice","maize","trce","pulses","tero","trro","sunflower",
                           "soybean","groundnut","rapeseed","sugarcane","others","mgrass","begr","betr")
      
      irrigation_type <- c("rainfed","irrigated_surface","irrigated_sprinkler","irrigated_drip")
      
      out <- NULL
      
      for(i in c(1:4)){
        
        toAdd <- x[,,c(((i-1)*16+1):(i*16))]
        getNames(toAdd) <- band_names_cfts
        toAdd <- add_dimension(toAdd, add="irrigationtype", nm=irrigation_type[i], dim=3.2)
        
        out <- mbind(out, toAdd)
      }
      
      x <- out
      
    } else if (subtype%in%c("soilc_lu")){
      
      start_year  <- 2015                 #Start year of data set
      years       <- 2015:2100            #Vector of years that should be exported          
      nbands      <- 1                   # Number of bands in the .bin file
      avg_range   <- 1                    #Number of years used for averaging
      ncells      <- 67420
      # Transformation factor gC/m^2 --> t/ha
      unit_transform <-0.01
      
      x<-readLPJ(
        file_name=file,
        wyears=years,
        syear=start_year,
        averaging_range=avg_range,
        bands=nbands,
        ncells=ncells,
        soilcells=TRUE)
      
      x<-collapseNames(as.magpie(x))
      getNames(x)<-subtype
      x<-x*unit_transform
    } else if (subtype%in%c("soilc_pnv")){
      
      start_year  <- 1901                 #Start year of data set
      years       <- 1901:2100            #Vector of years that should be exported          
      nbands      <- 1                   # Number of bands in the .bin file
      avg_range   <- 1                    #Number of years used for averaging
      ncells      <- 67420
      # Transformation factor gC/m^2 --> t/ha
      unit_transform <-0.01
      
      x<-readLPJ(
        file_name=file,
        wyears=years,
        syear=start_year,
        averaging_range=avg_range,
        bands=nbands,
        ncells=ncells,
        soilcells=TRUE)
      
      x<-collapseNames(as.magpie(x))
      getNames(x)<-subtype
      x<-x*unit_transform
    
    } else if (subtype%in%c("soilc_hist")){
      
      start_year  <- 1901                 #Start year of data set
      years       <- 1901:2015            #Vector of years that should be exported          
      nbands      <- 1                   # Number of bands in the .bin file
      avg_range   <- 1                    #Number of years used for averaging
      ncells      <- 67420
      # Transformation factor gC/m^2 --> t/ha
      unit_transform <-0.01
      
      x<-readLPJ(
        file_name=file,
        wyears=years,
        syear=start_year,
        averaging_range=avg_range,
        bands=nbands,
        ncells=ncells,
        soilcells=TRUE)
      
      x<-collapseNames(as.magpie(x))
      getNames(x)<-subtype
      x<-x*unit_transform    
      
  } else {stop(paste0("subtype ",subtype," is not existing"))} 

  
  dimnames(x)[[1]] <- paste0("GLO.",1:dim(x)[1])
  x<-clean_magpie(x)
  
  
  return(x)
}  