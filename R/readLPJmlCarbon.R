#' @title readLPJmlCarbon
#' @description Read LPJmL contant
#' 
#' @param subtype Input name coming from calcLPJmlCarbon
#' 
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @seealso
#' \code{\link{readLPJmlCarbon}},
#' @examples
#' 
#' \dontrun{ 
#' readSource("LPJmlCarbon", subtype="historical.nat_veg.soilc_layer", convert="onlycorrect")
#' }
#'
#' @importFrom magclass read.magpie
#' @importFrom lpjclass readLPJ
#' @importFrom lpjclass read.LPJ_input 

readLPJmlCarbon<-function(subtype="historical.nat_veg.soilc_layer"){
  
  
  subtype     <- gsub("\\*","/" ,subtype)
  file_name   <- paste0(gsub("\\.", "/", subtype),".bin")
  type_raw    <- subtype
  subtype     <- gsub("^.+\\.", "", type_raw)
  landtype    <- gsub("^.+\\.", "", gsub(paste0(".",subtype),"",type_raw))
  climatetype <- gsub(paste0(".",landtype,".",subtype),"",type_raw)

  
  LPJmL2MAgPIE <- c(lu_maize_nres_if = "crop",
                    lu_maize_nres_rf = "crop",
                    lu_maize_wres_if = "crop",
                    lu_maize_wres_rf = "crop",
                    nat_veg          = "natveg",
                    lu_grass         = "past")
  
  landtype <- toolSubtypeSelect(landtype,LPJmL2MAgPIE)
  
  if(grepl("historical",climatetype)){
    
    start_year  <- 1901                
    years       <- 1951:2009 
    
  } else if(grepl("IPSL_CM5A_LR",climatetype)){
    
    start_year  <- 1951                
    years       <- 1951:2099 
    
  } else {stop(paste0("start_year and range of ",climatetype," unknown"))}
  
  
  
  if(subtype%in%c("soilc","litc","vegc","alitfallc")){
    
    start_year  <- start_year           #Start year of data set
    years       <- years                #Vector of years that should be exported          
    nbands      <- 1                    # Number of bands in the .bin file
    avg_range   <- 1                    #Number of years used for averaging
   
    unit_transform <-0.01               # Transformation factor gC/m^2 --> t/ha
    
    x <- readLPJ(
      file_name=file_name,
      wyears=years,
      syear=start_year,
      averaging_range=avg_range,
      bands=nbands,
      soilcells=TRUE)
    
    x <- collapseNames(as.magpie(x))
    x <- x*unit_transform
    getNames(x) <- subtype
    x <- add_dimension(x, dim = 3.1, add = "climate.landuse", nm = paste0(climatetype,".",landtype))
    
  
  } else if(subtype%in%c("soilc_layer")){
    
    start_year  <- start_year           #Start year of data set
    years       <- years                #Vector of years that should be exported          
    nbands      <- 5                    # Number of bands in the .bin file
    avg_range   <- 1                    #Number of years used for averaging
    
    unit_transform <-0.01               # Transformation factor gC/m^2 --> t/ha
    
    x <- readLPJ(
      file_name=file_name,
      wyears=years,
      syear=start_year,
      averaging_range=avg_range,
      bands=nbands,
      soilcells=TRUE)
    
    x <- collapseNames(as.magpie(x))
    x <- x*unit_transform
    
    getNames(x)     <- paste0("soilc.",getNames(x)) 
    getSets(x)[4:5] <- c("data" ,"layer")
    
    x <- add_dimension(x, dim = 3.1, add = "climate.landuse", nm = paste0(climatetype,".",landtype))
    
  } else if (grepl("m*", subtype)){
    
    start_year  <- start_year          #Start year of data set
    years       <- years               #Vector of years that should be exported          
    nbands      <- 12                   #Number of bands in the .bin file
    avg_range   <- 1                    #Number of years used for averaging
    
    unit_transform <-0.01               #Transformation factor gC/m^2 --> t/ha
    
    x <- readLPJ(
      file_name=file_name, 
      wyears=years, 
      syear=start_year,
      averaging_range = avg_range,
      monthly=TRUE, 
      soilcells=TRUE)
    
    x <- collapseNames(as.magpie(x))
    x <- x*unit_transform
    
    if(grepl("layer", subtype)){
      subtype     <- gsub("_", "\\.", subtype)                     # Expand dimension to layers
      getNames(x) <- paste0(subtype,".",getNames(x)) 
      getSets(x)[4:6]  <- c("data" ,"layer","month") 
    } else{             
      getNames(x) <- paste0(subtype,".",getNames(x)) 
      getSets(x)[4:5]  <- c("data" , "month") 
    }
    
    x <- add_dimension(x, dim = 3.1, add = "climate.landuse", nm = paste0(climatetype,".",landtype))
    
  } else {stop(paste0("subtype ",subtype," is not existing"))}
  
  x<-clean_magpie(x)
  
  return(x)
}  