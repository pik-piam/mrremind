#' @title convertPRIMAPhist
#' @description function to convert PRIMAP-hist data to isocountry resolution
#'
#' @param x MAgPIE object
#'
#' @return MAgPIE object
#' 
#' @author Roman Popov
#'
#' @examples
#' \dontrun{
#' readSource("PRIMAPhist", subtype = "hist")
#' }
convertPRIMAPhist <- function(x){
  # convert to co2_c and n2o_n
  # change names of co2 and n2o for conversion (below)
  map1 <- c(ch4="ch4",         
            co2="co2_c",         
            fgasesar4="fgasesar4_co2eq_c",   
            fgases="fgases_co2eq_c",      
            hfcsar4="hfcsar4_co2eq_c",     
            hfcs="hfcs_co2eq_c",        
            kyotoghgar4="kyotoghgar4_co2eq_c",
            kyotoghg="kyotoghg_co2eq_c",    
            n2o="n2o_n",         
            pfcsar4="pfcsar4_co2eq_c",     
            pfcs="pfcs_co2eq_c",        
            sf6="sf6_co2eq_c")

  getNames(x,dim=3) <- map1[getNames(x,dim=3)]
  getNames(x,dim=4) <- c("Mt","Mt")
  x[,,"n2o_n"] <- x[,,"n2o_n"] / 1.5714
  carbon<-c("co2_c","fgasesar4_co2eq_c","fgases_co2eq_c","hfcsar4_co2eq_c",    
  "hfcs_co2eq_c","kyotoghgar4_co2eq_c","kyotoghg_co2eq_c","pfcsar4_co2eq_c",    
  "pfcs_co2eq_c","sf6_co2eq_c")  
  x[,,carbon] <- x[,,carbon] / 3.6667
  x=collapseNames(x)*0.001
  
  x <- toolCountryFill(x, fill = 0, no_remove_warning = c("ANNEXI", "ANT", "AOSIS", "BASIC", "EARTH", "EU28", "LDC", "NONANNEXI", "UMBRELLA"), verbosity = 2)
  x[is.na(x)] <- 0
  return(x)
}