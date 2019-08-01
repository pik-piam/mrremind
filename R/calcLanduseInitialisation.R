#' @title calcLanduseInitialisation
#' @description Calculates the cellular MAgPIE landuse initialisation area. Data from FAO on forestry is used to split the secondary forest pool of the LU2v1 dataset into forestry and secd_forest.
#'
#' @param cellular cellular (TRUE) or country-level/regional (FALSE) data? For country-level vs regional data, remember to set "aggregate" to false.
#' @param land new default land="fao", if land="new", the set land seperates primary and secondary forests, "old" uses the old land set (including only forest)
#' @param input_magpie uses just 1995 values and apply area fix (set cell with zero area to minimal value to not distrube aggregating to clusters)
#' @param selectyears default on "past"
#' @return List of magpie object with results on country or cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LanduseInitialisation")
#' }
#' @importFrom magclass setNames


calcLanduseInitialisation<-function(cellular=FALSE, land="fao", selectyears="past", input_magpie=FALSE){

  selectyears <- sort(findset(selectyears,noset = "original"))
  
  if (cellular==FALSE){
    
    LUH2v2<-calcOutput("LUH2v2",landuse_types="LUH2v2",irrigation=FALSE,selectyears=selectyears,cellular=FALSE,aggregate = FALSE)

    if(land%in%c("new","old")){
      
      # divide secondary forest into forestry and secdf.    
      forestry_country   <- readSource("FAO_FRA2015","fac")[,,"PlantFor"]
      forestry_country  <-  time_interpolate(forestry_country,interpolated_year = selectyears,integrate_interpolated_years = TRUE,extrapolation_type = "constant")
      vcat(verbosity = 3,"Forestry is interpolated for missing years and held constant for the period before FAO starts")
      
      secondary_forest <- LUH2v2[,,"secdf"]- setNames(forestry_country[,getYears(LUH2v2),],NULL)
      forestry_overflow<-secondary_forest
      forestry_overflow[forestry_overflow>0]<-0
      tmp<-dimSums(forestry_overflow,dim=1)
      if (any(tmp<0)){  
        vcat(verbosity = 2,paste("Mismatch of FAO forestry and Hurtt secondary forest:",paste(paste(getYears(tmp),round(tmp,0),"Mha, "),collapse = " "),". cut off."))
      } 
      secondary_forest[secondary_forest<0]=0
      forestry <- LUH2v2[,,"secdf"] - secondary_forest
      
      # calculate other landpools
      crop    <- dimSums(LUH2v2[,,c("c3ann","c4ann","c3per","c4per","c3nfx")],dim=3)
      pasture <- dimSums(LUH2v2[,,c("pastr","range")],dim=3)
      other   <- dimSums(LUH2v2[,,c("primn","secdn")],dim=3)
      
      if (land=="new"){
        out<-mbind(
          setNames(crop,"crop"),
          setNames(pasture, "past"),
          setNames(forestry, "forestry"),
          setNames(LUH2v2[,,c("primf")], "primforest"),
          setNames(secondary_forest, "secdforest"),
          setNames(LUH2v2[,,c("urban")], "urban"),
          setNames(other, "other"))
      } else if (land=="old"){
        out<-mbind(
          setNames(crop,"crop"),
          setNames(pasture, "past"),
          setNames(forestry, "forestry"),
          setNames(LUH2v2[,,c("primf")]+secondary_forest, "forest"),
          setNames(LUH2v2[,,c("urban")], "urban"),
          setNames(other, "other"))
      }
      
    } else if(land=="fao"){
      
      FAOforest <- calcOutput("ForestArea", selectyears=selectyears, aggregate = FALSE)
      
      other    <- dimSums(LUH2v2[,,c("primn","secdn")],dim=3)
      forest   <- dimSums(LUH2v2[,,c("primf","secdf")],dim=3)

      # Correct for overflow effects (FAO_forest greater than forest and other land available in LUH)
      overflow             <- FAOforest[,,"forest"] - (forest + other)
      overflow[overflow<0] <- 0
      if(any((of <- dimSums(overflow, dim=1)) > 0)){
        vcat(verbosity = 2,paste("Mismatch of FAO forest exceed LUH forest + other land by:",paste(paste(getYears(of),round(of,0),"Mha, "),collapse = " "),". FAO forest data will be cut."))
      }
      
      # corrected forest areas <- weight of forest subcategories * corrected total forest area
      FAOforest           <- FAOforest/setNames(FAOforest[,,"forest"], NULL) * setNames(FAOforest[,,"forest"] - overflow, NULL)
      FAOforest           <- toolNAreplace(FAOforest)$x
      
      # Correct other land as diff of FAO forest area and LUH forest area
      FAOother            <- other + forest - FAOforest[,,"forest"]
      
      # calculate other landpools
      crop    <- dimSums(LUH2v2[,,c("c3ann","c4ann","c3per","c4per","c3nfx")],dim=3)
      pasture <- dimSums(LUH2v2[,,c("pastr","range")],dim=3)
      
      out<-mbind(
        setNames(crop,"crop"),
        setNames(pasture, "past"),
        setNames(LUH2v2[,,c("urban")], "urban"),
        FAOforest[,,c("primforest","secdforest","forestry")],
        setNames(FAOother, "other"))
      
    }

  } else {
    
    if(land%in%c("new","old")){
      
      mapping   <- toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE) 
      countries <- unique(mapping$iso)
      if(is.null(countries)) stop("There must be something wrong with CountryToCellMapping.csv! No country information found!")
      
      # Load cellular and country data
      countrydata <- calcOutput("LanduseInitialisation",aggregate = FALSE,land=land, selectyears=selectyears ,cellular=FALSE)
      LUH2v2      <- calcOutput("LUH2v2", aggregate=FALSE, landuse_types="LUH2v2", irrigation=FALSE, cellular=TRUE, selectyears = selectyears)
      LUH2v2      <- toolCell2isoCell(LUH2v2)
      
      # divide secondary forest into forestry and secdf. 
      forestry_shr <- countrydata[,,"forestry"] / dimSums(countrydata[,,c("forestry","secdforest")],dim=3)
      forestry_shr[is.nan(forestry_shr)]<-0
      forestry_shr <- toolAggregate(x = forestry_shr[countries,,],rel = mapping,from="iso",to="celliso",dim=1)
      forestry <- forestry_shr * LUH2v2[,,"secdf"]
      secondary_forest <- (1-forestry_shr) * LUH2v2[,,"secdf"]
      
      # calculate other landpools
      crop    <- dimSums(LUH2v2[,,c("c3ann","c4ann","c3per","c4per","c3nfx")],dim=3)
      pasture <- dimSums(LUH2v2[,,c("pastr","range")],dim=3)
      other   <- dimSums(LUH2v2[,,c("primn","secdn")],dim=3)
      
      if (land=="new"){
        out<-mbind(
          setNames(crop,"crop"),
          setNames(pasture, "past"),
          setNames(forestry, "forestry"),
          setNames(LUH2v2[,,c("primf")], "primforest"),
          setNames(secondary_forest, "secdforest"),
          setNames(LUH2v2[,,c("urban")], "urban"),
          setNames(other, "other"))
      } else if (land=="old"){
        out<-mbind(
          setNames(crop,"crop"),
          setNames(pasture, "past"),
          setNames(forestry, "forestry"),
          setNames(LUH2v2[,,c("primf")]+secondary_forest, "forest"),
          setNames(LUH2v2[,,c("urban")], "urban"),
          setNames(other, "other"))
      }
      
    } else if(land=="fao"){
  
      out <- calcOutput("FAOForestRelocate", selectyears=selectyears, aggregate=FALSE)
    }
  }
  
  if(input_magpie){
    out      <- round(out,8) 
    cellArea <- dimSums(out,dim=3)
    temp     <- out[,,"other"]
    zero     <- which(cellArea==0,arr.ind = F)
    temp[zero] <- 10^-8
    out[,,"other"] <-temp
  }
  
  return(list(x=out,
              weight=NULL,
              unit="Mha",
              description="Land use initialisation data for different land pools",
              isocountries=!cellular)
  )
}
  
  
  
  
  
  

