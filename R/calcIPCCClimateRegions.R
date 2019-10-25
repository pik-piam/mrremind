#' @title calcClimateRegionsIPCC
#' @description calculates IPCC Climate Regions (IPCC2006 ch.4.3) based on t, ppt, pet from LPJml. elevation dimension not included for tropical montane class
#' 
#' @param landusetypes all or only one (to save computation memory)
#' @param cellular FALSE for country level, TRUE for cells
#' @param convert fills missing countries for country level aggregation with warm temperate moist (mostly small island nations) 
#' @param yearly FALSE for normal magpie 5 year time spans, TRUE for yearly 
#' 
#' @return Country or cellular magpie object with matrix of fraction of each climate region by country or cell 
#' @author David Chen
#' @seealso
#' \code{\link{readLPJml_rev21}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("IPCCClimateRegions")
#' }
#' @importFrom SPEI thornthwaite
#' @importFrom magpiesets findset


calcIPCCClimateRegions<-function(landusetypes="all", cellular=FALSE, yearly=FALSE, convert=TRUE){

#PET based on thornwaite function
p = readSource("LPJml_rev21","precipitation",convert = FALSE)
p<-toolCell2isoCell(p)
t = readSource("LPJml_rev21","temperature",convert = FALSE)
t<-toolCell2isoCell(t)
lat<-pe<-p
lat[,,]<-NA
pe[,,]<-NA
lat<-p[,1,1]
lat[,,]<-as.numeric(toolMappingFile(type = "cell",name = "CountryToCellMapping.csv",readcsv = TRUE)$lat)
lat<-setNames(setYears(lat,NULL),NULL)
tmp<-t
tmp<-aperm(tmp,c(3,2,1))
old<-tmp
dim(tmp)=c(12*dim(t)[2],dim(t)[1])
tmp<-thornthwaite(tmp,lat=lat[,,])
old[,,]<-tmp
pet<-as.magpie(aperm(old,c(3,2,1)))

#yearly total precip and pet
precip <- dimSums(p, dim=c(3.1))
pet1 <- dimSums(pet, dim=c(3.1))
#yearly MAP:PET ratio
ratio <-precip/(pet1+0.000000001)
#binary where ratio>1
ratio = ratio>1
#Mean Annual Temperature
temp <- dimSums(t, dim=3.1)/12
#if any month temp is above 10, binary TRUE
t_monthly <- t
t_monthly10 = t_monthly>10
t_monthly10 = dimSums(t_monthly10, dim=3) > 0

getNames(temp) <- "temp"
getNames(ratio) <- "ratio"
getNames(precip) <- "precip"
getNames(t_monthly10) <- "t_monthly10"
climate <- mbind(temp,precip,ratio, t_monthly10)
#climate zones, set to zero, note, no tropical montane climate (exists in IPCC classification)
climate <- add_columns(climate, dim=3.1, addnm=c("tropical_wet","tropical_moist","tropical_dry",
                                                 "warm_temp_moist","warm_temp_dry",
                                                 "cool_temp_moist","cool_temp_dry",
                                                 "boreal_moist", "boreal_dry",
                                                 "polar_moist","polar_dry"))
climate[,,c("tropical_wet","tropical_moist","tropical_dry",
            "warm_temp_moist","warm_temp_dry",
            "cool_temp_moist","cool_temp_dry",
            "boreal_moist", "boreal_dry",
            "polar_moist","polar_dry")] <- 0
#IPCC decision tree
climate[,,"tropical_wet"][which(climate[,,"temp"]>18 & climate[,,"precip"]>2000)]<- 1
climate[,,"tropical_moist"][which(climate[,,"temp"]>18 & climate[,,"precip"]<=2000 & climate[,,"precip"]>1000)] <-1
climate[,,"tropical_dry"][which(climate[,,"temp"]>18 & climate[,,"precip"]<=1000)] <- 1
climate[,,"warm_temp_moist"][which(climate[,,"temp"]>10 & climate[,,"temp"]<=18 & climate[,,"ratio"]==1)] <- 1
climate[,,"warm_temp_dry"][which(climate[,,"temp"]>10 & climate[,,"temp"]<=18 & climate[,,"ratio"]==0)] <- 1
climate[,,"cool_temp_moist"][which(climate[,,"temp"]>0 & climate[,,"temp"]<=10 & climate[,,"ratio"]==1)] <- 1
climate[,,"cool_temp_dry"][which(climate[,,"temp"]>0 & climate[,,"temp"]<=10 & climate[,,"ratio"]==0)] <- 1
climate[,,"boreal_moist"][which(climate[,,"temp"]<0 & climate[,,"t_monthly10"]==1 & climate[,,"ratio"]==1)] <- 1
climate[,,"boreal_dry"][which(climate[,,"temp"]<0 & climate[,,"t_monthly10"]==1 & climate[,,"ratio"]==0)] <- 1
climate[,,"polar_moist"][which(climate[,,"temp"]<0 & climate[,,"t_monthly10"]==0 & climate[,,"ratio"]==1)] <- 1
climate[,,"polar_dry"][which(climate[,,"temp"]<0 & climate[,,"t_monthly10"]==0 & climate[,,"ratio"]==0)] <- 1

climate_regions <- climate[,,c("tropical_wet","tropical_moist","tropical_dry",
                               "warm_temp_moist","warm_temp_dry",
                               "cool_temp_moist","cool_temp_dry",
                               "boreal_moist", "boreal_dry",
                               "polar_moist","polar_dry")]

if (cellular==TRUE){
  out <- climate_regions
}

else if (cellular==FALSE){
  CountryToCell  <- toolMappingFile(type="cell",name = "CountryToCellMapping.csv",readcsv = TRUE)
  
  if (landusetypes=="all"){
      landuse<-calcOutput("LanduseInitialisation",cellular=TRUE,aggregate=FALSE)
    }
  else if (landusetypes!="all"){
    landuse<-calcOutput("LanduseInitialisation",cellular=TRUE,aggregate=FALSE)[,,landusetypes]
  }  
#cell to country aggregation
landuse<-time_interpolate(landuse, interpolated_year = getYears(ratio),extrapolation_type = "constant")
landuse_sum<-dimSums(landuse,dim=3)
climate_regions1 <- toolAggregate(x = climate_regions, rel = CountryToCell, weight = landuse_sum, from = "celliso", to = "iso", partrel = TRUE)
out <- climate_regions1

if (convert==TRUE){
  out <- toolCountryFill(out,fill=0)
  missing <- setdiff(getRegions(out), getRegions(climate_regions1))
  out[missing,,"warm_temp_moist"] <-1
  }
}
      
if (yearly==FALSE){
  past<-findset("past")
  out <- out[,past,]
}


weight = calcOutput("LanduseInitialisation",cellular=FALSE,aggregate=FALSE)
weight<-time_interpolate(weight, interpolated_year = getYears(out),extrapolation_type = "constant")
if (landusetypes!="all"){weight=weight[,,landusetypes]}

return(list(x=out,
            weight=weight,
            unit="NULL",
            min=0,
            max=1,
            description="Proportion of IPCC Climate Region",
            isocountries=!cellular
            )
      )
}