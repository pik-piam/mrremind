#' @title calc Capacity Factor
#' @description provides capacity factor values
#'
#' @return magpie object of the capacity factor data
#' @author Renato Rodrigues, Stephen Bi
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("CapacityFactor")
#' }
#' 

calcCapacityFactorHist <- function(){
  #mapping of remind technology names to IRENA categories
  rem_Irena_map <- data.frame(rem=c("hydro","wind","spv","csp","bioigcc","geohdr"),
                              irena= c("Hydropower","Wind","Solar photovoltaic","Concentrated solar power", "Bioenergy","Geothermal"))
  # Read capacity factor inputs
  hist_cap <- readSource(type="IRENA",subtype="Capacity")/1000 # converting from MW to GW
  hist_gen <- readSource("IRENA", subtype = "Generation")# Units are GWh
  # Calculate 2015 capacity factor for relevant technologies  
  cf_realworld <- hist_gen[,2015,rem_Irena_map$irena]/(8760*hist_cap[,2015,rem_Irena_map$irena]) 
  #rename
  getNames(cf_realworld) <- rem_Irena_map$rem
  #check data
  max(cf_realworld[,,"hydro"],na.rm = T)
  max(cf_realworld[,,"wind"],na.rm = T) #INF spm, >1 AZE
  max(cf_realworld[,,"spv"],na.rm = T)
  max(cf_realworld[,,"csp"],na.rm = T)
  max(cf_realworld[,,"bioigcc"],na.rm = T) #>1 CHL, JPN, POL 
  max(cf_realworld[,,"geohdr"],na.rm = T)
  
  
  
  #correct SPM infinite value
  cf_realworld[is.infinite(cf_realworld)] <- 0.8
  #correct AZE,CHL,JPN,POL >1 value
  cf_realworld[cf_realworld > 1] <- 0.8
  #get rid of NAs
  cf_realworld[is.na(cf_realworld)] <- 0
  
  
  #weight: historic generation
  hist_gen <- hist_gen[,2015,rem_Irena_map$irena]
  getNames(hist_gen) <- rem_Irena_map$rem
  hist_gen[is.na(cf_realworld)] <- 0
  
  # Read coal capacity and generation data to derive historical capacity factor rules
  # Read generation data from Energy Balances
  coalgen_c <- calcOutput("IO",subtype="output",aggregate = F)[,2003:2015,"pecoal.seel"]
  coalgen_c <- dimSums(coalgen_c,dim=3)
  
  map <- toolGetMapping("regionmappingH12.csv",type="regional")
  
  coalgen_R <- toolAggregate(coalgen_c,map,weight=NULL)
  getNames(coalgen_c) <- "pc"
  getNames(coalgen_R) <- "pc"
  
  #Read coal capacity data from GCPT
  hist_cap_coal_c <- readSource("GCPT",subtype="historical")
  hist_cap_coal_R <- toolAggregate(hist_cap_coal_c,rel=map,weight=NULL)
  
  #Calculate historical 5-year average capacity factors by country
  coal_factor_c <- new.magpie(getRegions(hist_cap_coal_c),years = c(2005,2010,2015),names="pc",fill=0)
  coal_factor_R <- new.magpie(getRegions(hist_cap_coal_R),years = c(2005,2010,2015),names="pc",fill=0)
  for (i in c(2005,2010,2015)) {
    if (paste0("y",(i+2)) %in% getYears(coalgen_c)) {
      coal_factor_c[,i,] <- dimSums(coalgen_c[,(i-2):(i+2),],dim=2)/(dimSums(hist_cap_coal_c[,(i-2):(i+2),],dim=2)*365*24/277777.77778)
      coal_factor_R[,i,] <- dimSums(coalgen_R[,(i-2):(i+2),],dim=2)/(dimSums(hist_cap_coal_R[,(i-2):(i+2),],dim=2)*365*24/277777.77778)
      #coal_factor_c[,i,][which(dimSums(hist_cap_coal_c[,(i-2):(i+2),],dim=2)==0)] <- 0
    }else {
      coal_factor_c[,i,] <- dimSums(coalgen_c[,(i-2):i,],dim=2)/(dimSums(hist_cap_coal_c[,(i-2):i,],dim=2)*365*24/277777.77778)
      coal_factor_R[,i,] <- dimSums(coalgen_R[,(i-2):i,],dim=2)/(dimSums(hist_cap_coal_R[,(i-2):i,],dim=2)*365*24/277777.77778)
      #coal_factor_c[,i,][which(hist_cap_coal_c[,i,]==0)] <- 0
    }
    #Replace countries without coal power with regional capacity factor. Better than 0 values for future estimates (???)
    coal_factor_c[,i,][which(!is.finite(coal_factor_c[,i,]))] <- 
      coal_factor_R[map$RegionCode[which(map$CountryCode %in% getRegions(coal_factor_c[which(!is.finite(coal_factor_c[,i,]))]))],i,]
    # The derived coal capacity factors for Bosnia, Ireland and Myanmar are > 1 in some years for as yet unclear reasons.
    coal_factor_c[,i,][which(coal_factor_c[,i,]>1)] <- 1
  }
  
  cf_realworld <- mbind(cf_realworld,coal_factor_c)
  hist_gen <- mbind(hist_gen,coalgen_c[,c(2005,2010,2015),])
  
  return(list(x=cf_realworld, weight=hist_gen,
               unit="% of capacity", 
               description="Installed capacity availability in 2015 - capacity factor (fraction of the year that a plant is running)"              
  ))
  
}

