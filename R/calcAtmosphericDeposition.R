#' @title calcAtmosphericDeposition
#' @description Conputes Atmospheric (nitrogen) deposition on different land-use types. It distinguishes ammonia (Nh3) and Nitrogen oxides (NOx) as well 
#' @param datasource deposition inventory
#' @param cellular cellular or country level emissions
#' @param emission if TRUE, not the depositio but the cellular emissions are reported
#' @param scenario if dataset contains several scenarios (e.g. ACCMIP), one scenario can be selected.
#' @param glo_incl_oceans provides global values that include oceans, as ocenas are not part of the country mapping
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcAtmosphericDepositionRates}},
#' \code{\link{calcNitrogenBudgetCropland}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("AtmosphericDeposition")
#' }
#' 

calcAtmosphericDeposition<-function(datasource="ACCMIP",glo_incl_oceans=FALSE,cellular=FALSE,emission=FALSE,scenario=NULL){
  luhdata<-calcOutput("LanduseInitialisation",cellular=TRUE,aggregate=FALSE)
  if(is.null(scenario)){scenario="rcp45"}
  
  if (datasource%in%c("ACCMIP")){
    ACCMIP = calcOutput("ACCMIP",glo_incl_oceans=glo_incl_oceans,aggregate = FALSE)
    #weight=setYears(collapseNames(readSource("ACCMIP",subtype="nhx_1850",convert = FALSE)[,,"area"]))
    if(emission==FALSE){
      ACCMIP2<-add_dimension(dimSums(ACCMIP[,,c("drydep","wetdep")][,,c("nh3_n","no2_n")],dim=3.2),dim=3.2,nm = "deposition")  
    } else {
      ACCMIP2<-ACCMIP[,,c("emi")][,,c("nh3_n","no2_n")]
    }
    
    time<-findset("time")
    ACCMIP2<-time_interpolate(ACCMIP2,interpolated_year = time,integrate_interpolated_years = FALSE,extrapolation_type = "constant")
    
    if(glo_incl_oceans==FALSE){
      vcat(2,"using constant landuse patterns for future deposition. Does not affect model results as they will be scaled with area lateron")
      luhdata2 <- toolHoldConstantBeyondEnd(luhdata)
      if(emission){luhdata2<-dimSums(luhdata2,dim=3)}
      out<-luhdata2*ACCMIP2
    } else {
      out<-ACCMIP2
    }
    if ((cellular==FALSE)&(glo_incl_oceans==FALSE)){
      mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)  
      out<-groupAggregate(out,query = mapping, dim = 1,from="celliso",to="iso")
      out  <- toolCountryFill(out,fill=0,verbosity = 2) 
    }
    out<-out[,,scenario]
  } else if (datasource=="Dentener"){
    if (emission==TRUE){stop("option 'emission' so far only for ACCMIP")}
    dentener<-readSource("Dentener",convert = FALSE)
    dentener<-dentener[,"y1993",]
    # wschl fehler in y1860 china! 
    
    vcat(verbosity=1,paste(round(sum(dentener<0)/length(dentener)*100,2)," % of data points with negative values in Dentener. set to 0."))
    dentener[dentener<0]<-0
    # not taking the natural rates from 1860, because they seem buggy. numbers do not add up.
    #deposition<-add_dimension(setYears(dentener[,"y1860",],"y1993"),dim = 3.1,add = "source",nm = "natural")
    #deposition<-add_columns(deposition,dim=3.1,addnm = "anthropogenic")
    #deposition[,,"anthropogenic"]<-dentener[,"y1993",]-deposition[,"y1993","natural"]
    
    #vcat(verbosity=1,paste(round(sum(deposition[,,"anthropogenic"]<0)/length(deposition[,,"anthropogenic"])*100,2)," % of additional negative numbers after split in anthropogenic and natural. set to 0."))
    #deposition[deposition<0]<-0
    
    # use deposition rates of 1993 for 1995
    dep<-luhdata[,"y1995",]*setYears(dentener,"y1995")
    
    out<-dep[,,c("no2_n","nh3_n")]
    out<-add_dimension(out,dim =3.2,nm = "history")
    out<-add_dimension(out,dim =3.3,nm = "deposition")
    
    if (cellular==FALSE){
      mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)  
      out<-groupAggregate(out,query = mapping, dim = 1,from="celliso",to="iso")
      out  <- toolCountryFill(out,fill=0)
    }
    
  } else {
    emi<-calcOutput("EmissionInventory",aggregate = FALSE,datasource=datasource,mapping=NULL)
    emi<-dimSums(emi[,,c("nh3_n","no2_n")],dim=3.1)
    if(glo_incl_oceans==TRUE){
      out=dimSums(emi,dim=c(1))
    } else {
      redep_share<-calcOutput("AtmosphericRedepositionShare",scenario=scenario,aggregate = FALSE)[,getYears(emi),]
      transboundary_redep_share<-calcOutput("AtmosphericTransboundaryRedepositionShare",scenario=scenario,aggregate = FALSE)[,getYears(emi),]
      
      domestic=emi*redep_share
      transboundary=dimSums(emi*(1-dimSums(redep_share,dim=3.1)),dim=1)*transboundary_redep_share
      out=collapseNames(domestic+transboundary)
      out<-dimOrder(out,perm = c(2,1))
      if(cellular) {
        weight = calcOutput("AtmosphericDeposition",datasource="ACCMIP",glo=FALSE,cellular=TRUE,emission=FALSE,scenario=NULL,aggregate = FALSE)
        mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)
        mapping <- mapping[which(mapping$iso%in%getRegions(weight)),]
        
        warning("the following section can be removed when toolAggregate is bugfixed")
        weight<-collapseNames(weight)
        out=out[getRegions(weight),,]
        getCells(weight) <-gsub(pattern = "\\.",replacement = "_",x = getCells(weight))
        weight<-clean_magpie(weight)
        mapping$celliso<-gsub(pattern = "\\.",replacement = "_",x = mapping$celliso)
        ### till here.
        
        out<-toolAggregate(x=out,rel=mapping,weight=weight[,getYears(out),],from="iso",to="celliso")
        getCells(out) <-gsub(pattern = "_",replacement = "\\.",x = getCells(out))
        
      }
    }

    out<-add_dimension(out,dim =3.2,nm = "history")
    out<-add_dimension(out,dim =3.3,nm = "deposition")
  }
  
  if (any(out < -1e-08)) {
    warning("very negative numbers. Check")
  }
  out[out<0]=0
  
  return(list(
    x=out,
    weight=NULL,
    unit="Mt Nr, NH3N and NO2N",
    isocountries=(!cellular & (nregions(out)!=1)),
    min=0,
    max=200,
    description="Atmospheric deposition, natural (1870 levels) and anthropogenic in the year 1995 (actually 1993) for different landuse classes."))
}