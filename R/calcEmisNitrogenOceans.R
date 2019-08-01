#' @title calcEmisNitrogenOceans
#' @description Conputes (nitrogen) emissions of Oceans.
#' @param method deposition inventory
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcAtmosphericDeposition}},
#' \code{\link{calcNitrogenBudgetCropland}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EmisNitrogenOceans")
#' }
#' 

calcEmisNitrogenOceans<-function(method="ACCMIP"){
  
  if (method=="ACCMIP"){
    emi<-collapseNames(calcOutput("AtmosphericDeposition",datasource="ACCMIP",glo_incl_oceans=FALSE,cellular=FALSE,emission=TRUE,aggregate = FALSE))
    emi_glo<-collapseNames(calcOutput("AtmosphericDeposition",datasource="ACCMIP",glo_incl_oceans=TRUE,cellular=FALSE,emission=TRUE,aggregate = FALSE))
    out<-emi
    out[,,]<-0
    out["ATA",,]<-emi_glo-dimSums(emi,dim=1)
    out<-add_columns(out,dim=3.1,addnm = c("n2o_n_direct","accumulation"))
    out[,,c("n2o_n_direct","accumulation")]=0
    # add global estimates for N2O and N burial from Voss et al (2013)
    # Voss, M., H. W. Bange, J. W. Dippner, J. J. Middelburg, J. P. Montoya, and B. Ward. 2013. "The Marine Nitrogen Cycle: Recent Discoveries, Uncertainties and the Potential Relevance of Climate Change." Philosophical Transactions of the Royal Society B: Biological Sciences 368 (1621): 20130121–20130121. doi:10.1098/rstb.2013.0121.
    vcat(2,"Values for oceans are inserted into the non-exisiting country Antarctica")
    out["ATA",,"n2o_n_direct"]<-3.8
    out["ATA",,"accumulation"]<-22
  } else if (method%in%c("Nsurplus","Nsurplus2")){
    baseyear="y2000"
    if(method=="Nsurplus2"){
      budget<-collapseNames(calcOutput("NitrogenBudgetOcean",deposition="Nsurplus",aggregate = FALSE)[,,"surplus"])
    } else {
      budget<-collapseNames(calcOutput("NitrogenBudgetOcean",deposition="CEDS",leaching="Nsurplus",aggregate = FALSE)[,,"surplus"])
    }
    emi<-collapseNames(calcOutput("EmisNitrogenOceans",aggregate = FALSE,method="ACCMIP")[,baseyear,])
    out<-setYears(emi/dimSums(budget,dim=1)[,baseyear,],NULL)*dimSums(budget,dim=1)
    out<-mbind(out,setNames(budget-dimSums(out,dim=3),"n2_n"))
    out<-add_columns(out,dim = 3.1,addnm = c("no3_n"))
    out[,,"no3_n"]=0
  }

  out<-add_dimension(out,dim = 3.1,nm="oceans")
  return(list(
    x=out,
    weight=NULL,
    unit="Mt Nr, NH3N and NO2N",
    description="Nitrogen emissions of oceans"))
}