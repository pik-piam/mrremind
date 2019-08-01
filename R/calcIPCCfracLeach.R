#' @title calcIPCCfracLeach
#' @description calculates the leaching rate FRAC_LEACH as defined by the IPCC Guidelines for National Greenhouse Gas Inventories 2006. We use the approach used by Canada, see
#' Velthof, Gerardus Lambertus, and J. Mosquera Losada. 2011. Calculation of Nitrous Oxide Emission from Agriculture in the Netherlands: Update of Emission Factors and Leaching Fraction. Alterra. http://library.wur.nl/WebQuery/wurpubs/406284.
#' @param cellular if true, returned on cell level
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ 
#' a<-calcOutput("IPCCfracLeach",cellular=FALSE)
#' }
#' 
#' @importFrom SPEI thornthwaite
#' @importFrom luscale weighted_mean.groupAggregate
calcIPCCfracLeach<-function(cellular=TRUE){
  if (cellular){
    past<-findset("past")
    #approach based on 
    #Velthof, Gerardus Lambertus, and J. Mosquera Losada. 2011. Calculation of Nitrous Oxide Emission from Agriculture in the Netherlands: Update of Emission Factors and Leaching Fraction. Alterra. http://library.wur.nl/WebQuery/wurpubs/406284.
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
    
    # estimate potential evapotranspiration using the thornwaite method for temperature and latitude
    
    tmp<-t
    tmp<-aperm(tmp,c(3,2,1))
    old<-tmp
    dim(tmp)=c(12*dim(t)[2],dim(t)[1])
    tmp<-thornthwaite(tmp,lat=lat[,,])
    old[,,]<-tmp
    pet<-as.magpie(aperm(old,c(3,2,1)))
  
    #komisch, weicht ab wenn man einzelne punkte vergleicht. scheint auch von den nachbarmonaten abzuhängen.
    #pet[2,5,3]==thornthwaite(t[2,1,3],lat=lat[2,,])
    
    pet=pet[,past,]
    temp=t[,past,]
    prec=p[,past,]
    
    ratio=prec/(pet+0.001)
    
    #ratio=c(0,0.05,0.23,0.5,1,2) teststring
    frac_leach=0.05+(0.3-0.05)/(1-0.23)*(ratio-0.23)
    frac_leach[frac_leach<0.05]<-0.05 # minimum leaching
    frac_leach[frac_leach>0.3]<-0.3 # maximum leaching
    frac_leach[ratio<0.1]<-0 # no leaching without water
    
    frac_leach_average<-dimSums(frac_leach[,,],dim=3)/12
    #plotmap2(frac_leach_average[,1,1])
    
    vcat(2,paste("For ",length(which(is.na(frac_leach_average))), " entries, no PET was possible to compute. set leaching to 0.3"))
    frac_leach_average[is.na(frac_leach_average)]=0.3
    
    weight=NULL
    
  }else if (!cellular){

    
    lu<-calcOutput("LanduseInitialisation",cellular=TRUE,aggregate = FALSE)

    frac_leach_average<-lu
    frac_leach_average[,,]<-calcOutput("IPCCfracLeach",aggregate = FALSE,cellular=TRUE)
    
    irrig<-calcOutput("LUH2v2",aggregate = FALSE,cellular=TRUE,irrigation=TRUE)
    
    irrig_shr<-collapseNames(irrig[,,"irrigated"][,,"crop"]/irrig[,,"total"][,,"crop"])
    irrig_shr[is.nan(irrig_shr)]<-0
    #plotmap2(irrig_shr[,1,1])
    #set leaching to maximum for irrigated regimes
    frac_leach_average[,,"crop"]=frac_leach_average[,,"crop"]*(1-irrig_shr)+0.3*irrig_shr
    
    weight=lu
    mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)  
    #x<-groupAggregate(x,query=mapping,dim = 3,from="luh2v2",to="land")
    #frac_leach_average<-toolAggregate(frac_leach_average,rel=mapping,dim = 1,from="cell",to="iso",weight = weight)
    frac_leach_average<-weighted_mean.groupAggregate(data = frac_leach_average,weight = weight,query=mapping,dim = 1,na.rm = T,from="celliso",to="iso")
    frac_leach_average[is.na(frac_leach_average)]<-0.05 # mostly forest in desert countries
    #plotcountrymap(frac_leach_average[,1,1])
    frac_leach_average  <- toolCountryFill(frac_leach_average,fill=0.3)
    budget<-calcOutput("NitrogenBudgetCropland",aggregate = FALSE)[,,"surplus"]
    budget2<-calcOutput("NitrogenBudgetPasture",aggregate = FALSE)[,,"surplus"]
    budget3<-calcOutput("NitrogenBudgetNonagland",aggregate = FALSE)[,,"surplus"]
    weight=mbind(setNames(budget,"crop"),
                 setNames(budget2,"past"),
                 setNames(budget3[,,"forestry"],"forestry"),
                 setNames(budget3[,,"primforest"],"primforest"),
                 setNames(budget3[,,"secdforest"],"secdforest"),
                 setNames(budget3[,,"other"],"other"),
                 setNames(budget3[,,"urban"],"urban")
                 )
  }
  
  
  return(list(
    x=frac_leach_average,
    weight=weight,
    unit="Million ha",
    min=0,
    max=0.31,
    description="Million hectare land area for different land use types.",
    isocountries=!cellular))
}
