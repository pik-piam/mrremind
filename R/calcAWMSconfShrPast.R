#' @title calcAWMSconfShrPast
#' @description calculates the share of manure excreted in different types of animal waste management systems in  confinements in the year 2005 using the IPCC Guidelines excretion rates.
#'
#' @param products IPCC: IPCC products. MAgPIE: Magpie products
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @seealso
#' \code{\link{calcAWMSconfShr}},
#' \code{\link{calcExcretionIPCC}}
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("AWMSconfShrPast")
#' }
#' @importFrom magclass collapseNames where
#' @importFrom madrat toolXlargest




calcAWMSconfShrPast<-function(products="magpie"){
  excretion<-calcOutput("ExcretionIPCC",products=products,aggregate = FALSE)
  awms<-setdiff(getNames(excretion,dim=2),c("pasture_range_paddock","fuel"))
  excretion<-excretion[,,awms]
  
  incomplete<-where(dimSums(excretion,dim=c(3.2))==0)$true$regions
  largest <- toolXlargest(type="Population", range=1:30)
  if(any(incomplete%in%largest)){
    vcat(verbosity = 1,paste("no complete excretion data for",paste(incomplete[incomplete%in%largest],collapse=" "),", and eventually some smaller countries."))
  }
  
  weight<-excretion
  weight[,,]<-NA
  weight[,,]<-dimSums(excretion,dim=3.2)
  shr<-collapseNames(excretion/weight)

  tmp<-shr[,,"other"]
  tmp[is.na(tmp)]<-1
  shr[is.na(shr)]<-0
  shr[,,"other"]<-tmp
  #shr<-round(shr,6)
  weight[is.na(weight)]<-0
  
  if(any(colSums(weight)==0)){warning("weight is zero for the whole world - dangerous!")}
  
  return(list(x=shr,
              weight=weight,
              unit="share",
              description="share of excreted nitrogen within stables excreted in which awms",
              min=0,
              max=1)
  )                   
}




