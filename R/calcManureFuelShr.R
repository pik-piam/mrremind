#' @title calcManureFuelShr
#' @description calculates the share of Manure excreted during grazing which is collected for fuel. For the future, we assume that with the development, the fuel share reaches 0.
#'
#' @param products IPCC: IPCC products. MAgPIE: Magpie products
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcExcretion}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ManureFuelShr")
#' }
#' 


calcManureFuelShr<-function(products="magpie"){
  past<-findset("past")
  
  excretion<-calcOutput("ExcretionIPCC",aggregate = FALSE,products=products)
  
  pasture_categories<-c("pasture_range_paddock","fuel")

  
  excretion<-excretion[,,pasture_categories]

  
  weight<-dimSums(excretion,dim=3.2)
  
  ManureFuelShr<-collapseNames(excretion[,,"fuel"]/weight)
  
  weight[is.na(weight)]<-0
  ManureFuelShr[is.na(ManureFuelShr)]<-0
  
  ManureFuelShr<-toolHoldConstantBeyondEnd(ManureFuelShr)
  weight<-toolHoldConstantBeyondEnd(weight)
  
  tmp<-ds<-calcOutput("DevelopmentState",aggregate = FALSE)
  tmp[,,]<-1
  ManureFuelShr<-convergence(origin = ManureFuelShr*tmp,aim = ManureFuelShr*(1-ds),start_year = "y2010",end_year = "y2050",type = "s")
  weight<-weight*tmp
  
  return(list(x=ManureFuelShr,
              weight=weight,
              unit="share",
              description="share of excreted nitrogen on pastures that is collected for fuel",
              min=0,
              max=1)
  )                   
}
