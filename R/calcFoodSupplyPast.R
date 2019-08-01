#' @title calcFoodSupplyPast
#' @description Calculates the food supply (as defined by FAO, including intake and household waste) for the past.
#'
#' @param per_capita if true, calculates per capita demand per day, otherwhise total demand per year
#' @param products a set with the products that shall be provided, e.g. kall. If NULL, the products are provided that are in the primary data
#' @param product_aggr if TRUE, all products are summed up, if "maingroups" products are summed over livestock products, staples and vegfruits.
#' @param populationweight datasource of populationweight: FAO can be selected in order to better meat exact values. Normal datasource is PopulationPast
#' @param attributes attributes of different products,i.e., kcal,protein,wm
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Xiaoxi Wang
#' @seealso
#' \code{\link{calcFAOharmonized}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FoodSupplyPast")
#' }
#' 
#' @importFrom magclass getSets

calcFoodSupplyPast<-function(per_capita=TRUE, products=NULL, product_aggr=FALSE, populationweight="PopulationPast",attributes=c("kcal","protein","wm")){
  kfo<-findset("kfo")
  kcal<-calcOutput("FAOmassbalance",aggregate = FALSE)
  kcal<-kcal[,,"households"][,,kfo][,,c("ge","nr","wm")]
  kcal<-collapseNames(kcal)
  
  #translate back to calories and proteins
  kcal[,,"ge"]=kcal[,,"ge"]/4.184
  kcal[,,"nr"]=kcal[,,"nr"]*6.25
  #make sure order is correct
  kcal<-kcal[,,c("ge","nr","wm")]
  getNames(kcal,dim=2) <- c("kcal","protein","wm")
  
  if(!is.null(products)){
    products<-findset(products)
    kall <- findset("kall")
    missing<-kall[which(!kall%in%getNames(kcal,dim = 1))]
    if (length(missing>0)){
      vcat(verbosity = 2, paste0("The following products were not included in FAO Food supply and were added with value 0:",paste(missing,collapse = " ")))
      kcal<-add_columns(kcal,addnm = missing,dim = 3.1)
      kcal[,,missing]<-0
    }
    kcal<-kcal[,,products]
  }
  if (product_aggr==TRUE){
    kcal<-dimSums(kcal,dim=3.1)
  } else if (product_aggr=="maingroups"){
    kap<-findset("kap")
    kst<-findset("kst")
    staples<-add_dimension(dimSums(kcal[,,kst],dim=3.1),dim = 3.1,add = "products",nm = "kst")
    animals<-add_dimension(dimSums(kcal[,,kap],dim=3.1),dim = 3.1,add = "products",nm = "kap")
    vegfruit<-add_dimension(dimSums(kcal[,,"others"],dim=3.1),dim = 3.1,add = "products",nm = "vegfruit")
    kcal<-mbind(staples,animals,vegfruit)
  } else if (product_aggr != FALSE) {stop("unknown product_aggr")}
   
  if(per_capita==TRUE){
    if (populationweight=="PopulationPast"){
      weight=collapseNames(calcOutput("PopulationPast",aggregate = FALSE))
    }else if (populationweight=="FAO"){
      weight <- collapseNames(readSource(type="FAO",subtype = "Pop",convert = T)[,getYears(kcal),"population"])/1000000
    }
    weight<-weight[,getYears(kcal),]
    out=kcal/weight/365*1000000

    if(any(is.nan(out))){out[is.nan(out)]=0}
    if(any(out==Inf)){out[out==Inf]=0}    
        
    unit="kcal per capita per day, g protein or fat per capita per day, kg food wet matter per capita per day"
    min=0
    max=5000
    
    

  } else if (per_capita==FALSE) {
    out<-kcal
    out[,,c("wm")]=out[,,c("wm")]/1000
    out[,,c("kcal")]=out[,,c("kcal")]
    out[,,c("protein")]=out[,,c("protein")]/10^6
    
    weight=NULL
    unit="Mio Kcal, Mt protein, Mt WM per year"
    min=0
    max=2e+10
  } else{stop("per_capita has to be binary")}
  
  
  out<-collapseNames(out[,,attributes])
  
  return(list(x=out, #datensatz (muss auf Iso-länder ebene sein)
              weight=weight, #wenn nicht absolute werte
              unit=unit,
              description="FAO food supply (including household waste)",
              min=min #für error checking (zB zw 0 und 1)
              )
  ) 
}

