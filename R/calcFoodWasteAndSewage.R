#recycling of food waste and sewage?
calcFoodWasteAndSewage<-function(){
  past<-findset("past") 
  mb<-calcOutput("FAOmassbalance",aggregate = FALSE)
  demand<-collapseNames(mb[,,"households"][,,c("nr")])
  # assuming N:P ratio of 10:1 based on Moree et al (2013)
  #Moree, A. L., A. H. W. Beusen, A. F. Bouwman, and W. J. Willems. 2013. “Exploring Global Nitrogen and Phosphorus Flows in Urban Wastes during the Twentieth Century.” Global Biogeochemical Cycles 27 (3): 836–46. doi:10.1002/gbc.20072.
  demand<-mbind(add_dimension(demand,dim=3.1,nm="nr"),add_dimension(demand,dim=3.1,nm="p")/10)
  # not differentiated by product
  demand<-dimSums(demand,dim=3.2)
  
  urban_shr<-setNames(calcOutput("UrbanPast", UrbanPast="WDI", aggregate=FALSE) ,"urban_hist")
  urban_shr<-collapseNames(urban_shr)[,past,]
  urban_shr<-mbind(setNames(urban_shr, "urban"),setNames(1-urban_shr,"rural"))
  urban_shr[urban_shr>1]<-1
  urban_shr[urban_shr<0]<-0
  
  # food waste should be estimated without using demand projections
  waste_shr<-calcOutput("Demand",aggregate = FALSE)
  waste_shr<-collapseNames(waste_shr[,,"waste_shr"][,past,"SSP2"])
  
  #urine:80% of intake for N, 62% for P (moree)
  urine<-new.magpie("GLO",NULL,c("nr","p"))
  urine[,,"nr"]<-0.8
  urine[,,"p"]<-0.62
  
  out<-mbind(
    add_dimension(demand*waste_shr,dim = 3.1,nm = "hh_food_waste"),
    add_dimension(demand*(1-waste_shr)*urine,dim = 3.1,nm = "urine"),
    add_dimension(demand*(1-waste_shr)*(1-urine),dim = 3.1,nm = "feces")
  )
  
  out<-out*urban_shr
  
  # add recycling as feed, fertilizer
  
  return(list(x=out,
              weight=NULL,
              unit="Mt Nr, P",
              description="Further destiny of food")
  )    
}
