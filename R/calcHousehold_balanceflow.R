
calcHousehold_balanceflow<-function(){
  massbalance<-calcOutput("FAOmassbalance",aggregate=F)
  # add missing products

  nutrition_attributes<-calcOutput("NutritionAttributes",aggregate=F)
  dimnames(nutrition_attributes)[[3]]<-gsub(".protein",dimnames(nutrition_attributes)[[3]],replacement = ".nr")
  dimnames(nutrition_attributes)[[3]]<-gsub(".kcal",dimnames(nutrition_attributes)[[3]],replacement = ".ge")
  nutrition_attributes[,,"ge"]=nutrition_attributes[,,"ge"]*4.184
  nutrition_attributes[,,"nr"]=nutrition_attributes[,,"nr"]/6.25
  
  #share<-calcFooduse_to_household_share()$x
  household<-dimSums(massbalance[,,"households"][,,c("ge","nr")],dim=c(3.2),na.rm=T)

  fooduse<-dimSums(massbalance[,,c("food","milling")],dim=c(3.2),na.rm=T)
  out<-collapseNames(household[,getYears(fooduse),][,,"ge"]/nutrition_attributes[,getYears(fooduse),][,,"ge"])-fooduse[,,"dm"]
  out[out==Inf]<-0
  out[is.nan(out)]<-0
  out2<-collapseNames(fooduse[,,"dm"])*nutrition_attributes[,getYears(fooduse),][,,c("ge","nr")] - fooduse[,,c("ge","nr")] 
  out<-mbind(out,out2)
  
  #add years beyond 2010
  years<-findset("time")
  lastyear=paste0("y",max(getYears(household,as.integer = T)))
  missingyears<-setdiff(years,getYears(out))
  out<-add_columns(x = out,addnm = missingyears,dim = 2.1)
  out[,missingyears,]<-setYears(out[,lastyear,],NULL)
  
  #fading out the balanceflow until 2050.
  out<-convergence(origin = out,aim = 0,start_year = "y2010",end_year = "y2050",type = "s")
    
  return(list(x=out,weight=NULL, unit="Mt dry matter (dm), PJ energy (ge), Mt reactive nitrogen (nr)",description="Balance flow to make country-specific values on nutrition outcome consistent with global homogeneous nutrition values. In case of GE and Nr includes processing losses, in the case of DM just balances unhomogeneous products."))
}

