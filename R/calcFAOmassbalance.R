#' @title calcFAOmassbalance
#' @description 
#' Calculates a massbalance dataset of agricultural production, processing and
#' use out of the combined data of calcFAOharmonized(). Covers dry matter (DM),
#' reactive nitrogen (Nr), Phosphorus (P), Generalizable Energy (GE) and wet
#' matter (WM). New products are added to the Food Balance Sheets, and many processing
#' conversions are made more explicit using simple assumptions. The first part of this
#' function is the calcFAOmassbalance_pre.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Xiaoxi Wang
#' @seealso
#' \code{\link{calcFAOmassbalance_pre}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FAOmassbalance")
#' }
#' @importFrom magclass getNames<- as.magpie


calcFAOmassbalance<-function(){
  mb<-calcOutput("FAOmassbalance_pre",aggregate = F)
  past<-findset("past")
  mb1<-add_columns(mb,dim = 3.2,addnm = "bioenergy")
  mb1[,,"bioenergy"]<-0
  mb1<-mb1[,,
    c("production","production_estimated",
    "export", "import","stock_variation",
    "domestic_supply",
    "food","feed","seed","waste","other_util","bioenergy",        
    "milling", "brans1","branoil1"  ,"flour1",
    "refining", "sugar1","molasses1",         
    "extracting", "oil1","oil2","oilcakes1",
    "fermentation","alcohol1",
    "alcohol2","alcohol3","brewers_grain1",
    "distilling","ethanol1","distillers_grain1",
    "distillingloss",
    "households")]  
  newitems<-setdiff(findset("kall"),getNames(mb1,dim=1))
  mb2<-add_columns(mb1,dim = 3.1,addnm = newitems)
  mb2[,,newitems]<-0
  
  ### Add feed by animal group
  # consists of feed by animal group according to Isabelle Weindls Feed Baskets plus a balanceflow to bee consistent with FAO
  feed<-calcOutput("FeedPast",balanceflow=FALSE,aggregate = FALSE)
  getNames(feed,dim=1)<-paste0("feed_",substring(getNames(feed,dim=1),7))
  feed<-as.magpie(aperm(unwrap(feed),c(1,2,4,3,5)))
  
  balanceflow<-calcOutput("FeedBalanceflow",aggregate = FALSE)[,past,]
  getNames(balanceflow,dim=1)<-paste0("feed_",getNames(balanceflow,dim=1))
  balanceflow<-balanceflow*calcOutput("Attributes",aggregate = FALSE)
  balanceflow<-as.magpie(aperm(unwrap(balanceflow),c(1,2,4,3,5)))  
  
  feed<-feed+balanceflow
  
  #test to check whether distribution of feed was ok
  if(any(round(
      mb2[,,"feed"][,,getNames(mb,dim=1)]
      -dimSums(feed[,,getNames(mb,dim=1)],dim=3.2)
      ,7)!=0)){
    vcat(verbosity=1,"Something is strange here. Check Feedbalanceflow")
  }
  mb3<-mbind(mb2,feed)
  
#  forest <- calcOutput("FAOForestryDemand",aggregate = F)
  forest <- calcOutput("TimberDemand",aggregate = F)
  mb3[,,getNames(mb3[,,paste0("wood.",getNames(forest,dim = 2),".dm")])] <- forest[,intersect(getYears(mb3),getYears(forest)),getNames(forest[,,"Industrial roundwood"])]
  mb3[,,getNames(mb3[,,paste0("woodfuel.",getNames(forest,dim = 2),".dm")])] <- forest[,intersect(getYears(mb3),getYears(forest)),getNames(forest[,,"Wood fuel"])]
  
  # Adding Pasture as feed item
  mb3[,,"pasture"][,,c("domestic_supply","production","feed")]<-dimSums(feed[,,"pasture"],dim=3.2)
  
  # Adding Crop Residues Production and use
  kres<-findset("kres")
  res<-calcOutput("ResDemand",aggregate = F)
  res<-as.magpie(aperm(unwrap(res),c(1,2,4,3,5)))
  mb3[,,kres][,,c("bioenergy","domestic_supply","feed", "other_util","production")]<-res
  
  ##############################################################################
  ### Dividing other_util into bioenergy and other_util
  bioenergy <- collapseNames(calcOutput("1stBioenergyPast",aggregate = F)[,,"INDPROD"])
  att<-calcOutput("Attributes",aggregate = FALSE)
  EthanolOilFactor <- (att/(collapseNames(att[,,"ge"])))[,,c("ethanol","oils")]
  mb3[,,c("ethanol","oils")][,,"bioenergy"] <- bioenergy[,getYears(mb3),c("ethanol","oils")]*EthanolOilFactor
  
  # in some cases bioenergy demand from EEA exceeds ethanol production.
  # We limit it to the availabiltiy in FAOmassbalance_pre
  exceeded <- mb3[,,c("ethanol","oils")][,,"bioenergy"] > mb3[,,c("ethanol","oils")][,,"other_util"] 
  mb3[,,c("ethanol","oils")][,,"bioenergy"][exceeded]<-mb3[,,c("ethanol","oils")][,,"other_util"][exceeded]
  
  mb3[,,c("ethanol","oils")][,,"other_util"] <- mb3[,,c("ethanol","oils")][,,"other_util"] - mb3[,,c("ethanol","oils")][,,"bioenergy"]

  return(list(
    x=mb3,
    weight=NULL,
    unit="Mt DM, Mt WM, PJ, Mt Nr, Mt P, Mt K",
    description="FAO massbalance calculates all conversion processes within the FAO CBS/FBS and makes them explict."))
}
