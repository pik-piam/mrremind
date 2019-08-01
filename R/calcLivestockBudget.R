calcLivestockBudget<-function(){
  kli<-findset("kli")
  npk<-c("nr","p","k")

  excretion<-calcOutput("Excretion",aggregate = FALSE)[,,npk]
  excretion<-dimSums(excretion,dim=c(3.1))
  slaughtermass<-calcOutput("Slaughtermass",aggregate = FALSE)[,,kli][,,npk]
  mb<-calcOutput("FAOmassbalance",aggregate = FALSE)[,,npk]
  production <- collapseNames(mb[,,kli][,,"production"])
  slaughterwaste <- slaughtermass-production
  slaughterwaste<-round(slaughterwaste,5)

  feed<-dimSums(calcOutput("FeedPast",aggregate = FALSE,balanceflow=TRUE,nutrients="npk"),dim=3.2)
  getNames(feed,dim=1)<-substring(getNames(feed,dim=1),7)
  feed<-feed[,,kli]
  
  out<-mbind(
    add_dimension(feed,dim = 3.1,nm = "feed"),
    add_dimension(excretion,dim = 3.1,nm = "manure"),
    add_dimension(production,dim = 3.1,nm = "livestockproducts"),
    add_dimension(slaughterwaste,dim = 3.1,nm = "slaughterwaste")
  )
  
  test<-dimSums(out[,,c("feed")],dim=3.1)-dimSums(out[,,c("manure","livestockproducts","slaughterwaste")],dim=3.1)
  if (any(test>0.0001)){vcat(verbosity = 1, "Livstock Budget test indicates imbalances between feed, excretion and slaughtermass.")}
  return(list(x=out,
              weight=NULL,
              unit="Mt Nr, P or K",
              description="Nutrient budget for livestock: Feed equals the sum of excretion, livestockproducts and slaughterwaste",
              min=0,
              max=200)
  )      
}