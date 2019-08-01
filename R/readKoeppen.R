readKoeppen<-function(){
  x<-read.csv("kgzones.csv")
  x$country<-NULL
  x<-as.magpie(x,spatial=1,temporal=0,datacol=2)
  return(x)
}

