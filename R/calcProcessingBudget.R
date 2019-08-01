calcProcessingBudget<-function(){
  mb<-calcOutput("FAOmassbalance",aggregate = FALSE)
  selection<-c(
    "milling","brans1","branoil1","flour1",
    "refining","sugar1","molasses1",
    "extracting","oil1","oil2","oilcakes1",
    "fermentation","alcohol1","alcohol2","alcohol3","brewers_grain1",
    "distilling","ethanol1","distillers_grain1","distillingloss", 
    "households")
  flows_in<-c("milling","refining","extraction","fermentation","distilling")
  
}