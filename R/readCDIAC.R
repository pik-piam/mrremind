readCDIAC<- function() {
  
  emi <- read.csv("nation.1751_2013.csv",stringsAsFactors = TRUE)
  # replace dots with spaces, remove double spaces
  names(emi) <- gsub("  "," ",gsub("\\."," ",names(emi)))
  
  map <- c("Total CO2 emissions from fossil fuels and cement production thousand metric tons of C "= "FFIC",
            "Emissions from solid fuel consumption" = "Solids",
            "Emissions from liquid fuel consumption" = "Liquids",
            "Emissions from gas fuel consumption"   = "Gases",
            "Emissions from cement production"      = "Cement",
            "Emissions from gas flaring"            = "Flaring",
            "Per capita CO2 emissions metric tons of carbon " = "PerCap",
            "Emissions from bunker fuels not included in the totals " = "Bunker")
  
  # rename columns according to mapping defined above
  for (i in 1:length(map)) {
   names(emi) <- gsub(names(map)[i],map[i],names(emi))
  }
  
  # remove rows 1-4 containing comments
  emi <- emi[-(1:4),]
  
  # replace "." with NA
  emi[emi=="."]<-NA

  # change classes of data columns from Factor or int to numeric  
  cols <- 3:10
  emi[,cols] <- apply(emi[,cols], 2, function(x) as.numeric(as.character(x)))
  
  emi$Nation <- gsub("\\.","",emi$Nation)
  
  x <- as.magpie(emi,spatial=1,datacol=3)

  return(x)
}
