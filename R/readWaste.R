#' Read WhataWaste2.0 World Bank data
#' 
#' Read-in a xlsx file as magclass object
#' kg/cap
#' 
#' @param subtype data subtype. "Generation" "Composition" and "Treatment" in kg/capita
#' @return magpie object of the WhataWaste data with Generation, Disposal, or Composition 
#' @author David Chen
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="Waste",subtype="Generation")
#' }
#' 
#' @importFrom readxl read_xlsx
#' @importFrom dplyr rename
#' @importFrom magclass as.magpie


readWaste <- function(subtype) {
  
  #Reading generation data
  #data <- read.csv("C:/PIK/inputdata/sources/NewWaste/waste2.csv")
  
  data <- read.csv("waste2.csv")
  
  
  data<-data[,c("iso3c", "total_msw_total_msw_generated_tons_year",                                              
                colnames(data)[grep("composition", colnames(data))],
                colnames(data)[grep("treatment", colnames(data))],
                colnames(data)[grep("special", colnames(data))]
  )]
  
  colnames(data) <- gsub("_percent", replacement = "", colnames(data))
  colnames(data) <- gsub("waste_", replacement = "", colnames(data))
  colnames(data)[(names(data) == c("iso3c", "total_msw_total_msw_generated_tons_year"))] <- c("country", "total")
  
  #dummy year
  n <- nrow(data)
  year <- rep(2015, n)
  data <- cbind(data,year)
  data <- data[,c(1,ncol(data),2:(ncol(data)-1))]
  
  x <- as.magpie(data, spatial=1, temporal=2, datacol=3)
  
  #convert tons to 
  #Or add CHI to GBR and 
  # and XKX to SRB?
  
  x <- x[!getRegions(x) == "CHI"]  
  x <- x[!getRegions(x) == "XKX"]  
  
  pop <- calcOutput("Population", aggregate = F,naming="indicator.scenario")
  pop2015 <- 1000000*pop[,"y2015","pop.SSP2"]
  
  regions <-intersect(getRegions(x),getRegions(pop))
  
  x_pc <- 1000*(x[c(regions),,"total"]/pop2015[c(regions),,])
  
  
  if (subtype=="Generation"){
    x <-x_pc
    x<-collapseNames(x, collapsedim=c(2,3))
    getNames(x) <- "total_waste_pc"
  }   
  
  else if (subtype== "Composition") {
    comp <-  x[,,c(2:10)]
    x <-(comp * x_pc)/100
    x<-collapseNames(x)
    getNames(x) <- gsub("composition_", replacement="",getNames(x))
    getNames(x) <- gsub("_organic_waste", replacement="",getNames(x))
    getNames(x) <- gsub("_garden_green_waste", replacement="",getNames(x))
    getNames(x) <- gsub("wood", replacement="wood_waste",getNames(x))
    
    
  }
  
  
  else if (subtype== "Treatment") {
    treat <-  x[,,c(11:21)]
    x <-(treat*x_pc)/100
    x<-collapseNames(x)
    getNames(x) <- gsub("treatment_", replacement="",getNames(x))
  }
  
  else if (subtype == "Special"){
    specialwaste <- x[,,c(22:27)]
    x <-specialwaste/pop2015[c(regions),,]
    x<-collapseNames(x)  
    getNames(x) <- gsub("special_", replacement="",getNames(x))
    getNames(x) <- gsub("_tons_year", replacement="",getNames(x))
  }
  
  
  return(x)  
  
}



# #after this all should go into convert but here for now since convert can't be done without regressions
# a <- calcOutput("DevelopmentState", aggregate=F)
# commonregions=intersect(getRegions(a),getRegions(x))
# x<-mbind(x[c(commonregions),,], a[c(commonregions),"y2005","SSP2"])
# 
# for (i in c(commonregions)){
#   if(x[i,,"SSP2"] == 0 & x[i,,"dumps"]==0){
#     x[i,,"dumps"] <- x[i,,"other_disp"]}     
#   else if(x[i,,"SSP2"] == 0 & x[i,,"landfills"]==0){
#     x[i,,"landfills"] <- x[i,,"other_disp"]}
#   else if(x[i,,"SSP2"] > 0 & x[i,,"SSP2"]<= 0.5 & x[i,,"dumps"]==0){
#     x[i,,"dumps"] <- x[i,,"other_disp"]}
#   else if(x[i,,"SSP2"] > 0 & x[i,,"SSP2"]<= 0.5 & x[i,,"recycled"]==0){
#     x[i,,"recycled"] <- x[i,,"other_disp"]}
#   else if(x[i,,"SSP2"] > 0.5 & x[i,,"SSP2"]< 1 & x[i,,"landfills"]==0){
#     x[i,,"landfills"] <- x[i,,"other_disp"]}
#   else if(x[i,,"SSP2"] > 0.5 & x[i,,"SSP2"]< 1 & x[i,,"recycled"]==0){
#     x[i,,"recycled"] <- x[i,,"other_disp"]}
#   else if(x[i,,"SSP2"] > 0.5 & x[i,,"SSP2"]< 1 & x[i,,c("landfills","recycled")]!=0){
#     x[i,,"dumps"] <- x[i,,"other_disp"] + x[i,,"dumps"]}
#   else if(x[i,,"SSP2"] == 1 & x[i,,"landfills"]==0){
#     x[i,,"landfills"] <- x[i,,"other_disp"]}     
#   else if(x[i,,"SSP2"] == 1 & x[i,,"recycled"]==0){
#     x[i,,"recycled"] <- x[i,,"other_disp"]}
#   else if(x[i,,"SSP2"] == 1 & x[i,,c("landfills","recycled")]!=0){
#     x[i,,"landfills"] <- x[i,,"other_disp"] + x[i,,"landfills"]}  
# }
# 
# x <- x[,,c(-6,-8)]
# return(x)