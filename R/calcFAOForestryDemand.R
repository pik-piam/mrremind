#' @title calcFAOForestryDemand
#' @description 
#' Calculates the demand of forestry products from FAO data.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link{calcFAOmassbalance_pre}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FAOForestryDemand")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @export

calcFAOForestryDemand <- function(){
  x <- readSource("FAO","ForestProdTrade")
  
  naming <- c("Roundwood","Industrial roundwood","Wood fuel")
 
  y <- x[,,naming]
  
  y <- y[,,c("Production_(m3)","Import_Quantity_(m3)","Export_Quantity_(m3)")]
  
  # Moisture content IHPA standard  : 12% as in FAO report Page 19, table 3.3 (http://www.fao.org/forestry/23525-0a5fbd590fc0988ce61f01bffe1b2f019.pdf)
  
  # conversion factor of roundwood  : 632.5 kg/m3 (mean value) as in FAO Document (http://www.fao.org/3/a-i4441e.pdf), Page 6, table 4.
  
  y[,,"Industrial roundwood"] <- 632.5 * y[,,"Industrial roundwood"]
  
  # Conversion factor of wood fuel  : 307.1 kg/m3 (mean value) as in FAO Document (http://www.fao.org/3/a-i4441e.pdf), Page 7, table 6.
  
  y[,,"Wood fuel"] <- 307.1 * y[,,"Wood fuel"]
  
  y[,,"Roundwood"] <- y[,,"Industrial roundwood"] + y[,,"Wood fuel"]
  
  #All the units are now in Kg
  getNames(y,dim = 2) <- c("production","import","export" )
  
  y <- add_columns(y,addnm = "other_util",dim = 3.2)
  y[,,"other_util"] <- y[,,"production"] + y[,,"import"] - y[,,"export"]
  
  y <- add_columns(y,addnm = "domestic_supply",dim = 3.2)
  y[,,"domestic_supply"] <- y[,,"other_util"]
  
  y[which(y<0)] <- 0
  
  y <- y/(1000*1000000) # Now data is in million tonnes
  
  y[is.na(y)] <- 0
  
  y[y<0] <- 0
  getNames(y,dim = 1) <- c("roundwood","wood","woodfuel")
  
  pop <- setNames(readSource("WDI",subtype="SP.POP.TOTL",convert = T),"weight")
  pop <- pop[,intersect(getYears(y),getYears(pop)),]
  
  y <- y[,intersect(getYears(y),getYears(pop)),]
  
  # weight <- y
  # weight[,,] <- 1
  
  out <- y
  return(list(x=out,
              weight=pop,
              min=0,
              unit="mt",
              description="Calculates the demand pattern of wood products based on historical FAO data"))
  
}

  ## Mapping procedure
  # country2cell <- read.csv(toolMappingFile("cell","CountryToCellMapping.csv"))
  # 
  # xx <- toolAggregate(y,rel = country2cell,from = "iso",to = "cell",dim = 1,partrel = T)
  
    # 
  # ## Production
  # plot_list <- list()
  # for(i in c("production")){
  #   for(j in getNames(xx,dim = 1)){
  #     for (k in c("y2005","y2006","y2007","y2008","y2009","y2010")){
  #       yrs <- length(c("y2005","y2006","y2007","y2008","y2009","y2010"))
  #       nametoplot <- paste0(j,".",i)
  #       p <- plotmap2(data = xx[,k,nametoplot],
  #                     legendname = paste0(i),
  #                     sea = TRUE,
  #                     title = paste0(j," ",i),
  #                     text_size = 14)
  #       plot_list[[k]] <- p
  #       vcat(verbosity = 1,"Plotting data for",k,"\n Forest type:",j,"\n  Data type:",i)
  #     }
  #     pdf(paste0(j,"_",i,".pdf"))
  #     for (l in 1:length(c("y2005","y2006","y2007","y2008","y2009","y2010"))) {
  #       print(plot_list[[l]])
  #       vcat(verbosity = 1, "\nPdf file:",(paste0(i,"_",j,".pdf")),"\n Page being written:",l,"/",yrs)
  #     }
  #     dev.off()
  #   } 
  # }
  # graphics.off()
  # 
  # 
  
