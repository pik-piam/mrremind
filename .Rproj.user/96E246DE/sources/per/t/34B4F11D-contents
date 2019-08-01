#' @title calc1stBioDem
#' @description 
#' Calculates projections of first generation biofuels demand,including biogas, bioethamol and biodiesel, from IEA database. 
#' The unit is Petajoule. 
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @param subtype all per default. ethanol_oils for selecting 1st gen crop types relevant for REMIND input.
#' @author Xiaoxi Wang, David Klein
#' @seealso
#' \code{\link{calc1stBioenergyPast}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("1stBioDem")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @importFrom magpiesets findset

calc1stBioDem<- function(subtype="all"){
  past<-findset("past")
  time<-findset("time")
  lastyear<-past[length(past)]

  x <- collapseNames(calcOutput(type = "FAOmassbalance",aggregate = FALSE)[,,"bioenergy"][,,"ge"])
  x <- add_columns(x,dim = 2.1,addnm = setdiff(time,past))
  x[,setdiff(time,past),]<-0
  
  ### fading out crop residues until 2040.
  #kres <- findset("kres")
  #tmp<-convergence(origin = x[,lastyear,kres],aim = 0,start_year = lastyear,end_year = "y2050",type = "s")
  #x[,getYears(tmp),kres]<-tmp
  
  bioenergy_projection <- readSource("LotzeCampenBiofuel") [,,c("oils","ethanol")] 
  
  x[,c("y2020","y2030"),c("oils","ethanol")]<-bioenergy_projection[,c("y2020","y2030"),c("oils","ethanol")]
  # if values decline, keep them constant
  x <- x[,sort(getYears(x)),]
  for (y in 2:length(getYears(x))) {
      x[,y,c("oils","ethanol")] <- pmax(x[,y,c("oils","ethanol")],setYears(x[,y-1,c("oils","ethanol")],NULL))
  }
  x[,,c("oils","ethanol")] <- time_interpolate(dataset = x[,,c("oils","ethanol")][,c("y2015","y2025"),,invert=TRUE],interpolated_year =c("y2015","y2025"),integrate_interpolated_years = TRUE)
  countries_without_values_oils<-where(x[,"y2020","oils"]==0)$true$regions       
  countries_without_values_ethanol<-where(x[,"y2020","ethanol"]==0)$true$regions   
  x[countries_without_values_oils,c("y2015","y2020","y2025","y2030"),"oils"]<-setYears(x[countries_without_values_oils,c("y2010"),"oils"],NULL)
  x[countries_without_values_oils,c("y2015","y2020","y2025","y2030"),"ethanol"]<-setYears(x[countries_without_values_oils,c("y2010"),"ethanol"],NULL)
  
  ### scenarios for bioenergy and bioethanol 
  x <- add_dimension(x = x,dim = 3.1,add = "scenario",nm = "const2030")
  x <- add_columns(x=x,addnm = "const2020",dim = 3.1)
  x <- add_columns(x=x, addnm = "phaseout2020",dim = 3.1)
  x[,,]<-x[,,"const2030"]
  
  yearstmp<-c("y2035","y2040","y2045","y2050","y2055","y2060","y2065","y2070","y2075","y2080","y2085","y2090","y2095","y2100","y2105","y2110","y2115","y2120","y2125","y2130","y2135","y2140","y2145","y2150")
  
  x[,yearstmp, "oils"][,,"const2030"] <- setYears(x[,c("y2030"),"oils"][,,"const2030"],NULL)
  x[,yearstmp, "ethanol"][,,"const2030"]  <- setYears(x[,c("y2030"),"ethanol"][,,"const2030"] ,NULL)
  
  yearstmp<-c("y2025","y2030","y2035","y2040","y2045","y2050","y2055","y2060","y2065","y2070","y2075","y2080","y2085","y2090","y2095","y2100","y2105","y2110","y2115","y2120","y2125","y2130","y2135","y2140","y2145","y2150")
  x[,yearstmp, "oils"][,,"const2020"] <- setYears(x[,c("y2020"),"oils"][,,"const2030"],NULL)
  x[,yearstmp, "ethanol"][,,"const2020"]  <- setYears(x[,c("y2020"),"ethanol"][,,"const2030"] ,NULL)
 
  q <- convergence(origin = x[,, c("oils","ethanol")][,,"const2020"],aim = 0,start_year = "y2020",end_year = "y2040",type = "linear")
  x[, getYears(q), c("oils","ethanol")][,,"phaseout2020"] <- q
  
  ### phase out of crop residues
  a <- x[,,c("res_cereals", "res_nonfibrous", "res_fibrous")]
  a[,getYears(a,as.integer = TRUE)>2010,] <- setYears(a[,2010,],NULL)
  w <- convergence(a, aim=0, start_year = 2010, end_year = 2060, type="s")
  x[,getYears(w), c("res_cereals", "res_nonfibrous", "res_fibrous")] <- w
  
  x <- clean_magpie(x)
  
  if (subtype=="ethanol_oils") {
    x <- x[,,c("ethanol","oils")][,,"const2030"]
    x <- collapseNames(x)
    # rename magpie crop names to remind crop names
    getNames(x) <- gsub("ethanol","pebios",getNames(x))
    getNames(x) <- gsub("oils","pebioil",getNames(x))
  }
  
  return(list(x=x, weight=NULL, 
              unit="PJ", 
              description="1st generation bioenergy demand for different scenarios based on data from IEA and Lotze-Campen 2014")
  )
}
