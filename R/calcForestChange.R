#' Forest change rates
#' 
#' Calculates average annual forest change rates for 1995, 2000, 2005 and 2010
#' from source FAO_forestry_extent. Constant after 2010. Positive values
#' inidicate deforestation, negative vlaues afforestation. A weight for the
#' aggregation is not implemented
#' 
#' 
#' @return writes a magpie object with the new regions to the output-folder
#' defined in the file "exampleconfig.cfg"
#' @author Florian Humpenoeder
#' @seealso \code{\link{setConfig}}, \code{\link{readSource}},
#' \code{\link{readFAO_forestry}}, \code{\link{calcOutput}}
#' @importFrom magclass lowpass

calcForestChange <- function() {
  
  x   <- readSource("FAO_forestry","extent")
  years <- getYears(x,as.integer = TRUE)
  y <- x[,2:length(years),]
  y[,,] <- NA
  for (t in 2:length(years)) {
    y[,years[t],] <- ((setYears(x[,t-1,],years[t]) - x[,t,])/(years[t]-years[t-1]))
  }
  y <- mbind(setYears(y[,1,],1995),y)
  y <- lowpass(y,i=2)
  y <- mbind(y,setYears(y[,2010,],2150))
  y <- time_interpolate(y,interpolated_year = seq(2010,2150,by=5),integrate_interpolated_years = TRUE)

  return(list(x=y,weight=NULL,unit="Mha/yr",description="Average annual change in forest area between 1995 and 2010, constant at 2010 levels thereafter (FRA2010)"))
}
