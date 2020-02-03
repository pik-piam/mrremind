#' Convert James data
#' 
#' Convert James data on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing James data region resolution
#' @param subtype subtype of GDP indicator to be selected
#' @return GDP per capita in USD05 in PPP or MER as magpie object
#' @author David CHen Benjamin Bodirsky
#' @seealso \code{\link{readJames}}
#' @examples
#' 
#' \dontrun{ a <- convertJames2019(x,"IHME_USD05_PPP_pc")
#' }
#' @importFrom countrycode countrycode

convertJames2019 <- function(x,subtype) {
  x<-x[c("USSR_FRMR","CHN_354","CHN_361"),,,invert=TRUE] #Macao and HKG and Former USSR have 0 values in the dataset
  y<-toolCountryFill(x[,,subtype],fill = 0) 
  
  #fill missing islands not in MissingIslands, using older James
  old <- readSource("James", subtype=subtype)
  missing <- time_interpolate(old[c("ABW","PYF","NCL"),,], interpolated_year = c(1950:2019))
  y[c("ABW","PYF","NCL"),,] <- missing
  
  # use old HKG and MAC shares, and subtract from CHN
  pop <- calcOutput("Population", aggregate=F)[,,"pop_SSP2"]
  pop <- time_interpolate(pop, interpolated_year = c(1965:2019))
  old_t <- pop[,c(1965:2015),]*old[,c(1965:2015),]
  old_t <- collapseNames(old_t)
  shr <- old_t[c("HKG","MAC"),,]/dimSums(old_t[c("CHN","HKG","MAC"),,], dim=1)
  shr <- time_interpolate(shr, interpolated_year = c(1965:2019))
  
  y_t <- pop*y[,c(1965:2019),]
  y_t[c("HKG","MAC"),,] <- shr*y_t["CHN",,]
  y_t["CHN",,] <- y_t["CHN",,]-dimSums(y_t[c("HKG","MAC"),,], dim=1)
  y1 <- y_t/pop
  y1 <- collapseNames(y1, collapsedim = 1)
  
  return(y1)
}  
