#' @title convertLutz2014
#' 
#' @description It fills the missing values of the output of readLutz2014 through 
#' the weighted average of the values of two countries with similar carachteristcs
#' to the one that has na values.
#' @param x magpie object provided by the read function
#' 
#' @seealso
#' \code{\link{readLutz2014}}


convertLutz2014 <- function(x){
  
  pop_wdi<-calcOutput("Population",PopulationPast="WDI_completed",PopulationFuture="SSP_completed",aggregate = FALSE) # at least one citizen per country
  
  # handle countries with missing education data before 2010
  tmp<-where(is.na(x))$true$region
  #missing years
  missing=paste0("y",1965+(1:9)*5)
  # keep education structure constant over time
  x[,missing,] = setYears(x[,"y2010",]/x[,"y2010","Total"],NULL)*x[,missing,"Total"]
  x[is.nan(x)]<-0
	
	x <- toolCountryFill(x,fill=NA, no_remove_warning = "ANT" )
	
	# BB: use of speed_aggregate with an external mapping could replace the following function and speed it up
	fill_country_by_average_of_region <- function(x, country="SSD", region=c("AFG","TCD")){
	  vcat(2,paste0("interpolating country: ", country))
	  values <- x[region,,]
	  population <- pop_wdi[country,getYears(values),"pop_SSP2"]
	  x[country,,]  <-  setCells(dimSums(values, dim=1)/dimSums(values[,,"Total"][,,"Both"][,,"All"], dim=1),"GLO") * population
	  return(x)
	}
	
	x <- fill_country_by_average_of_region(x, country="AIA", region=c("PRI"))
	x <- fill_country_by_average_of_region(x, country="ALA", region=c("ISL","EST"))
	x <- fill_country_by_average_of_region(x, country="ATG", region=c("PRI"))
	x <- fill_country_by_average_of_region(x, country="AND", region=c("FRA","ESP"))
	x <- fill_country_by_average_of_region(x, country="ASM", region=c("NCL","WSM"))
	x <- fill_country_by_average_of_region(x, country="ATA", region=c("ISL","EST"))
	x <- fill_country_by_average_of_region(x, country="ATF", region=c("ISL","EST"))
	x <- fill_country_by_average_of_region(x, country="BES", region=c("PRI"))
	x <- fill_country_by_average_of_region(x, country="BLM", region=c("PRI"))
	x <- fill_country_by_average_of_region(x, country="BMU", region=c("PRI"))
	x <- fill_country_by_average_of_region(x, country="BVT", region=c("ISL","EST"))
	x <- fill_country_by_average_of_region(x, country="CCK", region=c("PRI"))
	x <- fill_country_by_average_of_region(x, country="COK", region=c("NCL","WSM"))												
	x <- fill_country_by_average_of_region(x, country="CUW", region=c("PRI"))
	x <- fill_country_by_average_of_region(x, country="CXR", region=c("NCL","WSM"))								
	x <- fill_country_by_average_of_region(x, country="CYM", region=c("PRI"))
	x <- fill_country_by_average_of_region(x, country="DMA", region=c("PRI"))								
	x <- fill_country_by_average_of_region(x, country="ESH", region=c("MRT","MLI"))
	x <- fill_country_by_average_of_region(x, country="FLK", region=c("ISL","EST"))
	x <- fill_country_by_average_of_region(x, country="FRO", region=c("ISL","EST"))
	x <- fill_country_by_average_of_region(x, country="GGY", region=c("ISL","EST"))
	x <- fill_country_by_average_of_region(x, country="GIB", region=c("GBR","ESP"))
	x <- fill_country_by_average_of_region(x, country="GRL", region=c("ISL","EST"))
	x <- fill_country_by_average_of_region(x, country="HMD", region=c("ISL","EST"))
	x <- fill_country_by_average_of_region(x, country="IMN", region=c("ISL","EST"))
	x <- fill_country_by_average_of_region(x, country="IOT", region=c("NCL","WSM"))
	x <- fill_country_by_average_of_region(x, country="JEY", region=c("ISL","EST"))
	x <- fill_country_by_average_of_region(x, country="KIR", region=c("NCL","WSM"))				
	x <- fill_country_by_average_of_region(x, country="KNA", region=c("PRI"))
	x <- fill_country_by_average_of_region(x, country="LIE", region=c("CHE","LUX"))					
	x <- fill_country_by_average_of_region(x, country="MAF", region=c("PRI"))								
	x <- fill_country_by_average_of_region(x, country="MCO", region=c("CHE","LUX"))
	x <- fill_country_by_average_of_region(x, country="MHL", region=c("NCL","WSM"))
	x <- fill_country_by_average_of_region(x, country="MNP", region=c("NCL","WSM"))#
	x <- fill_country_by_average_of_region(x, country="MSR", region=c("PRI"))#
	x <- fill_country_by_average_of_region(x, country="NFK", region=c("NCL","WSM"))
	x <- fill_country_by_average_of_region(x, country="NIU", region=c("NCL","WSM"))
	x <- fill_country_by_average_of_region(x, country="NRU", region=c("NCL","WSM"))
	x <- fill_country_by_average_of_region(x, country="PCN", region=c("NCL","WSM"))
	x <- fill_country_by_average_of_region(x, country="PLW", region=c("NCL","WSM"))
	x <- fill_country_by_average_of_region(x, country="SGS", region=c("ISL","EST"))
	x <- fill_country_by_average_of_region(x, country="SHN", region=c("ISL","EST"))
	x <- fill_country_by_average_of_region(x, country="SJM", region=c("ISL","EST"))
	x <- fill_country_by_average_of_region(x, country="SMR", region=c("CHE","LUX"))
	x <- fill_country_by_average_of_region(x, country="SPM", region=c("ISL","EST"))
	x <- fill_country_by_average_of_region(x, country="SSD", region=c("TCD","SDN"))
	x <- fill_country_by_average_of_region(x, country="SXM", region=c("PRI"))	
	x <- fill_country_by_average_of_region(x, country="SYC", region=c("MUS","MDV"))
	x <- fill_country_by_average_of_region(x, country="TCA", region=c("PRI"))
	x <- fill_country_by_average_of_region(x, country="TKL", region=c("NCL","WSM"))
	x <- fill_country_by_average_of_region(x, country="TUV", region=c("NCL","WSM"))
	x <- fill_country_by_average_of_region(x, country="TWN", region=c("KOR","HKG"))
	x <- fill_country_by_average_of_region(x, country="UMI", region=c("NCL","WSM"))
	x <- fill_country_by_average_of_region(x, country="VAT", region=c("CHE","LUX"))
	x <- fill_country_by_average_of_region(x, country="VGB", region=c("PRI"))
	x <- fill_country_by_average_of_region(x, country="WLF", region=c("NCL","WSM"))
	
	#change unit to million

	#x[is.nan(x)] <- 0
	#x[is.na(x)]  <- 0
	#NAs lead to problems later-on in the code. Please do a proper replacement as for the other countries!
	return(x)
}
