#' write TC damage parameters into input data
#' they are country-specific and should not be aggregated to the regional level at all

#' @author Franziska Piontek
#' @return MAgPIE object of damage parameters for country level tropical cyclone damage function

calcTCdamage <- function(subtype){
	if(subtype == "const"){
		output <- readSource("TCdamageKrichene",subtype="const")
		description <- "first (constant) damage coefficient for tropical cyclone damages"
		# average weight
		weight <- new.magpie(getRegions(output),getYears(output),getNames(output),fill=1)
	} else if (subtype == "tasK"){
		output <- readSource("TCdamageKrichene",subtype="tasK")
		description <- "second (linear in temperature) damage coefficient for tropical cyclone damages"
		# average weight
		weight <- new.magpie(getRegions(output),getYears(output),getNames(output),fill=1)
	}
	
	return(list(x         = output,
              weight      = weight,
              unit        = "dimensionless", 
              description = description))
}

