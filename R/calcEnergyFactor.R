#' @title calcEnergyFactor
#' @description provides generalizable energy after crops are processed into 1st generation biofuel
#' @return List of magpie object with results on global level, empty weight, unit and description.
#' @author Ewerton Araujo
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EnergyFactor")
#' }
#' @importFrom magpiesets findset
#' @export 


calcEnergyFactor<-function(){
  attribute <- calcOutput("Attributes", aggregate = F)
  b <- calcOutput("Processing_conversion_factors", aggregate = F)
  pro_factors <- b["GLO", "y2005", c("distilling.ethanol.tece", "distilling.ethanol.sugr_cane", "distilling.ethanol.maiz", "extracting.oils.groundnut", "extracting.oils.cottn_pro", "extracting.oils.rapeseed", "extracting.oils.soybean", "extracting.oils.sunflower", "extracting.oils.oilpalm")]
  y <- new.magpie(cells_and_regions = "GLO", years = NULL, names = c("tece", "sugr_cane", "maiz", "groundnut", "cottn_pro", "rapeseed", "soybean", "sunflower", "oilpalm", "betr", "begr"), fill = 0)
  y[,,"maiz"] <- as.numeric(pro_factors[,,"distilling.ethanol.maiz"])*as.numeric(attribute[,,"ge.ethanol"])
  y[,,"sugr_cane"] <- as.numeric(pro_factors[,,"distilling.ethanol.sugr_cane"])*as.numeric(attribute[,,"ge.ethanol"])
  y[,,"tece"] <- as.numeric(pro_factors[,,"distilling.ethanol.tece"])*as.numeric(attribute[,,"ge.ethanol"])
  y[,,"groundnut"] <- as.numeric(pro_factors[,,"extracting.oils.groundnut"])*as.numeric(attribute[,,"ge.oils"])
  y[,,"cottn_pro"] <- as.numeric(pro_factors[,,"extracting.oils.cottn_pro"])*as.numeric(attribute[,,"ge.oils"])
  y[,,"rapeseed"] <- as.numeric(pro_factors[,,"extracting.oils.rapeseed"])*as.numeric(attribute[,,"ge.oils"])
  y[,,"soybean"] <- as.numeric(pro_factors[,,"extracting.oils.soybean"])*as.numeric(attribute[,,"ge.oils"])
  y[,,"sunflower"] <- as.numeric(pro_factors[,,"extracting.oils.sunflower"])*as.numeric(attribute[,,"ge.oils"])
  y[,,"oilpalm"] <- as.numeric(pro_factors[,,"extracting.oils.oilpalm"])*as.numeric(attribute[,,"ge.oils"])
  y[,,"betr"] <- attribute[,,"ge.betr"]
  y[,,"begr"] <- attribute[,,"ge.begr"]
  return(list(x=y,weight=NULL,unit="PJ/Mt DM.",description="generalizable energy after crops are processed into 1st generation biofuel"))
}




