#' @title calc RLDC Coefficients
#' @description provides RLDC coefficients values
#'
#' @param subtype Either 'LoB' or 'Peak'
#' @seealso \code{\link{calcOutput}}
#' @return magpie object of the RLDC coefficients data
#' @author Renato Rodrigues
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput(type="RLDCCoefficients",subtype='LoB')
#' }
#' 

calcRLDCCoefficients <- function(subtype="LoB"){
  
  if (!((subtype=="LoB") || (subtype=="Peak"))){
    stop("Not a valid subtype!")
  }
  
  # function to select which coefficients to use for each country based on the coefficients group that has a bigger value for the sum of the weight factor in the original data 
  coeffSelectFunction <- function(data, out, weight, uniqueCoeff){
    # create object with only unique elements (non duplicated data)
    coeff <- unique(data[,,uniqueCoeff])
    # get current region mapping
    map <- toolGetMapping(type = "regional", name = getConfig("regionmapping"), where = "mappingfolder")
    #loading output values based on coefficients with higher sum of weight factor in relation to unique elements (original region data)
    for (newRegion in unique(map$RegionCode)){ # loop through new region mapping
      totalPE <- new.magpie(getRegions(coeff), getYears(coeff),"TotalPE",fill=0) #initializing region dependent weight factor
      for (coeffSet in getRegions(coeff)){  # loop through unique set of coefficients
        for(country in map$CountryCode[map$RegionCode==newRegion] ) { # loop through all countries that belongs to the new region mapping
          if (identical(array(coeff[coeffSet,,uniqueCoeff]),array(data[country,,uniqueCoeff])) ){ #if coefficients of the country belongs to a specific set of unique coefficients sum its PE to the total PE of the unique coefficients combination
            totalPE[coeffSet,,"TotalPE"] = totalPE[coeffSet,,"TotalPE"] + as.vector(weight[country,,])
          }
        }
      }
      for(country in map$CountryCode[map$RegionCode==newRegion] ){ # asignning coefficents from original region with bigger PE value to countries of new region mapping
        out[country,,uniqueCoeff] <- array(coeff[which.max(totalPE[,,"TotalPE"]),,uniqueCoeff])
      }
    }
    return(out)
  }
  
  # Read RLDC Coefficients
  if (subtype=="LoB") {
    
    #loading coefficients data at country level
    data <- readSource("REMIND_11Regi", subtype="RLDCCoefficientsLoB")
    
    #creating output
    description <- "RLDC Load Bands Coefficients."
    output <- new.magpie(getRegions(data), getYears(data), getNames(data),fill=0)
    # filling intercept coefficients in output magPie object
    output[,,c("1.p00","2.p00","3.p00","4.p00")] <- data[,,c("1.p00","2.p00","3.p00","4.p00")]
    # filling non intercept coefficients (coeff1*X, coeff2*X2, coeff3*X3,...), whith values from the coefficients with the biggest sum of the weight factor
    # loading fe values to be used as weight for selecting the best fitting coefficient groups
    fe <- calcOutput("FE",aggregate=FALSE)
    #Selecting best fitted coefficients
    #updating output with selected coefficients for load Band 1
    output <- coeffSelectFunction(data,output,fe[,2005,"FE (EJ/yr)"],c('1.p10','1.p01','1.p20','1.p11','1.p02','1.p30','1.p21','1.p12','1.p03'))
    #updating output with selected coefficients for load Band 2
    output <- coeffSelectFunction(data,output,fe[,2005,"FE (EJ/yr)"],c('2.p10','2.p01','2.p20','2.p11','2.p02','2.p30','2.p21','2.p12','2.p03'))
    #updating output with selected coefficients for load Band 3
    output <- coeffSelectFunction(data,output,fe[,2005,"FE (EJ/yr)"],c('3.p10','3.p01','3.p20','3.p11','3.p02','3.p30','3.p21','3.p12','3.p03'))
    #updating output with selected coefficients for load Band 4
    output <- coeffSelectFunction(data,output,fe[,2005,"FE (EJ/yr)"],c('4.p10','4.p01','4.p20','4.p11','4.p02','4.p30','4.p21','4.p12','4.p03'))
    
    #weight
    weight <- new.magpie(getRegions(output), getYears(output), getNames(output))
    weight[,,c('1.p10','1.p01','1.p20','1.p11','1.p02','1.p30','1.p21','1.p12','1.p03','2.p10','2.p01','2.p20','2.p11','2.p02','2.p30','2.p21','2.p12','2.p03','3.p10','3.p01','3.p20','3.p11','3.p02','3.p30','3.p21','3.p12','3.p03','4.p10','4.p01','4.p20','4.p11','4.p02','4.p30','4.p21','4.p12','4.p03')] <- 1

  } else if (subtype=="Peak") {
    
    #loading coefficients data at country level
    data <- readSource("REMIND_11Regi", subtype="RLDCCoefficientsPeak")
    
    #creating output
    description <- "RLDC Load Bands Coefficients."
    output <- new.magpie(getRegions(data), getYears(data), getNames(data),fill=0)
    # filling intercept coefficients in output magPie object
    output[,,c("curt.p00","curtShVRE.p00","peak.p00","shtStor.p00","STScost.p00","STSRes2Cap.p00")] <- data[,,c("curt.p00","curtShVRE.p00","peak.p00","shtStor.p00","STScost.p00","STSRes2Cap.p00")]
    # filling non intercept coefficients (coeff1*X, coeff2*X2, coeff3*X3,...), whith values from the coefficients with the biggest sum of the weight factor
    # loading fe values to be used as weight for selecting the best fitting coefficient groups
    fe <- calcOutput("FE",aggregate=FALSE)
    #Selecting best fitted coefficients
    #updating output with selected coefficients for curt
    output <- coeffSelectFunction(data,output,fe[,2005,"FE (EJ/yr)"],c('curt.p10','curt.p01','curt.p20','curt.p11','curt.p02','curt.p30','curt.p21','curt.p12','curt.p03'))
    #updating output with selected coefficients for curtShVRE
    output <- coeffSelectFunction(data,output,fe[,2005,"FE (EJ/yr)"],c('curtShVRE.p10','curtShVRE.p01','curtShVRE.p20','curtShVRE.p11','curtShVRE.p02','curtShVRE.p30','curtShVRE.p21','curtShVRE.p12','curtShVRE.p03'))
    #updating output with selected coefficients for peak
    output <- coeffSelectFunction(data,output,fe[,2005,"FE (EJ/yr)"],c('peak.p10','peak.p01','peak.p20','peak.p11','peak.p02','peak.p30','peak.p21','peak.p12','peak.p03'))
    #updating output with selected coefficients for shtStor
    output <- coeffSelectFunction(data,output,fe[,2005,"FE (EJ/yr)"],c('shtStor.p10','shtStor.p01','shtStor.p20','shtStor.p11','shtStor.p02','shtStor.p30','shtStor.p21','shtStor.p12','shtStor.p03'))
    #updating output with selected coefficients for STScost
    output <- coeffSelectFunction(data,output,fe[,2005,"FE (EJ/yr)"],c('STScost.p10','STScost.p01','STScost.p20','STScost.p11','STScost.p02','STScost.p30','STScost.p21','STScost.p12','STScost.p03'))
    #updating output with selected coefficients for STSRes2Cap
    output <- coeffSelectFunction(data,output,fe[,2005,"FE (EJ/yr)"],c('STSRes2Cap.p10','STSRes2Cap.p01','STSRes2Cap.p20','STSRes2Cap.p11','STSRes2Cap.p02','STSRes2Cap.p30','STSRes2Cap.p21','STSRes2Cap.p12','STSRes2Cap.p03'))
    
    #weight
    weight <- new.magpie(getRegions(output), getYears(output), getNames(output))
    weight[,,c('curt.p10','curt.p01','curt.p20','curt.p11','curt.p02','curt.p30','curt.p21','curt.p12','curt.p03','curtShVRE.p10','curtShVRE.p01','curtShVRE.p20','curtShVRE.p11','curtShVRE.p02','curtShVRE.p30','curtShVRE.p21','curtShVRE.p12','curtShVRE.p03','peak.p10','peak.p01','peak.p20','peak.p11','peak.p02','peak.p30','peak.p21','peak.p12','peak.p03','shtStor.p10','shtStor.p01','shtStor.p20','shtStor.p11','shtStor.p02','shtStor.p30','shtStor.p21','shtStor.p12','shtStor.p03','STScost.p10','STScost.p01','STScost.p20','STScost.p11','STScost.p02','STScost.p30','STScost.p21','STScost.p12','STScost.p03','STSRes2Cap.p10','STSRes2Cap.p01','STSRes2Cap.p20','STSRes2Cap.p11','STSRes2Cap.p02','STSRes2Cap.p30','STSRes2Cap.p21','STSRes2Cap.p12','STSRes2Cap.p03')] <- 1
    
  }
  
  return(list(x=output, weight=weight,
              unit="ratio", 
              description=description,
              mixed_aggregation=TRUE              
  )) 
}  
