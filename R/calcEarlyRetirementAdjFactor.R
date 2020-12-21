#' @title calc Early Retirement Adjustment Factor
#' @description provides the extra retirement rate to account for relatively old fleet technologies retirement
#' @param subtype Some scenarios may require certain regions to increase retirement rate, e.g. PPCA coal phase-out
#' @return magpie object of additional adjusment percentage to be added to the fraction of the early retired capital in countries to account for relatively old technologies fleet  
#' @author Renato Rodrigues
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput(type="EarlyRetirementAdjFactor")
#' }
#'  

calcEarlyRetirementAdjFactor <- function(subtype="none"){
  #loading early retirement adjustment factor data
  data <- readSource("REMIND_11Regi", subtype="earlyRetirementAdjFactor")
  
  ### SB 09.2020 
  ### Increasing China's maximum retirement rate to match the EU and US (13%)
  ### China's weighted average plant lifespan was half the global average for plants retired in 2015-2020, due to technology efficiency upgrades,
  ### overcapacity (ongoing), and mismatched local and national political agendas. Plants are permitted by local authorities but can be retired by
  ### federal authorities. We expect the trend to continue to some degree because of the CPC's recent climate commitments and their political longevity.
  data["CHN",,] <- data["USA",,]
  
  ### SB 12.2020
  ### Also increasing REF's retirement rate to match EU and US since they have an even older coal fleet
  data["RUS",,] <- data["USA",,]
  data["UKR",,] <- data["USA",,]
  data["UZB",,] <- data["USA",,]
  data["KAZ",,] <- data["USA",,]
  data["KGZ",,] <- data["USA",,]
  data["MDA",,] <- data["USA",,]
  
  #loading weight factor
  IO <- calcOutput("IO",subtype="input",aggregate=FALSE)
  weight <- NULL
  weight <- dimSums(IO[,2005,getNames(data)],dim=3)
  weight <- setNames(weight,getNames(data))

  return(list(x=data, weight=weight,
               unit="percentage", 
               description="extra retirement rate for technologies in countries with relatively old fleet",
              mixed_aggregation=TRUE              
   )) 
}