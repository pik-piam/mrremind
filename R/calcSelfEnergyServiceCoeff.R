#' Calculate Demand based on the output of the Demand Model
#' 
#' This function extracts the self energy service data from the source
#' NTNU_LCA.
#' 
#' 
#' @return self energy service coefficants and corresonding weights as a list
#' of two MAgPIE objects
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readNTNU_LCA}},
#' \code{\link{convertNTNU_LCA}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("SelfEnergyServiceCoeff")
#' 
#' }
#' 
calcSelfEnergyServiceCoeff <- function() {
   
 
    data <- readSource("NTNU_LCA")
    
    self <- data[,,"Self energy consumption",pmatch=TRUE]
        
    #### separate dimensions ###################
    getNames(self) <- gsub("\\|",".",getNames(self))
    getNames(self) <- gsub(" \\(.*\\)","",getNames(self))
        
    # reduce dimensions of spec und self 
    self <- collapseNames(self,collapsedim=3)
    
    # delete entries that are used for additional calculations done in calcEnergyServiceCoeff
    id <- c("Freight transportation","Iron","Cement")
    wanted <- getNames(self,dim=5)[!is.element(getNames(self,dim=5),id)]
    output <- self[,,wanted]
    
    output <- time_interpolate(output,c(2015,2020,2025,2035,2040,2045), integrate_interpolated_years=TRUE)
    
    weight <- new.magpie(getRegions(output),fill=1) 
    
    return(list(x=output,weight=weight))
}

