#' Calculate Demand based on the output of the Demand Model
#' 
#' This function calculates enery service coefficients based in the Self energy
#' consumption and Specific energy input for freight transportation, iron and
#' cement
#' 
#' 
#' @return Energy Service Coefficients and corresonding weights as a list of
#' two MAgPIE objects
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readNTNU_LCA}},
#' \code{\link{convertNTNU_LCA}}
#' @examples
#' 
#' \dontrun{ 
#' 
#' a <- calcOutput("EnergyServiceCoeff")
#' 
#' }
#' @importFrom magclass getNames<-
calcEnergyServiceCoeff <- function() {
   
 
    data <- readSource("NTNU_LCA")
    
    self <- data[,,"Self energy consumption",pmatch=TRUE]
    spec <- data[,,"Specific energy input",pmatch=TRUE]
    
    #### separate dimensions ###################
    # Self energy consumption
    getNames(self) <- gsub("\\|",".",getNames(self))
    getNames(self) <- gsub(" \\(.*\\)","",getNames(self))
    
    # Specific energy input
    getNames(spec) <- gsub("\\|",".",getNames(spec))
    getNames(spec) <- gsub(" \\(.*\\)","",getNames(spec))
    getNames(spec) <- gsub("Iron and steel","Iron",getNames(spec))         # use names of self
    getNames(spec) <- gsub("Cement and concrete","Cement",getNames(spec))  # use names of self
    
    # reduce dimensions of spec und self 
    spec <- collapseNames(spec,collapsedim=3)
    self <- collapseNames(self,collapsedim=3)
    
    # calculate output
    id <- getNames(spec,dim=3)
    output <- self[,,id]*spec
    
    output <- time_interpolate(output,c(2015,2020,2025,2035,2040,2045), integrate_interpolated_years=TRUE)
    
    weight <- new.magpie(getRegions(output),fill=1) 
    
    return(list(x=output,weight=weight))
}

