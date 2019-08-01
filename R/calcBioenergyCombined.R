#' @title calcBioenergyCombined
#' @description Calculates projections of combined Bioenergy demand adding first and second generation demands based on data from 
#' Lotze Campen (2014) and from REMIND-MAgPIE-coupling. The Unit is Petajoule
#'  
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Ewerton Araujo
#' @seealso
#' \code{\link{calc1stBioDem}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("BioenergyCombined")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @importFrom magpiesets findset
#' @export
 
calcBioenergyCombined <- function() {
  
first_biodem_sep <- calcOutput("1stBioDem", aggregate = F)
second_biodem <- calcOutput("2ndBioDem", aggregate = F)


first_biodem_sep <- first_biodem_sep[,getYears(second_biodem), c("const2030.ethanol", "const2030.oils", "const2020.ethanol", "const2020.oils", "phaseout2020.ethanol", "phaseout2020.oils")]
first_biodem <- new.magpie(getRegions(first_biodem_sep), getYears(first_biodem_sep), names = c("const2030", "const2020", "phaseout2020"), fill = 0, sets = c("region", "t", "1stGenScen"))
first_biodem[,,"const2030"] <- first_biodem_sep[,,"const2030.ethanol"]+first_biodem_sep[,,"const2030.oils"]
first_biodem[,,"const2020"] <- first_biodem_sep[,,"const2020.ethanol"]+first_biodem_sep[,,"const2020.oils"]
first_biodem[,,"phaseout2020"] <- first_biodem_sep[,,"phaseout2020.ethanol"]+first_biodem_sep[,,"phaseout2020.oils"]
first_biodem <- add_dimension(first_biodem, dim = 3.2, add = "data", nm = "SSP1-Ref-SPA0")
first_biodem <- add_columns(first_biodem, addnm = c("SSP2-Ref-SPA0" ,"SSP5-Ref-SPA0", "SSP1-20-SPA0" , "SSP1-26-SPA0"  ,"SSP1-37-SPA0" , "SSP1-45-SPA0" , "SSP2-20-SPA0", "SSP2-26-SPA0" , "SSP2-37-SPA0" , "SSP2-45-SPA0",  "SSP2-60-SPA0",  "SSP5-20-SPA0",  "SSP5-26-SPA0" , "SSP5-37-SPA0",  "SSP5-45-SPA0", "SSP5-60-SPA0" , "SSP1-20-SPA1" , "SSP1-26-SPA1",  "SSP1-37-SPA1",  "SSP1-45-SPA1",  "SSP2-20-SPA2",  "SSP2-26-SPA2",  "SSP2-37-SPA2" , "SSP2-45-SPA2",  "SSP2-60-SPA2",  "SSP2-OS-SPA2" , "SSP5-20-SPA5" , "SSP5-26-SPA5",  "SSP5-37-SPA5",  "SSP5-45-SPA5" , "SSP5-60-SPA5" , "SSP5-OS-SPA5" ), dim = 3.2)
first_biodem[,,c(4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58,61,64,67,70,73,76,79,82,85,88,91,94,97)] <- first_biodem[,,1]
first_biodem[,,c(5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,74,77,80,83,86,89,92,95,98)] <- first_biodem[,,2]
first_biodem[,,c(6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,96,99)] <- first_biodem[,,3]

second_biodem <- add_dimension(second_biodem, dim = 3.1, add = "1stGenScen", nm ="const2030")
second_biodem <- add_columns(second_biodem, addnm = c("const2020", "phaseout2020"), dim = 3.1)
second_biodem[,,c(34:66)] <- second_biodem[,,c(1:33)]
second_biodem[,,c(67:99)] <- second_biodem[,,c(1:33)]

BioenergyCombined <- new.magpie(cells_and_regions = getRegions(second_biodem), years = getYears(second_biodem), getNames(second_biodem, fulldim = FALSE), fill = 0, sets = c("region", "t", "data"))
BioenergyCombined <- first_biodem + second_biodem

return(list(x=BioenergyCombined, weight=NULL,
            unit="PJ",
            description="Combined Bioenergy demand (first and second generation")
       )
}