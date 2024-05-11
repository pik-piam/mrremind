#' calcSolar
#' calculate Area, Capacity and Energy for photovoltaics (PV) and contentrated solar power (CSP)
#'
#' @return magpie object
#'
#' @author Julian Oeser, modified by Renato Rodrigues
#' @seealso \code{\link{calcOutput}}
#' @examples
#'
#' \dontrun{ a <- calcOutput(type="Solar")
#' }
#'
#'
#' @importFrom utils head


calcSolar <- function() {

  x <- readSource("DLR")

  # calculate area values for CSP technology using formula: CSP capacity / (176.19 MW therm / km?)
  x[, , "CSP"][, , "area"] <- (x[, , "capacity"][, , "CSP"] / 176.19)

  # adjust CSP capacities using two correction factors: csp capacity * 0.37 (efficiency thermal -> electric) * 0.33 (scale down for solar multiple 3)

  x[, , "capacity"][, , "CSP"] <- (x[, , "capacity"][, , "CSP"] * 0.37 * 0.33)

  # Add rooftop potential for regions with limited PV potential - at the moment JPN and IND.
  # Data comes from Joshi et al, 2021:
  # Joshi, S., Mittal, S., Holloway, P., Shukla, P.R., Ó Gallachóir, B., Glynn, J., 2021. High resolution global spatiotemporal assessment of rooftop solar photovoltaics potential for renewable electricity generation. Nat Commun 12, 5738. https://doi.org/10.1038/s41467-021-25720-2
  # Capacities are derived in the regional .xlsx files in the DLR source folder by
  # 1) converting from area to capacity with the region-specific luse value;
  # 2) taking 50% of the total determined rooftop area as technically usable for PV plants
  # 3) downscaling CF by multiplying with 0.66 to represent higher costs for rooftop than for utility-scale
  # 4) upscaling capacities by dividing by 0.66 so that total energy stays unchanged
  # These steps are performed in JPN.XLSX/IND.XLSX files in sources/DLR, and the results added here by hand
  # (may be changed in the future if rooftop values for more countries should be added)

  # adding rooftop PV capacity in Japan in the respective Full Load hour bins. e.g. 729 means 729 kWh/kWp
  # The capacity values are upscaled by 1000 to go from GW to MW
  x["JPN", , "capacity.PV"][, , "729"] <- (x["JPN", , "capacity.PV"][, , "729"] + 1000 * 29)
  x["JPN", , "capacity.PV"][, , "771"] <- (x["JPN", , "capacity.PV"][, , "771"] + 1000 * 53)
  x["JPN", , "capacity.PV"][, , "812"] <- (x["JPN", , "capacity.PV"][, , "812"] + 1000 * 79)
  x["JPN", , "capacity.PV"][, , "854"] <- (x["JPN", , "capacity.PV"][, , "854"] + 1000 * 153)
  x["JPN", , "capacity.PV"][, , "895"] <- (x["JPN", , "capacity.PV"][, , "895"] + 1000 * 141)
  x["JPN", , "capacity.PV"][, , "937"] <- (x["JPN", , "capacity.PV"][, , "937"] + 1000 * 87)
  x["JPN", , "capacity.PV"][, , "979"] <- (x["JPN", , "capacity.PV"][, , "979"] + 1000 * 14)

  # adding respective area increases for rooftop PV in Japan in the respective bins,  using the luse value for Japan of 92
  x["JPN", , "area.PV"][, , "729"] <- (x["JPN", , "area.PV"][, , "729"] + 1000 / 92 * 29)
  x["JPN", , "area.PV"][, , "771"] <- (x["JPN", , "area.PV"][, , "771"] + 1000 / 92 * 53)
  x["JPN", , "area.PV"][, , "812"] <- (x["JPN", , "area.PV"][, , "812"] + 1000 / 92 * 79)
  x["JPN", , "area.PV"][, , "854"] <- (x["JPN", , "area.PV"][, , "854"] + 1000 / 92 * 153)
  x["JPN", , "area.PV"][, , "895"] <- (x["JPN", , "area.PV"][, , "895"] + 1000 / 92 * 141)
  x["JPN", , "area.PV"][, , "937"] <- (x["JPN", , "area.PV"][, , "937"] + 1000 / 92 * 87)
  x["JPN", , "area.PV"][, , "979"] <- (x["JPN", , "area.PV"][, , "979"] + 1000 / 92 * 14)

  # adding rooftop PV capacity in IND in the respective bins (upscaled by 1000 from GW to MW)
  x["IND", , "capacity.PV"][, , "895"]  <- (x["IND", , "capacity.PV"][, , "895"]  + 1000 * 30)
  x["IND", , "capacity.PV"][, , "937"]  <- (x["IND", , "capacity.PV"][, , "937"]  + 1000 * 110)
  x["IND", , "capacity.PV"][, , "979"]  <- (x["IND", , "capacity.PV"][, , "979"]  + 1000 * 259)
  x["IND", , "capacity.PV"][, , "1020"] <- (x["IND", , "capacity.PV"][, , "1020"] + 1000 * 103)
  x["IND", , "capacity.PV"][, , "1062"] <- (x["IND", , "capacity.PV"][, , "1062"] + 1000 * 306)
  x["IND", , "capacity.PV"][, , "1104"] <- (x["IND", , "capacity.PV"][, , "1104"] + 1000 * 63)
  x["IND", , "capacity.PV"][, , "1145"] <- (x["IND", , "capacity.PV"][, , "1145"] + 1000 * 31)

  # adding respective area increases for rooftop PV in IND in the respective bins,  using the luse value for IND of 102
  x["IND", , "area.PV"][, , "895"]  <- (x["IND", , "area.PV"][, , "895"]  + 1000 / 102 * 30)
  x["IND", , "area.PV"][, , "937"]  <- (x["IND", , "area.PV"][, , "937"]  + 1000 / 102 * 110)
  x["IND", , "area.PV"][, , "979"]  <- (x["IND", , "area.PV"][, , "979"]  + 1000 / 102 * 259)
  x["IND", , "area.PV"][, , "1020"] <- (x["IND", , "area.PV"][, , "1020"] + 1000 / 102 * 103)
  x["IND", , "area.PV"][, , "1062"] <- (x["IND", , "area.PV"][, , "1062"] + 1000 / 102 * 306)
  x["IND", , "area.PV"][, , "1104"] <- (x["IND", , "area.PV"][, , "1104"] + 1000 / 102 * 63)
  x["IND", , "area.PV"][, , "1145"] <- (x["IND", , "area.PV"][, , "1145"] + 1000 / 102 * 31)


  # calculate distance classes 50-100 and 100-inf based on differences between classes

  x <- add_columns(x, addnm = c("50-100", "100-inf"), dim = 3.3)
  x[, , "50-100"] <- x[, , "0-100"]-x[, , "0-50"]
  x[, , "100-inf"] <- x[, , "0-inf"]-x[, , "0-100"]

  # adjust for negative values in differences

  # print warning for countries where negative values make up more than 1% of positive values
  x.pos <- x
  x.pos[x.pos<0] = 0
  x.neg <- x
  x.neg[x.neg>0] = 0

  y  <- dimSums(x.neg[, , "50-100"], dim=3.4) / dimSums(x.pos[, , c("50-100",  "0-50")], dim=c(3.4, 3.3))
  countries.neg <- where(y< -0.01)$true$regions
  vcat(2, paste0("In the following countries negative values made up more than 1% in newly created distance bin 50-100: ", countries.neg))

  # set negative values to 0
  x[, , "50-100"][x[, , "50-100"]<0] <- 0
  x[, , "100-inf"][x[, , "100-inf"]<0] <- 0

  return(list(x=x,
              weight = calcOutput("FE", aggregate = FALSE)[, "y2015", "FE|Electricity (EJ/yr)"],
              unit = "Area in km2; Capacity factor in share of year; Energy in EJ",
              description = "Area (limitGeopot), Capacity factor (nur) and Energy (maxprod) for photovoltaics (spv) and contentrated solar power (csp)",
              aggregationFunction = toolSolarFunctionAggregate
  ))

}
