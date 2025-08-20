#' Calculate hydro potential
#'
#' Provides hydro potential data
#'
#'
#' @return hydro potential data and corresponding weights as a list of two
#' MAgPIE objects
#' @author Lavinia Baumstark
#' @seealso \code{\link{readWGBU}}, \code{\link{convertWGBU}}
#' @examples
#'
#' \dontrun{
#' calcOutput("PotentialHydro")
#' }
#'
calcPotentialHydro <- function() {

  # read hydro data
  wgbu <- readSource("WGBU")

  # technical potential
  techPot  <- wgbu[,,"Technisches Potenzial (TWh/a)"]
  # economic potential
  ecoPot  <- wgbu[,,"Wirtschaftlich es Potenzial (TWh/a)"]
  # produced electricity
  #
  # prodElec <- wgbu[,,"Erzeugter Strom(GWh/a)"] / 1000
  prodElec <- readSource("IRENA", "Generation")
  IRENA_hydro_cap <- readSource("IRENA", "Capacity") # in MW

  # Note: "Hydropower" contains renewable hydropower and mixed hydro plants, but
  # not pure pumped storage
  prodElec <- prodElec[,2015,"Hydropower"] / 1000
  IRENA_hydro_cap <- IRENA_hydro_cap[,2015,"Hydropower"]

  # ensure that overall potential can produce the generation of 2015, if not set
  # potential to IRENA 2015 generation
  checkDiff <- new.magpie(getRegions(techPot),NULL,fill = 0)
  for(r in getRegions(techPot)){
	  checkDiff[r,,] <- rowSums(techPot[r,,])-prodElec[r,,]
  }
  # find regions which need adjustment (techPot < prodElec)
  regions <- getRegions(checkDiff[which(checkDiff < 0)])
  # adjust regions
  for(r in regions){
	  techPot[r,,"Technisches Potenzial (TWh/a)"] <- prodElec[r,,]
  }

  # calculate rest of technical potential (minus installed capacity)
  restPot <- setNames(techPot - prodElec, "restPotential")

  restPot[restPot<0] <- 0 #making sure that we do not have negative potentials

  # calculate capacity factors for each country
  capFac <- setNames((prodElec / IRENA_hydro_cap) / 8760 * 1000000, "capacityFactor")

  # set capacity factor of regions with no installed capacity to 0
  capFac[is.na(capFac)] <- 0

  # produced electricity data with grade dimension
  prodElecGrade <- new.magpie(getRegions(prodElec),
                              getYears(prodElec),
                              c("1","2","3","4","5"),
                              fill=0)
  for(r in getRegions(prodElec)){
    if(capFac[r,,] > 0.5){
      prodElecGrade[r,,"1"] <- prodElec[r,,]
    } else if ((capFac[r,,] > 0.4) & (capFac[r,,] <= 0.5)) {
      prodElecGrade[r,,"2"] <- prodElec[r,,]
    } else if ((capFac[r,,] > 0.3) & (capFac[r,,] <= 0.4)) {
      prodElecGrade[r,,"3"] <- prodElec[r,,]
    } else if ((capFac[r,,] > 0.2) & (capFac[r,,] <= 0.3)) {
      prodElecGrade[r,,"4"] <- prodElec[r,,]
    } else if (capFac[r,,] <= 0.2) {
      prodElecGrade[r,,"5"] <- prodElec[r,,]
    }
  }

  for (r in getRegions(restPot)){
    if (any(prodElecGrade[r,,]!=0) & techPot[r,,]==0)
      techPot[r,,] <- 0.1
  }

  # rest of technical potential data with grade dimension
  restPotGrade  <- new.magpie(getRegions(restPot),
                              getYears(restPot),
                              c("1","2","3","4","5"),
                              fill=0)
  # allocate rest potential
  # (number of grades that can be filled depends on the grade of the installed capacity)
  # (3 categories depending on the share of installed capcity to the technical potential)
  for(r in getRegions(restPot)){
    cat(r,": ")
    if(all(prodElecGrade[r,,]==0)) {        # no installed capacity in this country
      cat("no installed capacity")
      if (restPot[r,,] == 0) {cat(", technical potential = 0", "\n")}
      restPotGrade[r,,"3"] <- 1/3 * restPot[r,,]
      restPotGrade[r,,"4"] <- 1/3 * restPot[r,,]
      restPotGrade[r,,"5"] <- 1/3 * restPot[r,,]
    } else {                                # some capacity installed
      if(which(prodElecGrade[r,,]!=0) == 1){          # installed capacity in the first grade
        cat("first grade", "\n")
        if(ecoPot[r,,]/techPot[r,,] <= 0.5){                                               # category 1
          restPotGrade[r,,"1"] <- 0.04 * restPot[r,,]
          restPotGrade[r,,"2"] <- 0.18 * restPot[r,,]
          restPotGrade[r,,"3"] <- 0.23 * restPot[r,,]
          restPotGrade[r,,"4"] <- 0.25 * restPot[r,,]
          restPotGrade[r,,"5"] <- 0.30 * restPot[r,,]
        } else if ((ecoPot[r,,]/techPot[r,,] > 0.5) & (ecoPot[r,,]/techPot[r,,] <= 0.75)){ # category 2
          restPotGrade[r,,"1"] <- 1/10 * restPot[r,,]
          restPotGrade[r,,"2"] <- 3/10 * restPot[r,,]
          restPotGrade[r,,"3"] <- 1/5 * restPot[r,,]
          restPotGrade[r,,"4"] <- 1/5 * restPot[r,,]
          restPotGrade[r,,"5"] <- 1/5 * restPot[r,,]
        } else if (ecoPot[r,,]/techPot[r,,] > 0.75){                                       # category 3
          restPotGrade[r,,"1"] <- 0.15 * restPot[r,,]
          restPotGrade[r,,"2"] <- 0.35 * restPot[r,,]
          restPotGrade[r,,"3"] <- 0.25 * restPot[r,,]
          restPotGrade[r,,"4"] <- 0.15 * restPot[r,,]
          restPotGrade[r,,"5"] <- 0.10 * restPot[r,,]
        }
      } else if (which(prodElecGrade[r,,]!=0) == 2) { # installed capacity in the 2nd grade
        cat("second grade","\n")
        if(ecoPot[r,,]/techPot[r,,] <= 0.5){                                               # category 1
          restPotGrade[r,,"2"] <- 0.20 * restPot[r,,]
          restPotGrade[r,,"3"] <- 0.25 * restPot[r,,]
          restPotGrade[r,,"4"] <- 0.25 * restPot[r,,]
          restPotGrade[r,,"5"] <- 0.30 * restPot[r,,]
        } else if ((ecoPot[r,,]/techPot[r,,] > 0.5) & (ecoPot[r,,]/techPot[r,,] <= 0.75)){ # category 2
          restPotGrade[r,,"2"] <- 1/4 * restPot[r,,]
          restPotGrade[r,,"3"] <- 1/4 * restPot[r,,]
          restPotGrade[r,,"4"] <- 1/4 * restPot[r,,]
          restPotGrade[r,,"5"] <- 1/4 * restPot[r,,]
        } else if (ecoPot[r,,]/techPot[r,,] > 0.75){                                       # category 3
          restPotGrade[r,,"2"] <- 0.30 * restPot[r,,]
          restPotGrade[r,,"3"] <- 0.25 * restPot[r,,]
          restPotGrade[r,,"4"] <- 0.25 * restPot[r,,]
          restPotGrade[r,,"5"] <- 0.20 * restPot[r,,]
        }
      } else if (which(prodElecGrade[r,,]!=0) == 3) { # installed capacity in the 3rd grade
        cat("third grade","\n")
        if(ecoPot[r,,]/techPot[r,,] <= 0.5){                                               # category 1
          restPotGrade[r,,"3"] <- 0.30 * restPot[r,,]
          restPotGrade[r,,"4"] <- 0.35 * restPot[r,,]
          restPotGrade[r,,"5"] <- 0.35 * restPot[r,,]
        } else if ((ecoPot[r,,]/techPot[r,,] > 0.5) & (ecoPot[r,,]/techPot[r,,] <= 0.75)){ # category 2
          restPotGrade[r,,"3"] <- 1/3 * restPot[r,,]
          restPotGrade[r,,"4"] <- 1/3 * restPot[r,,]
          restPotGrade[r,,"5"] <- 1/3 * restPot[r,,]
        } else if (ecoPot[r,,]/techPot[r,,] > 0.75){                                       # category 3
          restPotGrade[r,,"3"] <- 0.35 * restPot[r,,]
          restPotGrade[r,,"4"] <- 0.35 * restPot[r,,]
          restPotGrade[r,,"5"] <- 0.30 * restPot[r,,]
        }
      } else if (which(prodElecGrade[r,,]!=0) == 4) { # installed capacity in the 4th grade
        cat("fourth grade","\n")
        if(ecoPot[r,,]/techPot[r,,] <= 0.5){                                               # category 1
          restPotGrade[r,,"4"] <- 0.45 * restPot[r,,]
          restPotGrade[r,,"5"] <- 0.55 * restPot[r,,]
        } else if ((ecoPot[r,,]/techPot[r,,] > 0.5) & (ecoPot[r,,]/techPot[r,,] <= 0.75)){ # category 2
          restPotGrade[r,,"4"] <- 1/2 * restPot[r,,]
          restPotGrade[r,,"5"] <- 1/2 * restPot[r,,]
        } else if (ecoPot[r,,]/techPot[r,,] > 0.75){                                       # category 3
          restPotGrade[r,,"4"] <- 0.55 * restPot[r,,]
          restPotGrade[r,,"5"] <- 0.45 * restPot[r,,]
        }
      } else if (which(prodElecGrade[r,,]!=0) == 5) { # installed capacity in the 5th grade
        cat("fifth grade","\n")
        if(ecoPot[r,,]/techPot[r,,] <= 0.5){                                               # category 1
          restPotGrade[r,,"5"] <- 1.00 * restPot[r,,]
        } else if ((ecoPot[r,,]/techPot[r,,] > 0.5) & (ecoPot[r,,]/techPot[r,,] <= 0.75)){ # category 2
          restPotGrade[r,,"5"] <- 1/1 * restPot[r,,]
        } else if (ecoPot[r,,]/techPot[r,,] > 0.75){                                       # category 3
          restPotGrade[r,,"5"] <- 1.00 * restPot[r,,]
        }
      } # grade of installed capacity
    } # installed capacity yes/no
  } # r - countries


  maxprod <- restPotGrade + prodElecGrade

  # convert into EJ/a
  maxprod <- maxprod * 0.0036

  # add "nur" data, use categories from prodElecGrade
  nur <- new.magpie(getRegions(maxprod),getYears(maxprod),getNames(maxprod))
  nur[,,"5"] <- 0.15
  nur[,,"4"] <- 0.25
  nur[,,"3"] <- 0.35
  nur[,,"2"] <- 0.45
  nur[,,"1"] <- 0.55

  # put maxprod and nur together
  maxprod <- add_dimension(maxprod,dim=3.1,add="char",nm="maxprod")
  nur     <- add_dimension(nur,dim=3.1,add="char",nm="nur")
  data <- mbind(maxprod,nur)

  # create weight-matrix
  w <- new.magpie(getRegions(data),getYears(data),getNames(data),fill=1)
  w[,,"maxprod"] <- NA

  # delete temporal dimension for GAMS
  getYears(data) <- NULL

  ### specific countries: increase potential beyond NDC targets
  data[c("BEL","LUX","NLD","AUT"),,"maxprod"] <- data[c("BEL","LUX","NLD","AUT"),,"maxprod"]*1.3 # EWN
  data[c("CZE","EST","LVA","LTU","POL","SVK"),,"maxprod"] <- data[ c("CZE","EST","LVA","LTU","POL","SVK"),,"maxprod"]*1.365 #ECE
  data[c("GIB","GGY","IRL","IMN","JEY","GBR"),,"maxprod"] <- data[c("GIB","GGY","IRL","IMN","JEY","GBR"),,"maxprod"]*2.1 # UKI

  # FS: Australia reached full hydro potential today at about 0.07 EJ/yr
  # e.g. DII Australia 2015: http://www.eria.org/RPR_FY2014_No.33_Chapter_2.pdf
  # assignning all potential to fourth grade since this is the last grade used by REMIND
  data["AUS",,"maxprod"] <- 0
  data["AUS",,"maxprod.4"] <- 0.07

  # AM: slightly increase hydro potential for VTM to allow REN21 targets
  data["VNM",,"maxprod"] <- data["VNM",,"maxprod"]*1.05

  # FS: in Northern Central Europe region (ENC) potential is slightly below current capacity
  # increase potentials by 5% to avoid infeasibility with capacity targets in NDC2018 realization
  data["FIN",,"maxprod"] <- data["FIN",,"maxprod"]*1.05
  data["DNK",,"maxprod"] <- data["DNK",,"maxprod"]*1.05
  data["SWE",,"maxprod"] <- data["SWE",,"maxprod"]*1.05
  data["DEU",,"maxprod"] <- data["DEU",,"maxprod"]*1.05

  return(list(x                 = data,
              weight            = w,
              unit              = "EJ/a",
              description       = "hydro potential",
              mixed_aggregation = TRUE
  ))
}
