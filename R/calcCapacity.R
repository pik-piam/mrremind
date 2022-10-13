#' @title calc Capacity
#' @description provides historical capacity values in TW
#'
#' @param subtype data subtype. Either "capacityByTech" or "capacityByPE"
#' @return magpie object of  capacity data
#' @author Renato Rodrigues, Stephen Bi
#' @examples
#'
#' \dontrun{
#' calcOutput("Capacity",subtype="capacityByTech")
#' }
#'
#' @importFrom luscale rename_dimnames
#' @importFrom magclass add_dimension
#'

calcCapacity <- function(subtype){
  if (subtype == "capacityByTech_windoff") {

    description <- "Historical capacity by technology."

    # Use IRENA data for world renewables capacity.
    # Year: 2000-2017
    # Technologies: "csp", "geohdr", "hydro", "spv", "wind"
    IRENAcap <- readSource(type="IRENA",subtype="Capacity") # Read IRENA renewables capacity data
    IRENAcap <- IRENAcap[,,c("Concentrated solar power",
                             "Geothermal", "Hydropower",
                             "Solar photovoltaic",
                             "Onshore wind energy",
                             "Offshore wind energy"
                             )] # selecting data used on REMIND
    mapping <- data.frame(IRENA_techs=c("Concentrated solar power",
                                         "Geothermal",
                                         "Hydropower",
                                         "Solar photovoltaic",
                                         "Onshore wind energy",
                                         "Offshore wind energy"),
                          REMIND_techs=c("csp", "geohdr", "hydro", "spv", "wind", "windoff"),
                          stringsAsFactors = FALSE)
    # renaming technologies to REMIND naming convention
    IRENAcap <- rename_dimnames(IRENAcap, dim = 3, query = mapping, from = "IRENA_techs", to="REMIND_techs")
    IRENAcap <- IRENAcap * 1E-06 # converting MW to TW
    # overwriting Russia and Japan capacities for wind and spv to avoid REMIND convergence problems
    # (this is a temporary solution that should be removed once the bounds in REMIND are reworked)
    # IRENAcap["JPN",2010,"wind"] <- 0.0012
    # IRENAcap["RUS",2010,"spv"] <- 5e-06
    # IRENAcap["RUS",2015,"wind"] <- 2e-05
    # IRENAcap["RUS",2015,"spv"] <- 2e-05
    #

    # Use Openmod capacity values updated by the LIMES team for the European countries.
    # Year: 2015
    # Technologies: "tnrs","ngcc","ngt","dot"
    Openmodcap <- readSource(type="Openmod") # Read Openmod capacities
    # selecting data used on REMIND "BAL"
    Openmodcap <- Openmodcap[c("FIN","NOR","SWE","EST","LVA","LTU","DNK","GBR","IRL","NLD","POL",
                               "DEU","BEL","LUX","CZE","SVK","AUT","CHE","HUN","ROU","SVN","FRA",
                               "HRV","BGR","ITA","ESP","PRT","GRC"),,c("tnr","ngcc","ngt","oil")]
    mapping <- data.frame( Openmod_techs=c("tnr","ngcc","ngt","oil"),
                           REMIND_techs=c("tnrs","ngcc","ngt","dot"), stringsAsFactors = FALSE)
    # renaming technologies to REMIND naming convention
    Openmodcap <- rename_dimnames(Openmodcap, dim = 3, query = mapping, from = "Openmod_techs", to="REMIND_techs")
    Openmodcap <- Openmodcap * 1E-03 # converting GW to TW

    # Use WEO 2017 data to additional countries: "USA","BRA","RUS","CHN","IND","JPN"
    # Year: 2015
    # Technologies: "tnrs","dot"
    WEOcap <- readSource(type="IEA_WEO",subtype="Capacity") # Read IEA WEO capacities
    WEOcap <- WEOcap[c("USA","BRA","RUS","CHN","IND","JPN"),2015,c("Nuclear","Oil")] # selecting data used on REMIND
    mapping <- data.frame( WEO_techs=c("Nuclear","Oil"),
                           REMIND_techs=c("tnrs","dot"), stringsAsFactors = FALSE)
    # renaming technologies to REMIND naming convention
    WEOcap <- rename_dimnames(WEOcap, dim = 3, query = mapping, from = "WEO_techs", to="REMIND_techs")
    WEOcap <- WEOcap * 1E-03 # converting GW to TW

    # merge IRENA, Openmod and WEO capacities data
    output <- new.magpie(cells_and_regions=unique(c(getRegions(IRENAcap),getRegions(Openmodcap),getRegions(WEOcap))),
                         years = unique(c(getYears(IRENAcap),getYears(Openmodcap),getYears(WEOcap))),
                         names = unique(c(getNames(IRENAcap),getNames(Openmodcap),getNames(WEOcap))),
                         fill=0)
    output[getRegions(IRENAcap),getYears(IRENAcap),getNames(IRENAcap)] <- IRENAcap[getRegions(IRENAcap),
                                                                                   getYears(IRENAcap),
                                                                                   getNames(IRENAcap)]
    output[getRegions(Openmodcap),getYears(Openmodcap),getNames(Openmodcap)] <- Openmodcap[getRegions(Openmodcap),
                                                                                           getYears(Openmodcap),
                                                                                           getNames(Openmodcap)]
    output[getRegions(WEOcap),getYears(WEOcap),getNames(WEOcap)] <- WEOcap[getRegions(WEOcap),
                                                                           getYears(WEOcap),
                                                                           getNames(WEOcap)]

    output[is.na(output)] <- 0 #set NA to 0
    output  <- toolCountryFill(output,fill=0,verbosity=0) # fill missing countries

  } else if (subtype == "capacityByTech") {

    description <- "Historical capacity by technology."

    # Use IRENA data for world renewables capacity.
    # Year: 2000-2017
    # Technologies: "csp", "geohdr", "hydro", "spv", "wind"
    IRENAcap <- readSource(type="IRENA",subtype="Capacity") # Read IRENA renewables capacity data
    # selecting data used on REMIND
    IRENAcap <- IRENAcap[,,c("Concentrated solar power", "Geothermal", "Hydropower", "Solar photovoltaic", "Wind")]
    mapping <- data.frame(IRENA_techs=c("Concentrated solar power",
                                        "Geothermal", "Hydropower",
                                        "Solar photovoltaic",
                                        "Wind"),
                         REMIND_techs=c("csp", "geohdr", "hydro", "spv", "wind"),
                         stringsAsFactors = FALSE)
    # renaming technologies to REMIND naming convention
    IRENAcap <- rename_dimnames(IRENAcap, dim = 3, query = mapping, from = "IRENA_techs", to="REMIND_techs")
    IRENAcap <- IRENAcap * 1E-06 # converting MW to TW
    #overwriting Russia and Japan capacities for wind and spv to avoid REMIND convergence problems
    # (this is a temporary solution that should be removed once the bounds in REMIND are reworked)
    # IRENAcap["JPN",2010,"wind"] <- 0.0012
    # IRENAcap["RUS",2010,"spv"] <- 5e-06
    # IRENAcap["RUS",2015,"wind"] <- 2e-05
    # IRENAcap["RUS",2015,"spv"] <- 2e-05

    # Use Openmod capacity values updated by the LIMES team for the European countries.
    # Year: 2015
    # Technologies: "tnrs","ngcc","ngt","dot"
    Openmodcap <- readSource(type="Openmod") # Read Openmod capacities
    # selecting data used on REMIND # "BAL"
    Openmodcap <- Openmodcap[c("FIN","NOR","SWE","EST","LVA","LTU","DNK","GBR","IRL","NLD","POL",
                               "DEU","BEL","LUX","CZE","SVK","AUT","CHE","HUN","ROU","SVN","FRA",
                               "HRV","BGR","ITA","ESP","PRT","GRC"),,c("tnr","ngcc","ngt","oil")]
    mapping <- data.frame( Openmod_techs=c("tnr","ngcc","ngt","oil"),
                           REMIND_techs=c("tnrs","ngcc","ngt","dot"), stringsAsFactors = FALSE)
    # renaming technologies to REMIND naming convention
    Openmodcap <- rename_dimnames(Openmodcap, dim = 3, query = mapping, from = "Openmod_techs", to="REMIND_techs")
    Openmodcap <- Openmodcap * 1E-03 # converting GW to TW

    # Use WEO 2017 data to additional countries: "USA","BRA","RUS","CHN","IND","JPN"
    # Year: 2015
    # Technologies: "tnrs","dot"
    WEOcap <- readSource(type="IEA_WEO",subtype="Capacity") # Read IEA WEO capacities
    WEOcap <- WEOcap[c("USA","BRA","RUS","CHN","IND","JPN"),2015,c("Nuclear","Oil")] # selecting data used on REMIND
    mapping <- data.frame( WEO_techs=c("Nuclear","Oil"),
                           REMIND_techs=c("tnrs","dot"), stringsAsFactors = FALSE)
    # renaming technologies to REMIND naming convention
    WEOcap <- rename_dimnames(WEOcap, dim = 3, query = mapping, from = "WEO_techs", to="REMIND_techs")
    WEOcap <- WEOcap * 1E-03 # converting GW to TW

    # merge IRENA, Openmod and WEO capacities data
    output <- new.magpie(cells_and_regions=unique(c(getRegions(IRENAcap),getRegions(Openmodcap),getRegions(WEOcap))),
                         years = unique(c(getYears(IRENAcap),getYears(Openmodcap),getYears(WEOcap))),
                         names = unique(c(getNames(IRENAcap),getNames(Openmodcap),getNames(WEOcap))),
                         fill=0)
    output[getRegions(IRENAcap),getYears(IRENAcap),getNames(IRENAcap)] <- IRENAcap[getRegions(IRENAcap),
                                                                                   getYears(IRENAcap),
                                                                                   getNames(IRENAcap)]
    output[getRegions(Openmodcap),getYears(Openmodcap),getNames(Openmodcap)] <- Openmodcap[getRegions(Openmodcap),
                                                                                           getYears(Openmodcap),
                                                                                           getNames(Openmodcap)]
    output[getRegions(WEOcap),getYears(WEOcap),getNames(WEOcap)] <- WEOcap[getRegions(WEOcap),
                                                                           getYears(WEOcap),
                                                                           getNames(WEOcap)]

    output[is.na(output)] <- 0 #set NA to 0
    output  <- toolCountryFill(output,fill=0,verbosity=0) # fill missing countries

  } else if (subtype == "capacityByPE") {
    # Pe -> peoil, pegas, pecoal, peur, pegeo, pehyd, pewin, pesol, pebiolc, pebios, pebioil
    description <- "Historical capacity by primary energy."

    # Secondary Energy Electricity capacities by primary energy source

    ## Use Openmod capacity values updated by the LIMES team for the European countries.
    ## Year: 2015
    ## Primary Energies: "peur", "pecoal", "pecoal", "pegas", "pegas", "pehyd",
    ## "pewin", "pewin", "pesol", "pehyd", "pebiolc", "pesol", "peoil"
    EU_regi <- c("AUT","BEL","BGR","CZE","DEU","DNK","ESP","EST","FIN","FRA","GBR","GRC","HRV","HUN","IRL","ITA",
                 "LTU","LUX","LVA","NLD","POL","PRT","ROU","SVK","SVN","SWE")
    Openmodcap <- readSource(type="Openmod")[EU_regi,,] # Read Openmod capacities
    mapping <- data.frame( Openmod_techs=c("tnr", "pc", "lpc", "ngcc", "ngt", "hydro", "windon", "windoff", "spv",
                                           "psp", "biolcigcc", "csp", "oil"), #, "waste", "others"
                           REMIND_PE=c("peur", "pecoal", "pecoal", "pegas", "pegas", "pehyd", "pewin", "pewin",
                                       "pesol", "pehyd", "pebiolc", "pesol", "peoil"), stringsAsFactors = FALSE)
    # aggregating primary energies to REMIND naming convention
    Openmodcap <- toolAggregate(Openmodcap[,,mapping$Openmod_techs], rel=mapping, from="Openmod_techs",
                                to="REMIND_PE",dim=3.1)
    Openmodcap <- Openmodcap * 1E-03 # converting GW to TW

    # for now we are only using the Openmod capacities to European countries, non renewables capacities and hydro:
    # "pecoal", "pegas", "pebiolc", "peoil" (calcCapacityNuclear handles "peur"), "pehyd"
    Openmodcap <- Openmodcap[,,c("pecoal", "pebiolc", "peoil", "pehyd")] #pegas is handled at technology level

    ## Use WEO 2017 capacity values to additional countries: "USA","BRA","RUS","CHN","IND","JPN" and regions LAM and OAS
    ## Year: 2015
    ## Primary Energies: "peur", "pecoal", "pecoal", "pegas", "pegas", "pehyd", "pewin", "pewin", "pesol", "pehyd",
    ## "pebiolc", "pesol", "peoil"

    WEOcap <- readSource(type="IEA_WEO",subtype="Capacity") # Read IEA WEO capacities.
    # for now we are only using the WEO capacities for pecoal, pebiolc and pegas.
    WEOcap <- WEOcap[,,c("Coal","Bioenergy","Gas")]

    mapping <- data.frame( WEO_PE=c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Wind",
                                    "Geothermal", "Solar PV", "CSP", "Bioenergy"), #, "Marine"
                           REMIND_PE=c("pecoal", "peoil", "pegas", "peur", "pehyd",
                                       "pewin", "pegeo", "pesol", "pesol", "pebiolc"),
                           stringsAsFactors = FALSE)
    WEOcap <- toolAggregate(WEOcap, rel=mapping[which(mapping$WEO_PE %in% getNames(WEOcap)),], from="WEO_PE",
                            to="REMIND_PE",dim=3.1) # aggregating primary energies to REMIND naming convention
    WEOcap <- WEOcap * 1E-03 # converting GW to TW

    # merge Openmod and WEO capacities data
    out <- new.magpie(cells_and_regions=unique(c(getRegions(Openmodcap),getRegions(WEOcap))),
                         years = unique(c(getYears(Openmodcap),getYears(WEOcap))),
                         names = unique(c(getNames(Openmodcap),getNames(WEOcap))),
                         fill=0)
    out[getRegions(WEOcap),getYears(WEOcap),getNames(WEOcap)] <- WEOcap[getRegions(WEOcap),
                                                                        getYears(WEOcap),
                                                                        getNames(WEOcap)]
    out[getRegions(Openmodcap),getYears(Openmodcap),getNames(Openmodcap)] <- Openmodcap[getRegions(Openmodcap),
                                                                                        getYears(Openmodcap),
                                                                                        getNames(Openmodcap)]

    #filtering results -> for now we are only using capacities for all countries for c("pecoal", "pegas", "pebiolc"),
    # and additionaly for european countries and china for c("pehyd")
    output <- new.magpie(cells_and_regions=c(getRegions(out)), years = c(2010,2015,2020),
                         names = c("pecoal","pegas","pebiolc","pehyd"), fill=0)
    output[,2015,c("pecoal", "pegas", "pebiolc")] <- out[,2015,c("pecoal", "pegas", "pebiolc")]
    output[EU_regi,2010,c("pecoal", "pegas", "pebiolc")] <- out[EU_regi,2010,c("pecoal", "pegas", "pebiolc")]
    output[EU_regi,c(2010,2015),c("pecoal", "pegas", "pebiolc","pehyd")] <- out[EU_regi,c(2010,2015),
                                                                                c("pecoal", "pegas", "pebiolc","pehyd")]
    output["CHN",2015,"pehyd"] <- 0.319 # WEO data says ~319 GW in 2015

    output[is.na(output)] <- 0 #set NA to 0
    output  <- toolCountryFill(output,fill=0,verbosity=0) # fill missing countries

    output <- add_dimension(output, dim = 3.2, add = "enty", nm = "seel") # add secondary energy dimension

    # estimating lower bound coal capacity to remaining countries assuming
    # (1) capacity factors are given by REMIND pc capacity factor in 2015,
    # (2) generation is given by IEA 2015 generation values,
    # (3) all 2015 coal capacity is provided by the pc technology.
    # SB 09.2020 Using new coal capacity data from GCPT
    #coalGen <- calcOutput("IO",subtype="input",aggregate=FALSE)[,,"pecoal.seel.pc"]
    #capFac <- calcOutput("CapacityFactor", round=6,aggregate=FALSE)[,,"pc"]
    #capacity2015 <- coalGen[,2015,]/capFac[,2015,]* 1E-02
    #emptReg <- getRegions(output)[as.vector(output[,2015,"pecoal"]==0)]
    #output[emptReg,2015,"pecoal"] <- capacity2015[emptReg,2015,"pecoal.seel.pc"]

    #Global Coal Plant Tracker historical coal capacity data
    coal_hist <- readSource("GCPT",subtype="historical")
    coal_hist <- coal_hist[,getYears(coal_hist)>="y2008",]
    ts <- as.numeric(gsub("y","",getYears(coal_hist)))
    for (yr in getYears(output)) {
      yr <- as.numeric(gsub("y","",yr))
      if ((yr+2) %in% ts) {
        output[,yr,"pecoal"] <- dimSums(coal_hist[,(yr-2):(yr+2),],dim=2)/5 * 1e-03
      }else if ((yr+1) %in% ts) {
        output[,yr,"pecoal"] <- dimSums(coal_hist[,(yr-2):(yr+1),],dim=2)/4 * 1e-03
      }else {
        output[,yr,"pecoal"] <- dimSums(coal_hist[,(yr-2):yr,],dim=2)/3 * 1e-03
      }
    }
  }else if (subtype=="coal2025") {
    output <- readSource("GCPT",subtype="future") * 1e-03
    description <- "Post-COVID coal power capacity scenarios for 2025"


  }else if (subtype == "ember") {
    # get Ember data
    x <- readSource("Ember")

    # choose only capacity variables
    cap <- x[,,"GW"]

    # load and apply mapping to REMIND variables
    map <- toolGetMapping("Mapping_Ember.csv", type="reportingVariables") %>%
      filter(!is.na(!!sym("REMIND_Variable")), !!sym("REMIND_Variable") != "")  # remove incomplete mapping rows
    cap <- toolAggregate(cap, dim=3.1, rel=map, from = "Variable", to="REMIND_Variable", partrel = T, verbosity = 2)

    # convert GW to TW and move unit to variable name
    getNames(cap, dim=1) <- paste(getNames(cap, dim=1), "(TW)")
    cap <- collapseDim(cap, dim=3.2)
    output <- cap * 1e-03
    description <- "Capacities from the yearly Ember electricity data set"

  } else {
    stop("Not a valid subtype!")
  }

  # Returning capacity values
  return(list(x=output, weight=NULL,
              unit="TW",
              description=description
  ))
}
