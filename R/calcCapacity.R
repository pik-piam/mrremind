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
calcCapacity <- function(subtype) {

  last_ts <- 2020

  if ((subtype == "capacityByTech_windoff") | (subtype == "capacityByTech")) {

    if (subtype == "capacityByTech_windoff"){
      description <- "Historical capacity by technology including offshore wind."

      # Use IRENA data for world renewables capacity.
      # Year: 2000-2017
      # Technologies: "csp", "geohdr", "hydro", "spv", "wind", "windoff"
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
    }
      else if (subtype == "capacityByTech"){
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
      }

    # renaming technologies to REMIND naming convention
    IRENAcap <- luscale::rename_dimnames(IRENAcap, dim = 3, query = mapping, from = "IRENA_techs", to="REMIND_techs")
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
    Openmodcap <- luscale::rename_dimnames(Openmodcap, dim = 3, query = mapping, from = "Openmod_techs", to="REMIND_techs")
    Openmodcap <- Openmodcap * 1E-03 # converting GW to TW

    # Use WEO 2017 data to additional countries: "USA","BRA","RUS","CHN","IND","JPN"
    # Year: 2015
    # Technologies: "tnrs","dot"
    WEOcap <- readSource(type="IEA_WEO",subtype="Capacity") # Read IEA WEO capacities
    WEOcap <- WEOcap[c("USA","BRA","RUS","CHN","IND","JPN"),2015,c("Nuclear","Oil")] # selecting data used on REMIND
    mapping <- data.frame( WEO_techs=c("Nuclear","Oil"),
                           REMIND_techs=c("tnrs","dot"), stringsAsFactors = FALSE)
    # renaming technologies to REMIND naming convention
    WEOcap <- luscale::rename_dimnames(WEOcap, dim = 3, query = mapping, from = "WEO_techs", to="REMIND_techs")
    WEOcap <- WEOcap * 1E-03 # converting GW to TW

    #    ***CG: fix CHA gas power capacities: 97 GW by September 2020 (Oxford Institute for Energy Studies:
    #    Natural gas in China’s power sector: Challenges and the road ahead
    #    (https://www.oxfordenergy.org/wpcms/wp-content/uploads/2020/12/Insight-80-Natural-gas-in-Chinas-power-sector.pdf)
    #    >60% gas plants are co-generation, rest are peaking
    #    *** for 2018-2022, take 90GW, 90GW*0.6=54, the rest is split between ngcc and ngt

    CHA.2020.GasData <- as.magpie(
      tribble(
        ~region,   ~year,   ~data,      ~value,
        "CHN",     2010,    "gaschp",   0.022,
        "CHN",     2015,    "gaschp",   0.05,
        "CHN",     2020,    "gaschp",   0.054,
        "CHN",     2010,    "ngcc",     0.001,
        "CHN",     2015,    "ngcc",     0.005,
        "CHN",     2020,    "ngcc",     0.01,
        "CHN",     2010,    "ngt",      0.003,
        "CHN",     2015,    "ngt",      0.016,
        "CHN",     2020,    "ngt",      0.026))

    # merge IRENA, Openmod and WEO capacities data
    output <- new.magpie(cells_and_regions=unique(c(getRegions(IRENAcap),getRegions(Openmodcap), getRegions(WEOcap), getRegions(CHA.2020.GasData))),
                         years = unique(c(getYears(IRENAcap),getYears(Openmodcap),getYears(WEOcap), getYears(CHA.2020.GasData))),
                         names = unique(c(getNames(IRENAcap),getNames(Openmodcap),getNames(WEOcap), getNames(CHA.2020.GasData))),
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

    output[getRegions(CHA.2020.GasData),getYears(CHA.2020.GasData), getNames(CHA.2020.GasData)] <- CHA.2020.GasData

    output[is.na(output)] <- 0 #set NA to 0
    output  <- toolCountryFill(output,fill=0,verbosity=0) # fill missing countries

  }
  else if (subtype == "capacityByPE") {
    # Pe -> peoil, pegas, pecoal, peur, pegeo, pehyd, pewin, pesol, pebiolc, pebios, pebioil
    description <- "Historical capacity by primary energy."

    # Secondary Energy Electricity capacities by primary energy source
    # Data for non-RE techs from Ember
    # Except coal, which comes from Global Coal Plant Tracker

    ## Primary Energies: "peur", "pecoal", "pecoal", "pegas", "pegas", "pehyd",
    ## "pewin", "pewin", "pesol", "pehyd", "pebiolc", "pesol", "peoil"
    # mapping <- data.frame( Openmod_techs=c("tnr", "pc", "lpc", "ngcc", "ngt", "hydro", "windon", "windoff", "spv",
    #                                        "psp", "biolcigcc", "csp", "oil"), #, "waste", "others"
    # REMIND_PE=c("peur", "pecoal", "pecoal", "pegas", "pegas", "pehyd", "pewin", "pewin",
    #             "pesol", "pehyd", "pebiolc", "pesol", "peoil"), stringsAsFactors = FALSE)

    mapping <- data.frame(ember_techs = c("Biomass", "Coal", "Gas", "Oil", "Hydro", "Nuclear", "Solar", "Wind"),
                          REMIND_PE=c("pebiolc", "pecoal", "pegas", "peoil", "pehyd", "peur", "pesol", "pewin"), stringsAsFactors = FALSE)

    embercap <- calcOutput("Ember", subtype = "capacity", aggregate = F)
    embercap <- setNames(embercap,
                         nm = gsub("Cap|Electricity|", "",
                                   gsub(" (GW)", "",
                                        getNames(embercap), fixed = TRUE), fixed = TRUE))

    # aggregating primary energies to REMIND naming convention
    embercap <- toolAggregate(embercap[,,mapping$ember_techs], rel=mapping, from="ember_techs",
                                to="REMIND_PE",dim=3.1)
    embercap <- embercap * 1E-03 # converting GW to TW

    embercap <- embercap[,,c("peur", "pebiolc", "pehyd", "pegas")] #pegas is handled at technology level

    output <- new.magpie(cells_and_regions=c(getRegions(embercap)), years = seq(2005, last_ts, 5),
                         names = c("pecoal", "pegas", "pebiolc", "pehyd", "peur"), fill=0)

    output  <- toolCountryFill(output,fill=0,verbosity=0) # fill missing countries

    # estimating lower bound coal capacity to remaining countries assuming
    # (1) capacity factors are given by REMIND pc capacity factor in 2015,
    # (2) generation is given by IEA 2015 generation values,
    # (3) all 2015 coal capacity is provided by the pc technology.
    # SB Use coal capacity data from Global Coal Plant Tracker (GCPT)

    # historical coal capacity data
    coal_hist <- readSource("GCPT",subtype="historical") * 1e-03
    # coal_hist <- coal_hist[,getYears(coal_hist)>="y2008",]

    # Fill in output with GCPT and Ember data, averaging across each 5 (or 3 or 4) year period
    ts_coal <- getYears(coal_hist, as.integer = TRUE)
    ts_ember <- getYears(embercap, as.integer = TRUE)

    for (yr in getYears(output, as.integer = TRUE)) {
      if ((yr+2) %in% ts_coal) {                      ## Fill in coal separately because data is more recent
        output[,yr,"pecoal"] <- dimSums(coal_hist[,(yr-2):(yr+2),],dim=2)/5
      }else if ((yr+1) %in% ts_coal) {
        output[,yr,"pecoal"] <- dimSums(coal_hist[,(yr-2):(yr+1),],dim=2)/4
      }else {
        output[,yr,"pecoal"] <- dimSums(coal_hist[,(yr-2):yr,],dim=2)/3
      }
      if ((yr+2) %in% ts_ember) {
        output[,yr,getItems(output,dim=3)!='pecoal'] <- dimSums(embercap[,(yr-2):(yr+2),],dim=2)/5
      }else if ((yr+1) %in% ts_ember) {
        output[,yr,getItems(output,dim=3)!='pecoal'] <- dimSums(embercap[,(yr-2):(yr+1),],dim=2)/4
      }else {
        output[,yr,getItems(output,dim=3)!='pecoal'] <- dimSums(embercap[,(yr-2):yr,],dim=2)/3
      }
    }
    output <- magclass::add_dimension(output, dim = 3.2, add = "enty", nm = "seel") # add secondary energy dimension

  } else if (subtype=="coal_pipeline") {
    output <- readSource("GCPT",subtype="future") * 1e-03
    description <- "Coal power project pipeline completion scenarios"

  } else {
    stop("Not a valid subtype!")
  }

  # Returning capacity values
  return(list(x=output, weight=NULL,
              unit="TW",
              description=description
  ))
}
