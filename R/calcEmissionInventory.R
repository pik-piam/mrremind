#' @title calcEmisNitrogenCroplandPast
#' @description providees an emission inventory for the past, either from external data or own estimates.
#'
#' @param datasource The Inventory that shall be used. Options are CEDS, combined_CEDS_IPCC (including own estimates where available), IPCC(own estimates), Nsurplus (own estimates)
#' @param mapping if emission inventory shall be aggregated, this indicates the mapping csv. if NULL, the original resolution will be used. 
#' @param from column in mapping
#' @param to column in mapping
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EmisNitrogenCroplandPast")
#' }


calcEmissionInventory<-function(datasource="CEDS",mapping="mappingCEDS59toSectors.csv",from="CEDS59",to="Sectors"){
  past<-findset("past")
  if (datasource=="CEDS"){
    
    # read CEDS emissions data from sources
    bc    <- readSource("CEDS",subtype="BC")
    #ch4   <- readSource("CEDS",subtype="CH4") #excluding ghg emissions, they were strange
    co    <- readSource("CEDS",subtype="CO")
    #co2   <- readSource("CEDS",subtype="CO2")#excluding ghg emissions, they were strange
    #n2o   <- readSource("CEDS",subtype="N2O") #excluding ghg emissions, they were strange
    nh3   <- readSource("CEDS",subtype="NH3")
    nox   <- readSource("CEDS",subtype="NOx")
    nmvoc <- readSource("CEDS",subtype="NMVOC")
    oc    <- readSource("CEDS",subtype="OC")
    so2   <- readSource("CEDS",subtype="SO2")
    

    # identify common years (starting in 1970 at least ch4 has less years that the other gases)
    y <- Reduce(intersect,list(getYears(bc),
                             #  getYears(ch4),
                               getYears(co),
                            #   getYears(co2),
                             #  getYears(n2o),
                               getYears(nh3),
                               getYears(nox),
                               getYears(nmvoc),
                               getYears(oc),
                               getYears(so2)))
    
    ceds <- mbind(bc[,y,],
                  #ch4[,y,],
                  co[,y,],
                  #co2[,y,],
                  #n2o[,y,],
                  nh3[,y,],
                  nox[,y,],
                  nmvoc[,y,],
                  oc[,y,],
                  so2[,y,]) / 1000 # kt -> Mt
    rm(bc,
       #ch4,
       co,
       #co2,
       #n2o,
       nh3,
       nox,
       nmvoc,
       oc,
       so2)
    
    # "past" may contain more years than ceds -> add missing years
    missing_y <- setdiff(past,y)
    ceds <- add_columns(ceds,missing_y,dim=2.1)
    
    # rename emissions according to map
    map2 <- c(BC="bc",
              #CH4="ch4",
              CO="co",
              #CO2="co2_c",
              #N2O="n2o_n",
              NH3="nh3_n",
              NOx="no2_n",
              NMVOC="nmhc",
              OC="oc",
              SO2="so2")
    getNames(ceds,dim=2) <- map2[getNames(ceds,dim=2)]
    
    # remove third entry "kt" in data dimension
    ceds <- collapseNames(ceds,collapsedim = 3)
    
    # add nitrate leaching with zeros (to keeep a unique format with other sources)
    ceds<-add_columns(ceds[,past,],addnm = "no3_n",dim=3.2)
    ceds[,,"no3_n"]<-0
    #ceds[,,"n2o_n"]<-0 # this data seems buggy
    
    
    # correct units
    #ceds[,,"n2o_n"]=ceds[,,"n2o_n"]/44*28
    ceds[,,"nh3_n"]=ceds[,,"nh3_n"]/17*14
    ceds[,,"no2_n"]=ceds[,,"no2_n"]/46*14
    #ceds[,,"co2_c"]=ceds[,,"co2_c"]/44*12
    out<-ceds
  } else if (datasource%in%c("combined_CEDS_IPCC")){

    ceds<-calcOutput("EmissionInventory",
                     datasource="CEDS",
                     mapping=NULL,
                     aggregate = FALSE)
    
    ipcc<-calcOutput("EmissionInventory",
                     datasource="IPCC",
                     mapping=NULL,
                     aggregate = FALSE)

    #replace<-c("3B_Manure-management","3D_Soil-emissions")
    #"3E_Enteric-fermentation","3F_Agricultural-residue-burning-on-fields","3D_Rice-Cultivation"
    #ag_emis_in_magpie<-c("4B","4D1","4D2")
    #ag_emis_in_magpie<-c("4B","4D1","4D2")
    #ag_emis_in_magpie<-c("4D1")
    ipcc<-add_columns(ipcc,addnm="n2o_n",dim = 3.2)
    ipcc[,,"n2o_n"]<-dimSums(ipcc[,,c("n2o_n_direct","n2o_n_indirect")])
    
    joint_emissions<-intersect(getNames(ipcc,dim=2),getNames(ceds,dim=2))
    ceds[,,"3B_Manure-management"][,,joint_emissions]<-ipcc[,,"awms"][,,joint_emissions]
    ceds[,,"3D_Soil-emissions"][,,joint_emissions]<-dimSums(ipcc[,,c("inorg_fert" ,"man_crop","resid","som","rice","pasture_soils")][,,joint_emissions],dim=3.1)
    out<-ceds
  } else if (datasource%in%c("combined_CEDS_Nsurplus2")){
    
    ceds<-calcOutput("EmissionInventory",
                     datasource="CEDS",
                     mapping=NULL,
                     aggregate = FALSE)
    
    ipcc<-calcOutput("EmissionInventory",
                     datasource="Nsurplus2",
                     mapping=NULL,
                     aggregate = FALSE)
    
    #replace<-c("3B_Manure-management","3D_Soil-emissions")
    #"3E_Enteric-fermentation","3F_Agricultural-residue-burning-on-fields","3D_Rice-Cultivation"
    #ag_emis_in_magpie<-c("4B","4D1","4D2")
    #ag_emis_in_magpie<-c("4B","4D1","4D2")
    #ag_emis_in_magpie<-c("4D1")
    
    ipcc<-add_columns(ipcc,addnm="n2o_n",dim = 3.2)
    ipcc[,,"n2o_n"]<-dimSums(ipcc[,,c("n2o_n_direct","n2o_n_indirect")])
    
    joint_emissions<-intersect(getNames(ipcc,dim=2),getNames(ceds,dim=2))
    ceds[,,"3B_Manure-management"][,,joint_emissions]<-collapseNames(ipcc[,,"awms"][,,joint_emissions])
    ceds[,,"3D_Soil-emissions"][,,joint_emissions]<-dimSums(ipcc[,,c("cropland_soils","pasture_soils")][,,joint_emissions],dim=3.1)
    out<-ceds
    
  } else if (datasource%in%c("IPCC","Nsurplus","Nsurplus2")){
    out<-calcOutput("EmisNitrogenPast",method=datasource,aggregate = FALSE)
  
  }  else if (datasource%in%c("combined_CEDS_PRIMAPhist")){
    
    if (to != "PRIMAPhist"){
      stop("This datasource is only available with the PRIMAPhist sectoral mapping. Please set argument to = 'PRIMAPhist'")
    }
    
    mapping <- NULL
    ceds<-calcOutput("EmissionInventory",
                     datasource="CEDS",
                     mapping=NULL,
                     aggregate = FALSE)
    
    primap<-readSource("PRIMAPhist", subtype = "hist")
    
    
    # find years overlap
    joint_years <- intersect(getYears(ceds), getYears(primap))
    
    # aggregate ceds categories to primap categories
    map  <- toolMappingFile("sectoral", "mappingCEDS59toPRIMAP.csv", readcsv = TRUE)

    ceds_agg <- toolAggregate(ceds, map, dim = 3.1)
    getSets(primap) <- getSets(ceds)
    getSets(ceds_agg) <- getSets(ceds)
    
    joint_emissions <- intersect(getNames(primap,dim=2),getNames(ceds_agg,dim=2))
    
    
    # use PRIMAP data for joint emissions
    ceds_emissions <- getNames(ceds_agg,dim=2)[!(getNames(ceds_agg,dim=2) %in% joint_emissions)]
    ceds_agg <- ceds_agg[,,ceds_emissions]
    
    out <- mbind(ceds_agg[,joint_years,], primap[,joint_years,])
    

  } else {stop("datasource unknown")}
  
  if(!is.null(mapping)){
    # aggregate and rename CEDS59 sectors to CEDS16 sectors      
    map  <- toolMappingFile("sectoral", mapping,readcsv = TRUE)
    
    # reduce ceds to available categories
    out<-out[,,getNames(out,dim=1)[getNames(out,dim=1)%in%map[,1]]]
    
    out<-groupAggregate(data=out,dim=3.1,query = map, from=from, to=to)
  }
  
  return(list(x=out,
              weight=NULL,
              unit="Mt",
              description="Emission inventories from various datasources. Nitrogen emissions are in N equivalents, CO2 in C equivalents, all other in their original molecular weight.")
  )
}