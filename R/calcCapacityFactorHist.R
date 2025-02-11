#' @title calc Capacity Factor
#' @description provides capacity factor values
#'
#' @param subtype data subtype. Either "wind" or "windoff"
#'
#' @return magpie object of the capacity factor data
#' @author Renato Rodrigues, Stephen Bi
#' @examples
#' \dontrun{
#' calcOutput("CapacityFactor")
#' }
#'
calcCapacityFactorHist <- function(subtype){

  if (subtype == "wind") {

  #mapping of remind technology names to IRENA categories
  rem_Irena_map <- data.frame(rem=c("hydro","wind","spv","csp","bioigcc","geohdr"),
                              irena= c("Renewable hydropower","Wind","Solar photovoltaic","Concentrated solar power", "Bioenergy","Geothermal"))
  # Read capacity factor inputs
  hist_cap <- readSource(type="IRENA",subtype="Capacity")/1000 # converting from MW to GW
  hist_gen <- readSource("IRENA", subtype = "Generation")# Units are GWh
  # Calculate 2015 capacity factor for relevant technologies
  cf_realworld <- hist_gen[,2015,rem_Irena_map$irena]/(8760*hist_cap[,2015,rem_Irena_map$irena])
  #rename
  getNames(cf_realworld) <- rem_Irena_map$rem
  #check data
  max(cf_realworld[,,"hydro"],na.rm = T)
  max(cf_realworld[,,"wind"],na.rm = T) #INF spm, >1 AZE
  max(cf_realworld[,,"spv"],na.rm = T)
  max(cf_realworld[,,"csp"],na.rm = T)
  max(cf_realworld[,,"bioigcc"],na.rm = T) #>1 CHL, JPN, POL
  max(cf_realworld[,,"geohdr"],na.rm = T)

  #correct SPM infinite value
  cf_realworld[is.infinite(cf_realworld)] <- 0.8
  #correct AZE,CHL,JPN,POL >1 value
  cf_realworld[cf_realworld > 1] <- 0.8
  #get rid of NAs
  cf_realworld[is.na(cf_realworld)] <- 0


  #weight: historic generation
  hist_gen <- hist_gen[,2015,rem_Irena_map$irena]
  getNames(hist_gen) <- rem_Irena_map$rem
  hist_gen[is.na(cf_realworld)] <- 0

  return(list(x=cf_realworld, weight=hist_gen,
               unit="% of capacity",
               description="Installed capacity availability in 2015 - capacity factor (fraction of the year that a plant is running)"
  ))

  } else if (subtype == "windoff") {

    #mapping of remind technology names to IRENA categories
    tech_list = c("hydro","wind","windoff","spv","csp","bioigcc","geohdr")
    rem_Irena_map <- data.frame(rem=tech_list,
                                irena= c("Renewable hydropower","Onshore wind energy", "Offshore wind energy","Solar photovoltaic","Concentrated solar power", "Bioenergy","Geothermal"))
    # Read capacity factor inputs
    hist_cap <- readSource(type="IRENA",subtype="Capacity") / 1000 # converting from MW to GW
    hist_gen <- readSource("IRENA", subtype = "Generation")# Units are GWh

    # Calculate 2015 capacity factor for relevant technologies, using 2*generation(t) / (cap(t) + cap(t-1) )
    # then average over 5 years for 2015

    # load 5 years of generation
    hist_gen2 = hist_gen[,seq(2013,2017,1), rem_Irena_map$irena]%>%
      as.data.frame()
    # %>%
      # filter(.data$Data1 == "Offshore wind energy",.data$Region=="USA" )

    # load 5 years of capacity
    hist_cap2a = hist_cap[,seq(2012,2016,1), rem_Irena_map$irena] %>%
      as.data.frame() %>%
      mutate(Year = as.factor(as.integer(as.character(.data$Year)) + 1)) %>%
      select(.data$Year, .data$Region, .data$Data1, 'cap1' ='Value')

    # load 5 years of capacity
    hist_cap2b = hist_cap[,seq(2013,2017,1), rem_Irena_map$irena]%>%
      as.data.frame() %>%
      select(.data$Year, .data$Region, .data$Data1, 'cap2' ='Value')

    hist_cap2 <- full_join(hist_cap2b, hist_cap2a) %>%
      # filter(.data$Data1 == "Offshore wind energy",.data$Region=="USA" ) %>%
      mutate(cap = (.data$cap1 + .data$cap2) / 2) %>%
      select(.data$Year, .data$Region, .data$Data1, .data$cap)

    cf_year <- full_join(hist_gen2, hist_cap2) %>%
      mutate(Value = .data$Value / (8760 * .data$cap))

    cf_year$Value[cf_year$cap < 0.2] <- 0 # remove those CFs if the installed capacity are too tiny

    cf_year <- cf_year %>%
      select(.data$Year,.data$Region,.data$Data1,.data$Value) %>%
      as.magpie()

    cf_year[is.na(cf_year)] <- 0

    # cf_year <- as.data.frame(cf_year)

    #rename
    getNames(cf_year) <- rem_Irena_map$rem
    #check data
    max(cf_year[,,"hydro"],na.rm = T)
    max(cf_year[,,"wind"],na.rm = T)#INF spm, >1 AZE
    max(cf_year[,,"windoff"],na.rm = T)
    max(cf_year[,,"spv"],na.rm = T)
    max(cf_year[,,"csp"],na.rm = T)
    max(cf_year[,,"bioigcc"],na.rm = T) #>1 CHL, JPN, POL
    max(cf_year[,,"geohdr"],na.rm = T)

    #correct SPM infinite value
    cf_year[is.infinite(cf_year)] <- 0.8
    #correct AZE,CHL,JPN,POL >1 value
    cf_year[cf_year > 1] <- 0.8
    #get rid of NAs
    cf_year[is.na(cf_year)] <- 0

    # cf_year <- as.data.frame(cf_year)

    # averaging over 5 years for non-0 CFs
    cf_realworld_n0 <- cf_year %>%
      as.data.frame() %>%
      filter(.data$Value != 0) %>%
      group_by(.data$Region, .data$Data1) %>%
      summarise(Value=mean(.data$Value)) %>%
      mutate(Year="2015") %>%
      select(.data$Year,.data$Region,.data$Data1,.data$Value) %>%
      ungroup()

    # for regions and techs with 0 CFs for all 5 years
    cf_realworld_0 <- cf_year %>%
      as.data.frame() %>%
      group_by(.data$Region, .data$Data1) %>%
      summarise(Value=sum(.data$Value)) %>%
      filter(.data$Value == 0) %>%
      mutate(Year="2015") %>%
      select(.data$Year,.data$Region,.data$Data1,.data$Value) %>%
      ungroup()

    cf_realworld <- full_join(cf_realworld_n0, cf_realworld_0) %>%
      group_by(.data$Region, .data$Year) %>%
      mutate(Data1 = factor(.data$Data1, levels=tech_list) ) %>%
      ungroup() %>%
      as.magpie()
    # cf_realworld <- cf_realworld %>%
    #   as.data.frame()

    #weight: historic generation
    hist_gen <- hist_gen2 %>%
      select(.data$Year,.data$Region,.data$Data1,.data$Value) %>%
      group_by(.data$Region, .data$Data1) %>%
      summarise(Value=sum(.data$Value)) %>%
      mutate(Year="2015") %>%
      ungroup() %>%
      as.magpie()

    # hist_gen <- as.data.frame(hist_gen)
    getNames(hist_gen) <- rem_Irena_map$rem
    hist_gen[is.na(cf_realworld)] <- 0

    return(list(x=cf_realworld, weight=hist_gen,
                unit="% of capacity",
                description="Installed capacity availability in 2015 - capacity factor (fraction of the year that a plant is running)"
    ))
  }

}
