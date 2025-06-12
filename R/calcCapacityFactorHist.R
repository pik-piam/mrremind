#' @title calc Capacity Factor
#' @description provides capacity factor values
#'
#' @return magpie object of the capacity factor data
#' @author Renato Rodrigues, Stephen Bi, Fabrice Lécuyer
#' @examples
#' \dontrun{
#' calcOutput("CapacityFactor")
#' }
#'


calcCapacityFactorHist <- function(){
  refYear <- 2015
  description <- paste("Installed capacity availability in", refYear, ", or capacity factor (fraction of the year that a plant is running)")

  #mapping of remind technology names to IRENA categories
  mappingIRENA <- tibble::tribble(
    ~remind,   ~irena,
    "geohdr",  "Geothermal",
    "hydro",   "Hydropower", # contains renewable hydropower and mixed hydro plants, but not pure pumped storage
    "windon",  "Onshore wind energy",
    "windoff", "Offshore wind energy",
    "spv",     "Solar photovoltaic",
    "csp",     "Concentrated solar power",
    "bioigcc", "Bioenergy",
  )


  # Read capacity factor inputs
  histCapacity <- readSource(type = "IRENA", subtype = "Capacity") / 1000 # converting from MW to GW
  histCapacity <- histCapacity[,, mappingIRENA$irena]
  getNames(histCapacity) <- mappingIRENA$remind

  histGeneration <- readSource(type = "IRENA", subtype = "Generation") # Units are GWh
  histGeneration <- histGeneration[,, mappingIRENA$irena]
  getNames(histGeneration) <- mappingIRENA$remind

  # Calculate average capacity factor over 5 years around reference year
  # Capacity factor of year t is 2*generation(t) / (cap(t) + cap(t-1))

  # load 5 years of generation
  histGeneration = histGeneration[,seq(refYear-2,refYear+2,1),] %>% as.data.frame() %>%
    select("Year", "Region", "Technology" = "Data1", "generation" = "Value")

  # load 5 years of capacity at time t-1
  histCapacityBefore = histCapacity[,seq(refYear-3,refYear+1,1),] %>% as.data.frame() %>%
    mutate(Year = as.factor(as.integer(as.character(.data$Year)) + 1)) %>%
    select("Year", "Region", "Technology" = "Data1", "capacityBefore" = "Value")

  # load 5 years of capacity at time t
  histCapacityAfter = histCapacity[,seq(refYear-2,refYear+2,1),] %>% as.data.frame() %>%
    select("Year", "Region", "Technology" = "Data1", "capacityAfter" = "Value")

  histCapacity <- full_join(histCapacityBefore, histCapacityAfter) %>%
    mutate(capacity = (.data$capacityBefore + .data$capacityAfter) / 2) %>%
    select("Year", "Region", "Technology", "capacity")

  hoursPerYear <- 8760
  cf_year <- full_join(histGeneration, histCapacity) %>%
    mutate(Value = .data$generation / (hoursPerYear * .data$capacity))

  cf_year$Value[cf_year$capacity < 0.2] <- 0 # remove CFs if installed capacity is under 200MW
  cf_year$Value[cf_year$Value > 1] <- 0.8 # correct infinite values
  cf_year$Value[is.na(cf_year$Value)] <- 0 # correct NA values

  cf_year <- cf_year %>% select("Year", "Region", "Technology", "Value")

  # averaging over 5 years for non-0 CFs
  cf_realworld_n0 <- cf_year %>%
    filter(.data$Value != 0) %>%
    group_by(.data$Region, .data$Technology) %>%
    summarise(Value = mean(.data$Value)) %>%
    mutate(Year = toString(refYear)) %>%
    select("Year", "Region", "Technology", "Value") %>%
    ungroup()

  # for regions and techs with 0 CFs for all 5 years
  cf_realworld_0 <- cf_year %>%
    group_by(.data$Region, .data$Technology) %>%
    summarise(Value = sum(.data$Value)) %>%
    filter(.data$Value == 0) %>%
    mutate(Year = toString(refYear)) %>%
    select("Year", "Region", "Technology", "Value") %>%
    ungroup()

  cf_realworld <- full_join(cf_realworld_n0, cf_realworld_0) %>%
    mutate(Technology = factor(.data$Technology, levels = mappingIRENA$remind)) %>%
    arrange(.data$Year, .data$Region, .data$Technology) %>%
    as.magpie()

  # weight: historic generation
  histGeneration <- histGeneration %>%
    select("Year", "Region", "Technology", "generation") %>%
    group_by(.data$Region, .data$Technology) %>%
    summarise(Value = sum(.data$generation)) %>%
    mutate(Year = toString(refYear)) %>%
    ungroup() %>%
    as.magpie()

  histGeneration[is.na(cf_realworld)] <- 0

  # avoid zero weights, as they cause a warning in aggregation
  histGeneration[histGeneration == 0] <- 1e-10

  return(list(x = cf_realworld,
              weight = histGeneration,
              unit = "% of capacity",
              description = description
  ))
}
