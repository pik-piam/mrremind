#' CalcCoalLabourProductivity
#' calculates historical employment in the coal sector and projects current (2020) and future employment factors (Jobs/PJ)
#' @author Aman Malik
#' @param subtype Either "Employment_factor" or "Employment"
#' @importFrom dplyr left_join

calcCoalLabourProductivity <- function(subtype){
  Year <- NULL
  emp <- calcOutput("ILO",subtype="all",aggregate = F)[,,"Coal and Lignite"]
  dias <- readSource("Dias", subtype = "Employment") # Employment data from Dias et al. for EU
  dias <- dias[,,"direct.Fuel_supply.Coal"]
  #regs <- setdiff(getRegions(dias)[which(dias>0)],c("HUN","ITA","GBR")) #countries in Europe with coal employment
  regs <- getRegions(dias)[which(dias>0)]
  dias <- dias[regs,,]
  dias <- collapseNames(dias)
  prod <- readSource(type = "BP",subtype = "Production") # coal production from BP data
  prod <- prod[,,c("Coal Production (EJ)","Coal Production (t)")]
  prod["ZAF","y2019","Coal Production (t)"] <- 258.5 # overwriting BP value with data from national source
  # eur <- dimSums(prod[regs,,c("Coal Production (t)","Coal Production (EJ")],dim = 1)# since EUR is not a region in BP, summing over all countries with coal production
  # getRegions(eur) <- "EUR" # making a region EUR
  # prod <- mbind(prod,eur)# adding it original data

  # Employment in coal mining from various local (not ILO) sources

  rus_loc <- new.magpie("RUS",years=c(2006:2014,2018),fill = c(169.5,191.4,190.9,173.8,168.8,168.6,168.2,163.1,154.7,146.9))
  rus_loc <- rus_loc*1000
  usa_loc <- new.magpie("USA",years = c(2012:2019),fill = c(84.65,78.1,73.3,64.05,50.73, 51.5,	51.59,	51.89))
  usa_loc <- usa_loc*1000 # source: BLS
  ind_loc <- new.magpie("IND",years = c(2019:2010),fill = c(292118,304386,	316210,	327750,	339867,	352282,	364736,	377447,	390243,	404744),names = "Coal")
  ind_loc <- ind_loc/0.8 # the above numbers are only for Coal India Limited which produces almost 80% of Indian coal.
  ind_loc <- magpiesort(ind_loc)# source: CIL operational statistics
  zaf_loc <- new.magpie("ZAF",years = c(2009:2019),fill=c(70791,74025,78580,83244,88039,86106,77747,77259,82372,89647,94297))# source mining south africa
  aus_loc <- new.magpie("AUS",c(2000:2019),fill=c(17.1,	20.6,	15,	20.4,	19.4,	26.9,	28.9	,23.2,	29,	32.4,	36,	47.6,	60,	46.4,	58,	38.7,	42.9,	47.3,	53.8,	57.9))
  aus_loc <- aus_loc*1000 # https://nationalindustryinsights.aisc.net.au/industries/mining-drilling-and-civil-infrastructure/coal-mining
  chn_loc <- new.magpie("CHN",years = c(2000,2005,2010,2015,2018),fill = c(3.99,4.36,5.27,4.43,3.21)) # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7442150/
  chn_loc <- chn_loc*1000000*0.93 # above numbers are actually total of coal power and coal mining. Assuming 7% of labour is coal mining
  #rus_prod <- new.magpie("RUS",years = c(1995,1997,1999,2001,2003,2005,2008,2009,2011,2012),fill = c(237,306, 449,563, 648,726,1151,1174,1350,1439))# from Rutovitz 2015
  #rus_loc <- prod["RUS",getYears(rus_prod),"Coal Production (t)"]*1000000/rus_prod

  all <- bind_rows(as.data.frame(usa_loc),as.data.frame(ind_loc),as.data.frame(zaf_loc),as.data.frame(aus_loc),
                   as.data.frame(chn_loc),as.data.frame(emp["IDN",c(2012:2015),"Coal and Lignite"]*1000),
                   as.data.frame(dias),as.data.frame(rus_loc)) %>%
    select(-1,-4) %>%
    mutate(Year=as.integer(as.character(Year)))

  x <- as.magpie(all,spatial=1,temporal=2)
  x <- magpiesort(x)
  x <- toolCountryFill(x, fill = 0, verbosity = 2)
  x[is.na(x)] <- 0


if (subtype=="Employment"){
    return(list(
      x=x,
      weight=NULL,
      unit="",
      description="Total Employment in coal and lignite sector"))
  }

if (subtype=="Employment_factor"){

  x_tmp <- x/(prod[getRegions(x),getYears(x),"Coal Production (EJ)"]*1000) # historical values

  prod <- prod[,,"Coal Production (EJ)"]*1000 # converting to PJ
  #ef <- x/prod[getRegions(x),getYears(x),]
  # employment factors in 2020, taking nearest historical value
  ef_fut <- new.magpie(c("AUS","CHN",regs,"IDN","IND","RUS","USA","ZAF"),seq(2020,2050,5),names = "EF")
  ef_fut[,2020,] <- c(4.4,39,rep(32,12),15,28.5,16,3.6,15.6)
  imp <- new.magpie(getRegions(ef_fut),seq(2025,2050,5))
  imp["AUS",c(2025,2030),] <- 1.5
  imp["AUS",seq(2035,2050,5),] <- -0.6
  imp["CHN",c(2025,2030),] <- -8
  imp["CHN",seq(2035,2050,5),] <- -3
  imp[regs,c(2025,2030),] <- -2.5
  imp[regs,seq(2035,2050,5),] <- -1.7
  imp["IDN",c(2025,2030),] <- -5
  imp["IDN",seq(2035,2050,5),] <- -3
  imp["IND",c(2025,2030),] <- -7
  imp["IND",seq(2035,2050,5),] <- -3
  imp["RUS",c(2025,2030),] <- -6
  imp["RUS",seq(2035,2050,5),] <- -3
  imp["USA",c(2025,2030),] <- -1.1
  imp["USA",seq(2035,2050,5),] <- -0.3
  imp["ZAF",c(2025,2030),] <- 1.3
  imp["ZAF",seq(2035,2050,5),] <- -1.8

  for (i in seq(2025,2030,5)){
  ef_fut[,i,] <- setYears(ef_fut[,2020,],NULL)*(1+0.01*imp[,i,])^(i-2020)
  }

  for (i in seq(2035,2050,5)){
  ef_fut[,i,] <- setYears(ef_fut[,2030,],NULL)*(1+0.01*imp[,i,])^(i-2020)
  }
  # # value for 2050
  # ef_fut[c("USA","AUS"),2050,] <- 4 # for USA and AUS, EF = 4 in 2050
  # ef_fut[setdiff(getRegions(ef_fut),c("USA","AUS")),"y2050",] <- 10   # for all others EF = 10 in 2050
  #
  # ef_fut <- time_interpolate(ef_fut,interpolated_year = seq(  2035,2045,5),integrate_interpolated_years = T)
  x_tmp <- x_tmp[getRegions(ef_fut),,]
  x_final <- new.magpie(cells_and_regions = getRegions(x_tmp),years = base::union(getYears(x_tmp),getYears(ef_fut)))
  x_final[,getYears(x_tmp),] <- x_tmp
  x_final[,getYears(ef_fut),] <- ef_fut
  x_final[is.na(x_final)] <- 0
  x_final <- toolCountryFill(x_final, fill = 0, verbosity = 2)
  x_final[c("ITA","SVK"),"y2015",] <- 32

    return(list(
    x=x_final,
    weight=x_final ,
    unit="Jobs/PJ",
    description="Employment factor projections"))

}

}

