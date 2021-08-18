#' toolSolarFunctionAggregate
#' 
#' Aggregate Solar data into regions
#'  
#' @param x magclass object that should be aggregated
#' @param rel relation matrix containing a region mapping.
#' A mapping object should contain 2 columns in which each element of x
#' is mapped to the category it should belong to after (dis-)aggregation
#' 
#' @return return: returns region aggregated solar data
#' 
#' @author Felix Schreyer, Renato Rodrigues, Julian Oeser
#' @export
#' @importFrom magclass is.magpie as.data.frame as.magpie collapseNames add_columns getSets getItems
#' setItems collapseDim
#' @importFrom dplyr %>% mutate select rename filter left_join group_by ungroup arrange summarise desc 
#' lag full_join
#' @importFrom tidyr spread gather complete
#' @importFrom quitte as.quitte revalue.levels
#' @importFrom zoo na.approx
#' @importFrom stats weighted.mean


toolSolarFunctionAggregate <- function(x, rel=NULL){
  
  
  # old part by Julian Oeser
  
  # aggregate to regions
  x <- toolAggregate(x,rel)
  getSets(x)[1] <- "Region"
  
  ### split up pv in (needed?)
  
  # PVall (install PV everywhere),
  # PVcomp (install PV only where also csp could be installed) (FS: Why should one want to do this???),
  # PVonly (install PV only where no csp can be installed)
  
  # installable area for csp, pv in region
  area.pv <- dimSums(x[,,"area"][,,"PV"][,,c("0-50", "50-100")], dim=c(3.4,3.3)) # sum over bins and distance classes
  area.csp <- dimSums(x[,,"area"][,,"CSP"][,,c("0-50", "50-100")], dim=c(3.4, 3.3))
  
  # share of area if PV installed only where no csp can be installed
  area.only.pv.share <- collapseDim(((area.pv+1)-(area.csp+1)) / (area.pv+1))

  xPVonly <- setItems(x[,,"PV"]*area.only.pv.share, dim = "Technology", value = "PVonly")
  xPVcomp <- setItems(x[,,"PV"]-(x[,,"PV"]*area.only.pv.share), dim = "Technology", value = "PVcomp")

  x <- mbind(x, xPVonly, xPVcomp)
  
  
  ### create new distance class: 1-100red (distance class where all between 1-100 are included and 
  # far away distanced classes are reduced in FLH which accounts for transmission losses)
  bins.pv <- getNames(x[,,"PV"], dim = 4)
  bins.csp <- getNames(x[,,"CSP"], dim = 4)
  
  x1.pv <- x[,,"0-50"][,,"PV"][,,bins.pv]
  x1.csp <- x[,,"0-50"][,,"CSP"][,,bins.csp]
  
  x2.pv <- x[,,"50-100"][,,"PV"][,,bins.pv]
  x2.csp <- x[,,"50-100"][,,"CSP"][,,bins.csp]
  
  # equivalents of bins in the two distance classes
  offset.pv <- 4
  offset.csp <- 2
  
  bins.pv.d2 <- c(rep(head(bins.pv, 1), offset.pv), bins.pv[1:(length(bins.pv)-offset.pv)])
  bins.pv.agg <- cbind(bins.pv, bins.pv.d2)
  colnames(bins.pv.agg) <- c("d1", "d2")
  
  bins.csp.d2 <- c(rep(head(bins.csp, 1), offset.csp), bins.csp[1:(length(bins.csp)-offset.csp)])
  bins.csp.agg <- cbind(bins.csp, bins.csp.d2)
  colnames(bins.csp.agg) <- c("d1", "d2")
  
  x2.pv <- toolAggregate(x2.pv, rel=bins.pv.agg, dim=3.4)
  getSets(x2.pv) <- getSets(x1.pv)
  x2.csp <- toolAggregate(x2.csp, rel=bins.csp.agg, dim=3.4)
  getSets(x2.csp) <- getSets(x1.csp)
  
  .missingBins <- function(x1,x2) {
    bin1 <- getItems(x1, dim = "Bin")
    bin2 <- getItems(x2, dim = "Bin")
    return(bin1[which(!(bin1 %in% bin2))])
  }
  missing.bins.pv  <- .missingBins(x1.pv,  x2.pv)
  missing.bins.csp <- .missingBins(x1.csp, x2.csp)
  
  x2.pv <- add_columns(x2.pv, missing.bins.pv, 3.4)
  x2.pv[,,missing.bins.pv] <- 0
  
  x2.csp <- add_columns(x2.csp, missing.bins.csp, 3.4)
  x2.csp[,,missing.bins.csp] <- 0
  
  # do summation
  x <- add_columns(x, "1-100red", 3.3)
  x[,,"PV"][,,"1-100red"] <- x1.pv[,,"0-50"]+ x2.pv[,,"50-100"]
  x[,,"CSP"][,,"1-100red"] <- x1.csp[,,"0-50"]+ x2.csp[,,"50-100"]
  
  
  
  # emtpy declarations of variables used in dplyr operations
  # (needed to make dplyr work within library)
  
  region <- NULL
  Type <- NULL
  Technology <- NULL
  Distance <- NULL
  FLH <- NULL
  value <- NULL
  Bin <- NULL
  capacity <- NULL
  area <- NULL
  maxprod <- NULL
  production <- NULL
  maxprod.norm <- NULL
  TotalPot <- NULL
  diff <- NULL
  cumulative <- NULL
  grade <- NULL
  grade.seq <- NULL
  maxprod.weight <- NULL
  Seclast <- NULL
  Max <- NULL
  Min <- NULL
  
  
  
  ### FS: map and aggregate FLH bins from DLR data to REMIND grades
  
  # method: 
  # 1. cumulative potential (maxprod) is calculated in descending order of FLHs 
  # 2. cumulative potential is normalized by current (2015) FE electricity consumption (IEA)
  # 3. cumulative potential vs. FLH is interpolated between given data points step-wise and linearly
  # to have at least one point in each REMIND grade
  # 4. cumulative potential is mapped to REMIND grades by dividing it according to grade.breaks parameter (e.g. if 1. grade:
  # 0-0.2, that means that the top 20%  of locations are mapped into the first REMIND grade)
  # 5. all potential that is above the last value of grade breaks are mapped into the last grade
  # 6. an exception is made for regions/technologies where total potential is below the lower bound of the last grade s.t.
  # last grade would be emtpy -> here the grades are determined by equally spaced bins of potential up to the total potential (e.g. for Japan)
  
  
  # set parameters for bin to grade mapping

  grade.breaks <- c(0.1,0.3,0.6,1,2,5,10,20) # grade breaks normalized by current production 
  n.intp <- 400 # number of interpolation points between given bins (to make sure at least one value is assigned to every REMIND grade)
  thres.offset <- 30 # interpolation will be applied up to lower bound of last grade + offset (unit: normalized cumulative production)
                    # for assuring that enough data points are there for interpolation up to lower bound of last grade
  
  techs <- c("CSP", "PV") # technologies to aggregate
  dist <- "1-100red" # distance class to aggregate

  # get 2015 FE electricity from IEA for iso-countries
  IEA.FE <- calcOutput("FE")[,"y2015","FE|Electricity (EJ/yr)"]

  # reference for grade distinction = 2015 production
  # assign 0.01 EJ/yr as reference for grade distinction to countries with zero 2015 production
  MaxProd.Norm <- IEA.FE
  MaxProd.Norm[MaxProd.Norm == 0] <- 0.01 

  # convert data to quitte format because more convenient for the following operations
  df.x <- as.quitte(x[,,techs][,,dist]) %>% 
    # drop 0 and NA potentials 
    #filter(value > 0 , !is.na(value)) %>% 
    # rename Bin to Full load hours, convert to numerical
    mutate(FLH = as.numeric(as.character(Bin))) %>%
    select(region, Type, Technology, Distance, value, FLH)
  
  
  # calculate maxprod (potential/maximum possible production in grade) in GWh by: capacity * FLH (Full Load Hours) / 1000
  df.pot  <- df.x %>% 
    spread(Type, value) %>% 
    mutate( maxprod = capacity * FLH / 1000) %>% 
    # remove those locations with NA or 0 area or potential
    filter( maxprod > 0, area > 0) %>% 
    gather(Type, value, capacity, area, maxprod)
  
  # calculate cumulated capacity, maxprod (potential) and area in descending grade order 
  # to obtain how much can be produced in total at this FLH or higher FLH 
  df.cuml <- df.pot %>%
    group_by(region, Type, Technology, Distance) %>%
    arrange(desc(FLH)) %>%
    mutate( value = cumsum(value)) %>%
    ungroup()
  
  # process 2015 IEA electricity production
  df.prod.2015 <- as.quitte(MaxProd.Norm) %>% 
    # FE 2015 in GWh/yr
    mutate( production = value / 3.6 * 1e6) %>% 
    select(region, production)
  
  
  # calculate cumulative normalized potential 
  # noramlized by current (2015) production
  df.maxprod.norm <- df.cuml %>%
    filter(Type == "maxprod") %>%
    left_join( df.prod.2015) %>%
    mutate(maxprod.norm = value/production) %>% 
    select(region, Technology, Distance, FLH, maxprod.norm)
  
  ## join normalized cumulative maxprod agin to df with cumulative maxprod, area, capacity 
  #to be able to interplolate all three measures
  df.cuml <- df.cuml %>%
    spread(Type, value) %>%
    left_join(df.maxprod.norm)
  
  # get total potential per region, technology, distance,
  # needed for distinguishing mapping of fine grades to REMIND grades later
  df.totalPot <- df.cuml %>% 
    group_by(region, Technology, Distance) %>% 
    summarise( TotalPot  = max(maxprod.norm))
  
  
  # define function needed for interpolation inbetween given points
  # returns vector x appended by n  equally spaced values between those points
  # x: x coordinates of given (x,y) points
  # y: difference between neighboring x coordinates
  # n: number of points
  seq.between <- function(x,y,n) {
    fac <- 1:n/(n+1)
    out <- x
    for (i in 1:length(fac)) {
      out <- append(out, x-y*fac[i])
    }
    return(out)
  }
  
  # interpolate cumulative normalized potential vs. FLH step-wise linearly between the given data points to obtain fine grades
  df.interpolate <- df.cuml %>%
    # filter for potential below last grade, the rest will go into last grade, 
    # add offset (thres.offset) to obtain values above the lower bound of the last grade
    # to assure that values are interpolated at least up to this lower bound 
    filter( maxprod.norm < grade.breaks[length(grade.breaks)]+thres.offset) %>%
    group_by(region, Technology, Distance) %>%
    # order by descending FLH
    arrange(desc(FLH)) %>% 
    # recompute potential as difference between normalized potential of neighboring FLH bins 
    mutate( diff = maxprod.norm - lag(maxprod.norm)) %>% 
    mutate( diff = ifelse(is.na(diff), maxprod.norm, diff)) %>% 
    # expand by n.intp equally spaced values between the given maxprod.norm values
    complete(maxprod.norm = seq.between(maxprod.norm, diff, n.intp)) %>% 
    # interpolate maxprod, area, capacity linearly at new data points between given data points
    mutate(FLH =  na.approx(FLH, na.rm = F), maxprod = na.approx(maxprod, na.rm = F), 
           area = na.approx(area, na.rm = F), capacity = na.approx(capacity, na.rm = F)) %>% 
    # remove NAs that were appended above FLHs of first data point
    filter( !is.na(FLH))
  
  # transform cumulative values (maxprod, area, capacity) of fine grades to values per fine grade
  # by performing gradient operation
  df.fine.grades <- df.interpolate %>% 
    select(region, Technology, Distance, FLH, maxprod.norm, maxprod, capacity, area) %>% 
    gather(Type, value, maxprod, capacity, area) %>%  
    group_by(region, Type, Technology, Distance) %>% 
    arrange(desc(FLH)) %>% 
    # calculate value per fine grade by gradient of cumulative value 
    mutate( diff = value - lag(value)) %>% 
    # highest FLH has no predecessor, so take diff to 0, i.e. value itself
    mutate( diff = ifelse(is.na(diff), value, diff)) %>% 
    ungroup() %>% 
    # relabel: value is now the potential in this fine grade, 
    # while the other is the cumulative value incl. all grades with higher FLH
    rename( cumulative = value, value = diff)
  
  ## for regions/technologies with total potential WITHIN last grade:
  # assign REMIND grades to interpolated FLH bins according to grade breaks of normalized potential defined above
  df.map.REMIND <- df.fine.grades %>% 
    select(region, Technology, Distance, Type, maxprod.norm, FLH, value) %>%
    mutate(grade = .bincode(maxprod.norm, c(0,grade.breaks))) %>% 
    # filter out NA grades (maxprod.norm greater than largest element in grade.breaks), 
    # this potential would be in last grade
    filter(!is.na(grade))
  
  
  ## for regions/technologies with total potential BELOW last grade (so that last grade would be empty):
  # map to equally sized grades up to total potential (so that each grade filled)
  
  # determine region/technologies with first (highest FLH) data point above first grade
  # and with total potential below last grade
  df.OutsideBreaks <- df.cuml %>% 
    group_by(region, Technology, Distance) %>% 
    summarise( Min = min(maxprod.norm)) %>%
    ungroup() %>% 
    full_join(df.totalPot) %>% 
    filter( Min > grade.breaks[1] | TotalPot < grade.breaks[length(grade.breaks)]) %>%  
    mutate( Max = ifelse(is.na(TotalPot), grade.breaks[length(grade.breaks)], TotalPot))
  
  # for both (too large first grade and too small last grade): distribute grades equally spaced 
  # ugly loop soluation, I have not found a dplyr grouping solution 
  # for making .bincode() breaks argument depend on group
  for (i in 1:nrow(df.OutsideBreaks)) {
    # temporary dataframe for mapping small potentials to equally spaced grades
    df.map.temp <-  df.fine.grades %>% 
      select(region, Technology, Distance, Type, maxprod.norm, FLH, value) %>%
      filter( region == as.character(df.OutsideBreaks$region[i]),  
              Technology == as.character(df.OutsideBreaks$Technology[i]),
              Distance == as.character(df.OutsideBreaks$Distance[i]))
    # sry: a bit messay the following case distinction...if there is ever time, could be structured better
    # 1. for regions/technologies where highest FLH data point (DLR) is outisde first grade (s.t. first grade would be NA)
    # create equally spaced grades up to the first element of grade.breaks that is within the original FLH data bins
    if (df.OutsideBreaks$Min[i] > grade.breaks[1] & df.OutsideBreaks$Max[i] >= grade.breaks[8]) {
      # if lowest DRL FLH grade is below maximum of grade breaks, distribute grades equally from minimum to maximum DLR data point
      if (df.OutsideBreaks$Min[i] > grade.breaks[8]) {
        new.grade.breaks <- seq(df.OutsideBreaks$Min[i]-1e-5,df.OutsideBreaks$Max[i]+1e-5,length.out = length(grade.breaks))
      } else {
      grades.to.shift <- grade.breaks[grade.breaks>df.OutsideBreaks$Min[i]]
      new.grade.breaks <- seq(df.OutsideBreaks$Min[i]-1e-5,min(grades.to.shift), length.out = (9-length(grades.to.shift)+1))
      new.grade.breaks <- c(new.grade.breaks, grades.to.shift[-1])
      }
    # 2. if highest FLH data point (DLR) outside first grade and (!) 
      # total potential below lower bound of last grade -> 
      # equally spaced grades up to the first element of grade.breaks that is within the original FLH data bins and (!)
      # split last grade into multiple equally spaced grades
    } else if (df.OutsideBreaks$Min[i] > grade.breaks[1] & df.OutsideBreaks$Max[i] < grade.breaks[8] & nrow(df.map.temp) > 0) {
      grades.to.shift <- grade.breaks[grade.breaks>df.OutsideBreaks$Min[i]]
      new.grade.breaks <- seq(df.OutsideBreaks$Min[i]-1e-5,min(grades.to.shift), length.out = (9-length(grades.to.shift)+1))
      new.grade.breaks <- c(new.grade.breaks, grades.to.shift[-1])
      grades.to.keep <- new.grade.breaks[new.grade.breaks < df.OutsideBreaks$Max[i]]
      new.grade.breaks <- c(grades.to.keep[-length(grades.to.keep)], seq(grades.to.keep[length(grades.to.keep)],
                                                                           df.OutsideBreaks$Max[i]+1e-2, 
                                                                           length.out = (9-length(grades.to.keep)+2)))
    # 3. if total potential is below the current generation -> equally spaced grades up to total potential
    } else if (df.OutsideBreaks$Max[i] < 1 ) {
      new.grade.breaks <- c(seq(df.OutsideBreaks$Min[i]-1e-5,df.OutsideBreaks$Max[i], 
                              length.out=(length(grade.breaks)+2)))
    # 4. if total potential is above current generation but below lower bound of last grade 
      # -> split last grade into multiple equally spaced grades  
    } else if (df.OutsideBreaks$Max[i] > 1 &  df.OutsideBreaks$Max[i] < grade.breaks[8]  ) {
      grades.to.keep <- grade.breaks[grade.breaks < df.OutsideBreaks$Max[i]] 
      new.grade.breaks <- c(0,grades.to.keep[-length(grades.to.keep)], seq(grades.to.keep[length(grades.to.keep)],
                                                                         df.OutsideBreaks$Max[i]+1e-2, 
                          length.out = (9-length(grades.to.keep)+1)))
    }
      
    df.map.temp <- df.map.temp %>% 
      mutate(grade.seq = .bincode(maxprod.norm, new.grade.breaks))
    
    # bind rows of temporary datafrage to output dataframe of loop: df.map.small
    if (i == 1) {
      df.outside <- df.map.temp
    } else {
      df.outside <- rbind(df.outside, df.map.temp)
    }
  }
  # replace potential values of small potential regions with the equally spaced grades from the loop to avoid emtpy grades
  df.map.REMIND <- df.map.REMIND %>% 
    left_join(df.outside) %>%  
    mutate(grade = ifelse(is.na(grade.seq), grade, grade.seq)) %>% 
    select(-grade.seq) 
  
  
  # spread maxprod column to have weight for FLH aggregation in next step
  df.map.REMIND <- df.map.REMIND %>% 
    spread(Type, value) %>% 
    mutate(maxprod.weight  = maxprod) %>% 
    gather(Type, value, maxprod, capacity, area)
  
  
  
  
  ## aggregate fine grades to REMIND grades 
  df.aggregate.grades <- df.map.REMIND %>% 
    group_by(region, Type, Technology, Distance, grade) %>%
    # aggregate fine grade to REMIND grades: FLH <- FLHs weighted by maxprod of fine grade
    # potential, area, capacity <- sum over all values potential/area/capacity REMIND grade
    summarise( FLH = weighted.mean(FLH, maxprod.weight), value = sum(value)) %>%
    ungroup() %>% 
    # add FLH to Type column to make it another dimension like maxprod, area etc.
    spread(Type, value) %>%
    gather(Type, value, capacity, area, maxprod, FLH) %>%
    select(region, Type, Technology, Distance, grade, value) 
  
  # get overlap of first data point in last grade with second last grade 
  # needs to be substracted from last grade later 
  # to make sure that we get the right totals over all grades
  
  # last data point before lastgrade
  df.seclast <- df.cuml %>% 
    filter(maxprod.norm < grade.breaks[length(grade.breaks)]) %>% 
    group_by(region, Technology, Distance) %>% 
    mutate( Max = max(maxprod.norm)) %>% 
    ungroup() %>% 
    filter( maxprod.norm == Max) %>% 
    select(-Max) %>% 
    gather(Type, Seclast, maxprod, area, capacity) %>% 
    select(region, Technology, Distance, Type, Seclast)
  
  # calculate maxprod, area, capacity difference between last data point before last grade 
  # and lower bound of last grade, to be added removed from lastgrade later
  df.lastgrade.overlap <- df.interpolate %>%
    # filter for  maxprod, area, capacity at lower bound of last grade (threshold)
    filter(maxprod.norm < grade.breaks[length(grade.breaks)]) %>% 
    gather(Type, value, maxprod, area, capacity) %>% 
    group_by(region, Technology, Distance) %>% 
    mutate( Max = max(maxprod.norm)) %>% 
    ungroup() %>% 
    filter(maxprod.norm == Max) %>% 
    select(-Max) %>% 
    #join lsat data point before last grade
    left_join(df.seclast) %>% 
    mutate( diff = value - Seclast) %>% 
    select(region, Technology, Distance, Type, diff)
  
  
  
  
  # assign remaining potential to last grade for regions/technologies 
  # with total potential greater than last grade
  df.lastgrade <- df.cuml %>% 
    # filter for regions/technologies... with total potential within last grade
    filter( maxprod.norm >= grade.breaks[length(grade.breaks)]) %>%  
    select(region, Technology, FLH) %>% 
    left_join(df.pot) %>% 
    group_by(region, Technology, Distance, Type) %>%  
    # sum all data point above lower bound of last grade to get last grade
    summarise( value = sum(value), FLH = mean(FLH)) %>% 
    ungroup() %>% 
    mutate( grade = length(grade.breaks)+1) %>% 
    spread(Type, value) %>%
    gather(Type, value, capacity, area, maxprod, FLH) %>% 
    # join overlap of first data point in last grade with second last grade from last grade
    left_join(df.lastgrade.overlap) %>%  
    # subtract overlap from maxprod, area, capacity of last grade 
    # because this overlap was already assgined to second last grade
    mutate( value = ifelse(is.na(diff), value, value - diff)) %>% 
    select(region, Type, Technology, Distance, grade, value) 
  
  
  
  df.aggregate.grades <- df.lastgrade %>% 
    rbind(df.aggregate.grades) %>% 
    arrange(region, Technology, Distance, Type, grade, value)
  
  
  # relabel dimensions, convert values to fit to desired REMIND input
  df.out <- df.aggregate.grades %>% 
    revalue.levels(Type = c("area" = "limitGeopot", "FLH" = "nur"),
                   Technology = c("PV" = "spv", "CSP" = "csp")) %>% 
    # convert FLH to capacity factor
    mutate( value = ifelse( Type=="nur", round(value/8760,4), value)) %>% 
    # convert maxprod from GWh to EJ
    mutate( value = ifelse( Type == "maxprod", value * 3.6 * 1e-6, value)) %>% 
    arrange(region, Technology, Distance, Type, grade, value)
  
  
  
  
  ### create output magpie object
  
  # convert to magpie object
  out <- as.magpie(df.out, spatial = 1, temporal=NULL, datacol=6)
  # remove additional dimensions (e.g. Distance class)
  out <- collapseNames(out)
  # calculate land use in MW/km2
  out <- add_columns(out, addnm = c("luse"), dim = 3.1)
  out[,,"luse.spv"] <- dimSums(out, dim = 3.3, na.rm=T)[,,"capacity"][,,"spv"]/ dimSums(out, dim = 3.3, na.rm=T)[,,"limitGeopot"][,,"spv"]
  out[,,"luse.csp"] <- dimSums(out, dim = 3.3, na.rm=T)[,,"capacity"][,,"csp"]/ dimSums(out, dim = 3.3, na.rm=T)[,,"limitGeopot"][,,"csp"]
  # remove capacity
  out <- out[,,"capacity",invert=TRUE]
  # relabel sets
  getSets(out) <- c("region", "year", "type",  "technology" , "rlf")
  
  
  return(out)
}
