#' @title calcGrowingPeriod
#' @description This function determine a mean sowing date and a mean growing period for each cell
#' in order to determine when irrigation can take place.
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("GrowingPeriod", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcGrowingPeriod <- function() {

  # -> harcoden, argument       yield_ratio = 0.1, # threshold for cell yield over global average. crops in cells below threshold will be ignored
  # -> how to handle averaging? avg_range = 8

  ####################################################################################
  #Goal: calculate mean sowing date and growing period
  #Step 1 Take care of inconsistencies (harvest date or sowing date ==0 etc) and convert to magpie crop functional types
  #Step 2 remove wintercrops from both calculations for the northern hemisphere: sowd>180, hard<sowd
  #Step 3 remove crops that have an irrigated yield below 10% of global average (total cell area as aggregation weight for global value)
  #Step 4 Calculate growing period with the remaining crops
  #Step 5 remove sowd1 for sowing date calculation
  #Step 6 Calculate mean sowing date
  #Step 7 Set the sowd to 1 and growing period to 365 where dams are present and where they are NA (reflecting that all crops have been eliminated)
  #Step 8 Calculate the growing days per month for each cell and each year

  ####################################################################################

  ####################################################################################
  #Read sowing and harvest date input (new for LPJmL5)
  ####################################################################################

  LPJ2MAG      <- toolGetMapping( "MAgPIE_LPJmL.csv", type = "sectoral", where = "mappingfolder")

  # Load Sowing dates from LPJmL (use just rainfed dates since they do not differ for irrigated and rainfed)
  sowd         <- collapseNames(readSource("LPJmL", subtype="sdate", convert="onlycorrect")[,,"rainfed"])
  hard         <- collapseNames(readSource("LPJmL", subtype="hdate", convert="onlycorrect")[,,"rainfed"])

  good_crops   <- LPJ2MAG$MAgPIE[which(LPJ2MAG$LPJmL%in%getNames(sowd))]
  bad_crops    <- LPJ2MAG$MAgPIE[which(!LPJ2MAG$LPJmL%in%getNames(sowd))]

  sowd         <- toolAggregate(sowd, rel=LPJ2MAG, from = "LPJmL", to = "MAgPIE", dim=3.1, partrel = TRUE)
  hard         <- toolAggregate(hard, rel=LPJ2MAG, from = "LPJmL", to = "MAgPIE", dim=3.1, partrel = TRUE)

  if(length(bad_crops)>0) warning("No information on the growing period found for those crops: ",paste(unique(bad_crops),collapse=", "))

  #####################################################################################

  ####################################################################################
  #Step 1 Take care of inconsistencies (hard==0 etc)
  ####################################################################################

  #Set sowd to 1 and hard to 365 where either sowd or hard are 0
  sowd[which(hard==0|sowd==0)] <- 1
  hard[which(hard==0|sowd==0)] <- 365

  #Set hard to sowd-1 where sowd and hard are equal
  hard[which(hard==sowd & sowd>1)]  <- sowd[which(hard==sowd)]-1 ### WHY???
  hard[which(hard==sowd & sowd==1)] <- 365

  ####################################################################################

  ####################################################################################
  #Step 2 remove wintercrops from both calculations for the northern hemisphere: sowd>180, hard>365
  ####################################################################################

  cells_northern_hemisphere <- which(magpie_coord[,2] > 0)
  rm_wintercrops            <- new.magpie(getCells(sowd), getYears(sowd), names = getNames(sowd), sets = c("region", "year", "crop"), fill=1)

  #define all crops sowed after 180 days and where sowing date is after harvest date as wintercrops
  rm_wintercrops[cells_northern_hemisphere,,] <-
    ifelse(sowd[cells_northern_hemisphere,,]>180 &
           hard[cells_northern_hemisphere,,]<sowd[cells_northern_hemisphere,,],          ### WHY???
           NA,1)

  ####################################################################################

  ####################################################################################
  #Step 3 remove crops that have an irrigated yield below 10% of global average (total cell area as aggregation weight)
  ####################################################################################

  area   <- dimSums(readSource("LUH2v2", subtype="states", convert="onlycorrect"), dim = 3)
  yields <- collapseNames(calcOutput("Yields", aggregate = FALSE)[,,good_crops][,,"irrigated"])

  years  <- intersect(getYears(area), getYears(yields))
  area   <- area[,years,]
  yields <- yields[,years,]
  cell2GLO <- array(c(getCells(yields), rep("GLO",59199)), dim=c(59199,2))

  glo_yields   <- toolAggregate(yields, cell2GLO, weight = area)
  ratio_yields <- yields/glo_yields

  rm_lowyield   <- yields
  rm_lowyield[] <- 1
  rm_lowyield[ratio_yields < 0.1] <- NA

  rm(ratio_yields,yields,area,glo_yields)
  ####################################################################################

  ####################################################################################
  #Step 4 Calculate mean growing period with the remaining crops
  ####################################################################################

  #calculate growing period as difference of sowing date to harvest date
  sowd <- sowd[,years,]
  hard <- hard[,years,]
  grow_period <- hard - sowd
  grow_period[which(hard < sowd)] <- 365 + grow_period[which(hard < sowd)]

  #calculate the mean after removing the before determined winter- and low yielding crops
  rm_wintercrops <- rm_wintercrops[,years,]
  rm_lowyield    <- rm_lowyield[,years,]

  n_crops          <- dimSums(rm_wintercrops * rm_lowyield, dim=3, na.rm=T)
  mean_grow_period <- dimSums(grow_period * rm_wintercrops * rm_lowyield, dim=3, na.rm=T)/n_crops
  mean_grow_period[is.infinite(mean_grow_period)] <- NA

  #############################################################################

  ####################################################################################
  #Step 5 remove sowd1 for sowing date calculation
  ####################################################################################

  rm_sowd1          <- sowd
  rm_sowd1[]        <- 1
  rm_sowd1[sowd==1] <- NA

  ####################################################################################

  ####################################################################################
  #Step 6 Calculate mean sowing date
  ####################################################################################

  n_crops          <- dimSums(rm_wintercrops * rm_lowyield * rm_sowd1, dim=3, na.rm=T)
  mean_sowd <- dimSums(grow_period * rm_wintercrops * rm_lowyield * rm_sowd1, dim=3, na.rm=T)/n_crops
  mean_sowd[is.infinite(mean_sowd)] <- NA

  ####################################################################################

  ####################################################################################
  #Step 7 Set the sowd to 1 and growing period to 365 where dams are present and where they are NA (reflecting that all crops have been eliminated).
  ####################################################################################

  dams <- readSource("Dams", convert="onlycorrect")

  for(t in getYears(mean_sowd)){
    mean_sowd[which(dams==1),t]        <- 1
    mean_grow_period[which(dams==1),t] <- 365
  }

  mean_sowd[is.na(mean_sowd)]               <- 1
  mean_grow_period[is.na(mean_grow_period)] <- 365
  mean_sowd         <- round(mean_sowd)
  mean_grow_period  <- round(mean_grow_period)

  ####################################################################################

  ####################################################################################
  #Step 8 Calculate the growing days per month for each cell and each year
  ####################################################################################

  month        <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  month_length <- c(   31,   28,   31,   30,   31,   30,   31,   31,   30,   31,   30,   31)
  names(month_length) <- month

  #Determine which day belongs to which month
  days_months        <- 1:365
  names(days_months) <- 1:365

  before <- 0
  for(i in 1:length(month_length)){
    days_months[(before+1):(before+month_length[i])]        <- i
    names(days_months)[(before+1):(before+month_length[i])] <- names(month_length)[i]
    before <- before+month_length[i]
  }

  #mag object for the growing days per month
  grow_days_per_month <- new.magpie(getCells(mean_sowd), getYears(mean_sowd), month, fill=0)

  #determine the harvest day, take care if it is greater than 365
  mean_hard <- (mean_sowd + mean_grow_period - 1)%%365
  mean_hard[mean_hard==0] <- 365

  mean_hard<-as.array(mean_hard)
  mean_sowd<-as.array(mean_sowd)

  #Loop over the months to set the number of days that the growing period lasts in each month
  for(t in getYears(mean_sowd)){

    #goodcells are cells in which harvest date is after sowing date,
    #i.e. the cropping period does not cross the beginning of the year
    goodcells  <- ifelse(mean_hard[,t,] >= mean_sowd[,t,], 1, 0)
    badcells   <- ifelse(mean_hard[,t,] >= mean_sowd[,t,], 0, 1)

    for(month in 1:12){

      last_monthday  <- which(days_months==month)[length(which(days_months==month))]
      first_monthday <- which(days_months==month)[1]
      test_harvest_goodcells <- as.array(mean_hard[,t,]-first_monthday+1)
      days_in_this_month_goodcells<-as.array(last_monthday-mean_sowd[,t,]+1)
      days_in_this_month_goodcells[days_in_this_month_goodcells<0]<-0 #Month before sowing date
      days_in_this_month_goodcells[days_in_this_month_goodcells>month_length[month]]<-month_length[month] # Month is completely after sowing date
      days_in_this_month_goodcells[test_harvest_goodcells<0]<-0 #Month lies after harvest date
      days_in_this_month_goodcells[test_harvest_goodcells>0 & test_harvest_goodcells<month_length[month]]<-days_in_this_month_goodcells[test_harvest_goodcells>0 & test_harvest_goodcells<month_length[month]]-(last_monthday-mean_hard[test_harvest_goodcells>0 & test_harvest_goodcells<month_length[month],t,]) # Harvest date lies in the month. cut off the end of the month after harvest date
      days_in_this_month_goodcells<-days_in_this_month_goodcells<-days_in_this_month_goodcells*goodcells
      days_in_this_month_badcells_firstyear<-as.array(last_monthday-mean_sowd[,t,]+1)
      days_in_this_month_badcells_firstyear[days_in_this_month_badcells_firstyear<0]<-0 #Month before sowing date
      days_in_this_month_badcells_firstyear[days_in_this_month_badcells_firstyear>month_length[month]]<-month_length[month] # Month is completely after sowing date
      days_in_this_month_badcells_secondyear<-as.array(mean_hard[,t,]-first_monthday+1)
      days_in_this_month_badcells_secondyear[days_in_this_month_badcells_secondyear<0]<-0 #Month lies completely after harvest day
      days_in_this_month_badcells_secondyear[days_in_this_month_badcells_secondyear>month_length[month]]<-month_length[month] #Month lies completely before harvest day
      days_in_this_month_badcells<-(days_in_this_month_badcells_firstyear+days_in_this_month_badcells_secondyear)*badcells

      grow_days_per_month[,t,month]<-days_in_this_month_goodcells+days_in_this_month_badcells
    }
  }

  grow_days_per_month <- as.magpie(grow_days_per_month)
  if(any(is.na(grow_days_per_month))) stop("Found NAs.")

  return(list(
    x=grow_days_per_month,
    weight=NULL,
    unit="d",
    description="Growing days per month in days",
    isocountries=FALSE))

}
