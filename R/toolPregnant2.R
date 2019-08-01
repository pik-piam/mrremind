#' @title toolPregnant2
#' @description extra intake kcal/day for pregnant women. The number of pregnant women is 
#' computed through the number of 0 year old children each year (Lutz dataset).
#' According to Human energy requirments , Fao (Rome, 2004) , a woman requires an additional
#' food of 845 kcal/day and 675 kcal/day of food on average furing her pregancy and lactation 
#' period respectively. According to Naegele?s rule, the mean gestation period is 280 days (40 weeks)
#' and the lactation period 6 month (25 weeks).
#' @param demo demo is the population divided by sex male (M) , female (F) and both (B)
#' and divided by 8 age classes: 0-4, 5-9, 10-14, 15-19, AG1 (20-29), AG2 (30-59), AG3(60-79), AG4(80+) 
#' @param reproductive reproductive age classes (on which the energy requiremetns for newborns are distributed)
#' @export

toolPregnant2  <- function (demo, reproductive) {
  n.preg <- dimSums(demo[,,c("F.0--4","M.0--4")],dim=c("sex","BMIgroup"))/5
  preg_per_rep = (n.preg) / dimSums(demo[,,reproductive],dim=c("sex","age","BMIgroup"))
  preg_per_rep[is.nan(preg_per_rep)]<-0

  if(any(preg_per_rep>1,na.rm = TRUE)) {warning("more than one kid per women in reproductive age. strange")}
  
  intake_preg_pc <- (40/66)*845 + (26/66)*675
  out<- demo[,,reproductive]*0 + collapseNames(intake_preg_pc * preg_per_rep)
  
  return(out)
}


