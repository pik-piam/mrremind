#' toolHarmonize2Baseline
#'
#' @param x magclass object that should be set on baseline
#' @param base magclass object for baseline
#' @param ref_year Reference year
#' @param limited if TRUE, for an underestimated baseline the signal is limited to the additive term rather than the multiplicative factor
#'                if FALSE, x will be put on the baseline as a pure multiplicative factor
#' @param hard_cut Switch to TRUE for data that can not be harmonized, but have to be glued together
#'
#' @return the averaged data in magclass format
#' @author Kristine Karstens
#'
#' @export

toolHarmonize2Baseline <- function(x, base, ref_year, limited=TRUE, hard_cut=FALSE){
  
  
  if(!is.magpie(x) | !is.magpie(base)) stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")
  
  # check if years are overlapping and refs is part of both time horizons
  
  # set years
  years       <- union(getYears(x,as.integer = TRUE), getYears(base,as.integer = TRUE))
  till_ref    <- getYears(base,as.integer=TRUE)
  till_ref    <- till_ref[till_ref <= ref_year]
  after_ref   <- getYears(x,as.integer=TRUE)
  after_ref   <- after_ref[after_ref > ref_year]
  
  
  #check if x and base are identical in dimension except time
  if(getCells(x)!=getCells(base) | (sort(getNames(x))!=sort(getNames(base)))) stop("Dimensions of the MAgPIE objects do not match!")
  
  #get the names of the dimensions
  cells<-getCells(x)
  dataNames<-getNames(x)
  
  # convert to arrays to speed up performance
  x    <- as.data.frame(magpiesort(x))
  base <- as.data.frame(magpiesort(base))
  all<-merge(base,x,by=c("Cell","Region","Year","Data1"),all=TRUE)
  all<-subset(all,Year %in% c(till_ref,after_ref))
  colnames(all)<-c("Cell","Region","Year","Data1","Base","X")
  
  # Prepares the data frame that is going to be used in the operations
  #subsets of the reference years
  X_ref<-subset(x,Year==ref_year)
  Base_ref<-subset(base,Year==ref_year)
  
  #merges all values to their suitable reference year and creates a new column with the appropriate reference value
  all<-all %>% left_join(Base_ref,by=c("Cell","Region","Data1"))
  all<-all %>% left_join(X_ref,by=c("Cell","Region","Data1"))
  all$Year.y<-NULL #drops the repeated columns
  all$Year<-NULL #drops the repeated columns
  names(all)[names(all)=="Value.x"]<-"Base_ref" #renames the new columns
  names(all)[names(all)=="Value.y"]<-"X_ref" #renames the new columns
  
  all<-all %>% mutate(lambda=sqrt(X_ref/Base_ref)) #calculates and creates a new column for lambda
  
  if(hard_cut){
    
    #creates a new variable for the hard cut scenario harmonization
    all<-all %>% mutate(Harmonized=ifelse(Year.x %in% till_ref,Base,X))
    
  } else if (limited==FALSE){
    
    #creates a new variable for the limited==FALSE scenario harmonization  
    all<-all %>% mutate(Harmonized=ifelse(Year.x %in% till_ref,Base,
                                          ifelse((X_ref==0),0,Base_ref*X/X_ref)))
  }else{
    
    all<-all %>% mutate(Harmonized=ifelse(Year.x %in% till_ref,Base,
                                          ifelse(Base_ref<=X_ref,
                                                 ifelse(X_ref==0,0,Base_ref*X/X_ref),### LAMBDA EQUAL 1
                                                 ifelse(X_ref==0,X+Base_ref,Base_ref*(1+((X-X_ref)/Base_ref)*(Base_ref/X_ref)**lambda))### LAMBDA UNEQUAL 1
                                          )
    )
    )
    
    
    
  }
  
  all_final<-all[,c("Cell","Year.x","Data1","Harmonized")]
  colnames(all_final)<-c("Cell","Year","Data1","Value")
  all_final <- all_final[order(all_final$Year),]
  
  #check for nans
  if(any(is.na(all_final$Value))) stop("Data containing inconsistencies.")
  
  out <- toolCell2isoCell(as.magpie(all_final,spatial=1)) 
  
  return(out)
}