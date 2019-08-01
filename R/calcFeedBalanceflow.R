#' @title calcFeedBalanceflow
#' @description Calculates feed balanceflows from MAgPIE-Feed model to meet FAO data
#' 
#' @param per_livestock_unit default false
#' @param cellular if TRUE value is calculate on cellular level
#' @param products products in feed baskets that shall be reported
#' @param future if FALSE, only past years will be reported (reduces memory)
#' @return List of magpie objects with results on country or cellular level, unit and description.
#' @author Isabelle Weindl, Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FeedBalanceflow")
#' }
#' 
#' @importFrom magclass getNames

calcFeedBalanceflow<-function(per_livestock_unit=FALSE, cellular=FALSE,products="kall", future=TRUE){
  
  products2<-findset(products,noset="orignal")
  
  if(!per_livestock_unit){
    
    # kap              <- findset("kap")
    # kli              <- findset("kli")
    ProdAttributes      <- calcOutput("Attributes", aggregate = FALSE)
    
    FAOFeednutrients <- collapseNames(calcOutput("FAOmassbalance_pre",aggregate = FALSE)[,,"feed"])
    FAOFeed          <- collapseNames(FAOFeednutrients[,,"dm"])
    FAOFeed          <- add_columns(FAOFeed, addnm = "pasture", dim=3.1)
    
    MAGFeednutrients <- calcOutput("FeedPast", balanceflow = FALSE, cellular = FALSE, aggregate = FALSE,nutrients="all",products=products)
    MAGFeed          <- MAGFeednutrients[,,"dm"]
    
    MAGFeedShare     <- MAGFeed / dimSums(MAGFeed, dim=3.1)
    MAGFeedShare[is.nan(MAGFeedShare)] <- 0
    commonproducts = intersect(getNames(FAOFeed, dim=1), getNames(MAGFeed,dim=2))
    
    #include estimates for pasture feed demand to benchmark data FAOFeed:
    FAOFeed[,,"pasture"] <- collapseNames(dimSums(MAGFeed, dim=3.1))[,,"pasture"]
    #reduced pasture feed demand (which is determined in the feed model as balance post) by the amount of fish that is used as feed and not yet considered in feed baskets
    # replacement is done on the basis of proteins and cannot exceed 50% of the pasture feed demand:
    reducedgraz <- (collapseNames(dimSums(MAGFeednutrients[,,"pasture"],dim=3.1))[,,"nr"]
                                   -collapseNames(FAOFeednutrients[,,"fish"])[,,"nr"])/ProdAttributes[,,"nr.pasture"]
    FAOFeed[,,"pasture"][which(reducedgraz>0.5*FAOFeed[,,"pasture"])] <- reducedgraz[which(reducedgraz>0.5*FAOFeed[,,"pasture"])]
    FAOFeed[,,"pasture"][which(reducedgraz<0.5*FAOFeed[,,"pasture"])] <- 0.5*FAOFeed[,,"pasture"][which(reducedgraz<0.5*FAOFeed[,,"pasture"])]
  
    ##adjusted feed shares of pasture and 'indefinite' feed ressources for ruminants in South and Central Asia:
    #Table 3.28, Wirsenius 2000
    rum_pastshr_IND <- 0.360  #Permanent pasture (including browse)
    rum_scavshr_IND <- 0.225  #Herbage and browse from forest and other land & thinning and weeding in cropland
    FAOFeed["IND",,"pasture"] = (rum_pastshr_IND/(rum_scavshr_IND+rum_pastshr_IND))*FAOFeed["IND",,"pasture"] 
    
    #reduce temporal variability of estimated pasture feed demand:
    FAOFeed[,,"pasture"] <- lowpass(FAOFeed[,,"pasture"],i=3)

    #calculate feed balance flows:
    FeedBalanceflow  <- FAOFeed[,,commonproducts] - dimSums(MAGFeed, dim=3.1)[,,commonproducts]
    FeedBalanceflow2 <- collapseNames(MAGFeedShare[,,commonproducts] * FeedBalanceflow)
    
    FeedBalanceflow2[is.nan(FeedBalanceflow2)] <- 0
    FeedBalanceflow2[is.na(FeedBalanceflow2)]  <- 0
    
    if(any(round(dimSums(FeedBalanceflow2,dim=3.1)-FeedBalanceflow,5)!=0)){
      
      vcat(verbosity = 2,"Difficult to distribute the balanceflow between different livestock commodities, because it is not used at all in the feedbaskets. Distributed to ruminants for now.")
      overflow                              <- FeedBalanceflow - dimSums(FeedBalanceflow2,dim=3.1)
      FeedBalanceflow2[,,"alias_livst_rum"] <- FeedBalanceflow2[,,"alias_livst_rum"] + overflow
      
    }
    
    FeedBalanceflow  <- FeedBalanceflow2
    
    if(cellular){
      
      CountryToCell    <- toolMappingFile(type="cell",name = "CountryToCellMapping.csv",readcsv = TRUE)
      MAGFeedCell      <- calcOutput("FeedPast", balanceflow = FALSE, cellular = TRUE, aggregate = FALSE,nutrients="dm",products=products)
      MAGFeedCell      <- MAGFeedCell[,,commonproducts]
      MAGFeedCountry   <- toolAggregate(MAGFeedCell, rel=CountryToCell, from="celliso", to="iso", dim=1, partrel = TRUE)
      MAGFeedCellshare <- collapseNames(MAGFeedCell / MAGFeedCountry)
      MAGFeedCellshare[is.na(MAGFeedCellshare)]   <- 0
      
      FeedBalanceflow  <- toolAggregate(FeedBalanceflow, rel=CountryToCell, from="iso", to="celliso", dim=1, partrel = TRUE)
      
      for(livst_x in getNames(FeedBalanceflow,dim=1)){
        FeedBalanceflow[,,livst_x]  <- FeedBalanceflow[,,livst_x] * MAGFeedCellshare[,,livst_x]
      }
    }
    
    # add items that are not present in the FAO massbalance pre
    NewItems         <- setdiff(products2, getNames(FAOFeed, dim=1))
    FeedBalanceflow  <- add_columns(FeedBalanceflow, addnm = NewItems, dim=3.2)
    FeedBalanceflow[,,NewItems] <- 0
    
    if(future){
      FeedBalanceflow  <- toolHoldConstantBeyondEnd(FeedBalanceflow)
      # fading out the balanceflow until 2050.
      # Has to be the same as the SlaugherBalanceflow outfade!
      FeedBalanceflow  <- convergence(origin = FeedBalanceflow, aim = 0, start_year = "y2010", end_year = "y2050", type = "s")
    }
    
    weight <- NULL
    unit   <- "t DM"
    getNames(FeedBalanceflow,dim=1) <- substring(getNames(FeedBalanceflow,dim=1),7)
    
  } else if(per_livestock_unit){
    
    kli                           <- findset("kli")
    
    FeedBalanceflow               <- calcOutput("FeedBalanceflow", cellular=cellular, products=products, future=future, aggregate=FALSE)
    LivestockProduction           <- collapseNames(calcOutput("Production", products="kli", cellular=cellular, aggregate = FALSE)[,,kli][,,"dm"])
    LivestockProduction           <- add_columns(LivestockProduction, addnm = "fish", dim=3.1)
    LivestockProduction[,,"fish"] <- 0
    
    if(!cellular){
      LivestockProduction         <- toolHoldConstantBeyondEnd(LivestockProduction)
    }
    
    FeedBalanceflow               <- FeedBalanceflow/LivestockProduction
    FeedBalanceflow[is.na( FeedBalanceflow)] <- 0
    FeedBalanceflow[is.infinite( FeedBalanceflow)] <- 0

    weight       <- LivestockProduction
    unit         <- "1"
    
  } else {stop("per_livestock_unit has to be boolean")}
  
    return(list(x=FeedBalanceflow,
                weight=weight,
                unit=unit,
                description="Difference between feed baskets and feed use by FAO",
                isocountries=!cellular)
    )                   
}
