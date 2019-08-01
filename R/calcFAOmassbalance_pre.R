#' @title calcFAOmassbalance_pre
#' @description Calculates an extended version of the Food Balance Sheets. Makes explicit the conversion processes that convert one type of product into another. Includes processes like milling, distilling, extraction etc. Adds certain byproducts like distillers grains or ethanol.
#'
#' @param years years to be estimated. Takes a lot of time.
#' 
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' This is an intermediary result, which is used e.g. for estimating the feed baskets. For most uses, it is more appropriate to use the FAOmasbalance instead of the FAOmassbalance_pre.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcFAOmassbalance}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FAOmassbalance_pre")
#' }
#' @importFrom graphics plot
#' @importFrom magclass getSets as.magpie fulldim complete_magpie
#' @importFrom utils read.csv
#' @importFrom compiler cmpfun
#' @importFrom madrat madlapply

calcFAOmassbalance_pre <- function(years=paste0("y",(seq(1965,2010,5)))) {
  
  # CHN y2010 Groundnut Oil stockvariation p
  # CHN y2010 Other and products feed k 
  
 # years=paste0("y",(1960+1:10*5))
  #years=paste0("y",(seq(1965,2010,5)))
  massbalance<-NULL
  
  ### Functions
  
  
  processing_global<-function(
    object,
    goods_in=c("2536|Sugar cane","2537|Sugar beet"),
    from="processed",
    process="refining",
    # the order matters!
    goods_out=c("2818|Sugar, Refined Equiv","2544|Molasses"),
    report_as=c("sugar1","molasses1"),
    lossname="refiningloss"
  ){
    if (any(object[,,goods_in][,,c(report_as,lossname)]!=0)){stop("Output flows already exist.")}
    if (any(object[,,goods_out][,,c("production_estimated")]!=0)){stop("Output flows already exist.")}
    
    conv_factor<-(
      dimSums(object[,,goods_out][,,"production"],dim=c("region","ElementShort"))
      /dimSums(object[,,goods_in][,,from],dim=c("region","ItemCodeItem","ElementShort"))
    )
    
    if (any(dimSums(conv_factor[,,goods_out],dim="ItemCodeItem")[,,c("dm","nr","p","k","ge")]>1)) {
      print(conv_factor)
      print(dimSums(conv_factor[,,goods_out],dim="ItemCodeItem")[,,c("dm","nr","p","k","ge")])
      stop("conversion factors exceed 1. not suitable for a global conversion factor.")
    }
    
    #estimate outputs
    
    estim_outputs <- function(j){
      object[,,report_as[j]][,,goods_in]    <<- dimSums(object[,,goods_in][,,from],dim="ElementShort")*dimSums(conv_factor[,,goods_out[j]],dim=c("ItemCodeItem"))
      object[,,"production_estimated"][,,goods_out[j]] <<- dimSums(object[,,report_as[j]][,,goods_in],dim=c("ElementShort","ItemCodeItem"))
    }
    estim_outputs_c <- cmpfun(estim_outputs)
    
    invisible(lapply(c(1:length(goods_out)), estim_outputs_c))
    
    # for (j in 1: length(goods_out)) {
    #   object[,,report_as[j]][,,goods_in]    <- dimSums(object[,,goods_in][,,from],dim="ElementShort")*dimSums(conv_factor[,,goods_out[j]],dim=c("ItemCodeItem"))                                      
    #   object[,,"production_estimated"][,,goods_out[j]] <- dimSums(object[,,report_as[j]][,,goods_in],dim=c("ElementShort","ItemCodeItem"))
    # }
    
    
    # calculate refining losses as mass balance difference
    object[,,lossname][,,goods_in]<-(
      dimSums(object[,,goods_in][,,from],dim=c("ElementShort"))
      - dimSums(object[,,goods_in][,,report_as],dim=c("ElementShort"))
    )
    
    # Massbalance tests
    diff<- ( dimSums(object[,,goods_in][,,c(report_as,lossname)],dim=c("ElementShort"))
             - dimSums(object[,,goods_in][,,c(from)],dim=c("ElementShort")))
    if (any(abs(diff)>0.01)) {
      print(diff[,,goods_in])
      stop("NAs in dataset or function corrupt.")
    }
    
    diff<- ( dimSums(object[,,goods_in][,,c(from)],dim=c("ElementShort","ItemCodeItem"))
             - dimSums(object[,,goods_in][,,c(lossname)],dim=c("ElementShort","ItemCodeItem"))
             - dimSums(object[,,goods_out][,,c("production_estimated")],dim=c("ElementShort","ItemCodeItem"))
    )
    if (any(abs(diff)>0.01)) {
      print(diff[,,goods_in])
      stop("NAs in dataset or function corrupt.")
    }
    
    diff<- sum(object[,,goods_out][,,c("production_estimated")]) - sum(object[,,goods_out][,,c("production")])
    
    if (any(abs(diff)>0.01)) {
      print(diff[,,goods_in])
      stop("global estimated production does not meet global production")
    }
    
    # negative value check
    relevant_attributes<-c("dm","nr","ge","p","k")
    if(any(object[,,goods_in][,,c(report_as,from,lossname)][,,relevant_attributes]< 0)){
      print(diff[,,goods_in])
      warning("Massbalancing failed, negative values.")
    }
    #
    object[,,process][,,goods_in]<-object[,,from][,,goods_in]
    if (from!=process){object[,,from][,,goods_in]<-0}
    return(object)
  }
  
  processing_global_c <- cmpfun(processing_global)
  
  
  
  
    
  cereal_milling_global<-function(object) {
    cereals<-c("2511|Wheat and products","2513|Barley and products","2514|Maize and products",
               "2515|Rye and products","2516|Oats","2517|Millet and products",
               "2518|Sorghum and products","2520|Cereals, Other","2804|Rice (Paddy Equivalent)")
    brans<-c("2600|Brans")
    # the order matters!
    #milled<-c("food", "other_util")
    #flour<-c("flour1")
    milled<-c("food")
    flours<-c("flour1")
    process<-c("milling")
    
    if (any(object[,,cereals][,,c(flours,"brans1","branoil1")]!=0)){stop("Output flows already exist.")}
    
    milled_global <- dimSums(object[,,cereals][,,milled],dim=c("region","ElementShort"))
    brans_global <- dimSums(object[,,brans][,,"production"],dim=c("region","ElementShort","ItemCodeItem"))
    bran_attributes <- setYears((brans_global/dimSums(brans_global[,,"dm"],dim="attributes"))[1,1,],NULL)
    
    # estimating bran based on simple factors (Feedipedia)
    # rice: 10%, wheat: 25% 
    # we use 20% for wheat to account for some wholegrain meal
    # own estimates to not violate massbalance: corn and trce get only 5%
    bran_ratio<-new.magpie("GLO",NULL,cereals,fill = 0.20)
    getSets(bran_ratio)<-c("region","year","ItemCodeItem")
    bran_ratio[,,"2804|Rice (Paddy Equivalent)"]<-0.1
    bran_ratio[,,"2514|Maize and products"]<-0.1
    bran_ratio[,,"2518|Sorghum and products"]<-0.05
    bran_ratio[,,"2517|Millet and products"]<-0.05
    brans_uncalibrated <- bran_ratio * milled_global[,,"dm"]
    bran_ratio<-dimSums(bran_ratio*brans_global[,,"dm"]/dimSums(brans_uncalibrated[,,"dm"],dim="ItemCodeItem"),dim="attributes")
    
    #print("Bran ratio:")
    #print(bran_ratio)
    
    bran_estimated<-bran_ratio*dimSums(object[,,cereals][,,milled][,,"dm"],dim="attributes")*bran_attributes
    quality_indicator<-dimSums(object[,,brans][,,"production"],dim=c("ItemCodeItem","ElementShort"))/dimSums(bran_estimated,dim=c("ItemCodeItem","ElementShort"))
    object[,,"brans1"][,,cereals]<-dimSums(bran_estimated[,,cereals],dim=c("ElementShort"))
    object[,,"production_estimated"][,,brans]<-dimSums(bran_estimated[,,cereals],dim=c("ItemCodeItem","ElementShort"))
    
    maize_germoil_ratio = dimSums(object[,,"2582|Maize Germ Oil"][,,"production"],dim=c("region","ItemCodeItem","ElementShort")) / dimSums(milled_global[,,"2514|Maize and products"],dim="ItemCodeItem")
    maize_germoil_estimated<-object[,,"2514|Maize and products"][,,milled]*maize_germoil_ratio
    object[,,"branoil1"][,,"2514|Maize and products"]<-dimSums(maize_germoil_estimated[,,"2514|Maize and products"],dim=c("ElementShort"))
    object[,,"production_estimated"][,,"2582|Maize Germ Oil"]<-dimSums(maize_germoil_estimated[,,"2514|Maize and products"],dim=c("ItemCodeItem","ElementShort"))
    
    rice_branoil_ratio = dimSums(object[,,"2581|Ricebran Oil"][,,"production"],dim=c("region","ItemCodeItem","ElementShort")) / dimSums(milled_global[,,"2804|Rice (Paddy Equivalent)"],dim="ItemCodeItem")
    rice_branoil_estimated<-object[,,"2804|Rice (Paddy Equivalent)"][,,milled]*rice_branoil_ratio
    object[,,"branoil1"][,,"2804|Rice (Paddy Equivalent)"]<-dimSums(rice_branoil_estimated[,,"2804|Rice (Paddy Equivalent)"],dim=c("ElementShort"))
    object[,,"production_estimated"][,,"2581|Ricebran Oil"]<-dimSums(rice_branoil_estimated[,,"2804|Rice (Paddy Equivalent)"],dim=c("ItemCodeItem","ElementShort"))
    
    #calculate flour as residual
    flour=object[,,milled][,,cereals]-bran_estimated
    flour[,,"2804|Rice (Paddy Equivalent)"][,,milled]<-flour[,,"2804|Rice (Paddy Equivalent)"][,,milled]-rice_branoil_estimated
    flour[,,"2514|Maize and products"][,,milled]<-flour[,,"2514|Maize and products"][,,milled]-maize_germoil_estimated
   
    add_flours <- function(i){
      object[,,flours[i]][,,cereals]<<-flour[,,milled[i]][,,cereals]
    }
    
    add_flours_c <- cmpfun(add_flours)
    
    lapply(1:length(flours), add_flours_c)
    
    #  for (i in length(flours)){
    #   object[,,flours[i]][,,cereals]<-flour[,,milled[i]][,,cereals]
    # }
    
    # massbalance checks
    diff<- ( dimSums(object[,,cereals][,,c(flours,"brans1","branoil1")],dim=c("ElementShort"))
             - dimSums(object[,,cereals][,,c(milled)],dim=c("ElementShort")))
    if (any(round(diff,5)!=0)) {
      print(diff[,,cereals])
      stop("NAs in dataset or function corrupt.1")
    }
    diff<- ( dimSums(object[,,cereals][,,c("branoil1")],dim=c("ElementShort","ItemCodeItem"))
             - dimSums(object[,,c("2581|Ricebran Oil","2582|Maize Germ Oil")][,,c("production_estimated")],dim=c("ElementShort","ItemCodeItem")))
    if (any(round(diff,5)!=0)) {
      print(diff)
      stop("NAs in dataset or function corrupt.2")
    }
    # negative value check
    if(any(round(object[,,cereals][,,c(flours,milled,"brans1","branoil1")],5)<0)){
      stop("Massbalancing failed, negative values.")
    }
    
    add_processed <- function(i){
      object[,,process[i]][,,cereals]<<-object[,,milled[i]][,,cereals]
      if(milled[i]!=process[i]){object[,,milled[i]][,,cereals]<<-0}
    }
    
    add_processed_c <- cmpfun(add_processed)
    
    invisible(lapply(c(1:length(flours)), add_processed_c))
    # for (i in length(flours)){
    #   object[,,process[i]][,,cereals]<-object[,,milled[i]][,,cereals]
    #   if(milled[i]!=process[i]){object[,,milled[i]][,,cereals]<-0}
    # }
    
    ### Fooduse in brans is included in the commodity balance sheets, but not reflected in calories. we substract bran consumption from cereal consumption in the respective countries.
    # for simplicity, we distribute brans proportional to all cereal fooduse.
    
    branshr <- dimSums(object[,,brans][,,"food"][,,c("wm","ge","nr")],dim=c(3.1,3.2))/dimSums(object[,,cereals][,,"households"][,,c("wm","ge","nr")],dim=c(3.1,3.2))
    branshr[is.nan(branshr)]<-0
    if(any(branshr<0)){vcat(1,"branshr should not be smaller than zero.")}
    object[,,cereals][,,"households"][,,c("wm","ge","nr")]<-(1-branshr)*object[,,cereals][,,"households"][,,c("wm","ge","nr")]
    object[,,brans][,,"households"][,,c("wm","ge","nr")]<-object[,,brans][,,"food"][,,c("wm","ge","nr")]
    
    return(object)
  }
  
  cereal_milling_global_c <- cmpfun(cereal_milling_global)
  
  extract_good_from_flow<-function(
    object,
    good_in,
    from,
    process,
    extract,
    report_as,
    extraction_quantity,
    extraction_attribute,
    residual,
    prod_attributes
  ){
    if (length(from)>1){stop("please only use one from")}
    if (length(report_as)>1){stop("please only use one report_as")}
    if (length(good_in)>1){stop("please only use one good")}
    if (length(extract)>1){stop("please only use one good")}
    if (any(object[,,good_in][,,c(report_as,residual)]!=0)){warning("output flows already exist")}
    
    attr_no_wm<-c("dm","nr","p","k","ge")
    #print("Extraction rate")  
    attributes_from<-dimSums(dimSums(object[,,from][,,good_in],dim="region")/dimSums(object[,,from][,,good_in][,,extraction_attribute],dim=c("region","attributes")),dim=c("ItemCodeItem","ElementShort"))
    attributes_to<-dimSums(prod_attributes[,,extract]/dimSums(prod_attributes[,,extract][,,extraction_attribute],dim=c("attributes")),dim=c("ItemCodeItem"))
    extraction_factor<-attributes_from[,,attr_no_wm]/attributes_to[,,attr_no_wm]
    #print(extraction_factor)
    maxextract<-as.magpie(apply(X = extraction_factor,MARGIN = 2,FUN = min))
    #print(maxextract)
    
    if (extraction_quantity=="max") {
      extraction_quantity <- maxextract
    } else if (any(extraction_quantity>maxextract)) {
      print((extraction_quantity>maxextract))
      stop("too high extraction quantity")
    }
    
    extracted<-dimSums(object[,,from][,,good_in][,,extraction_attribute],dim=c("attributes","ItemCodeItem","ElementShort"))*extraction_quantity*attributes_to
    
    
    losses<-dimSums(object[,,good_in][,,from],dim="ElementShort")-extracted
    
    object[,,good_in][,,report_as]<-extracted
    object[,,good_in][,,residual]<-losses
    
    object[,,"production_estimated"][,,extract]=object[,,"production_estimated"][,,extract]+extracted
    
    diff<- ( dimSums(object[,,good_in][,,c(report_as,residual)],dim=c("ElementShort"))
             - dimSums(object[,,good_in][,,c(from)],dim=c("ElementShort")))
    if (any(abs(diff)>0.01)) {
      print(diff[,,good_in])
      stop("NAs in dataset or function corrupt.")
    }
    
    object[,,process][,,good_in]<-object[,,from][,,good_in]
    if (from!=process){object[,,from][,,good_in]<-0}
    return(object)  
  }
  
  extract_good_from_flow_c <- cmpfun(extract_good_from_flow)
  
  ethanol_processing<-function(
    object,
    good_in,
    ethanol_yield_liter_per_ton,
    prod_attributes
  ) {
    # liter yield in dm
    extraction_quantity=0.789*ethanol_yield_liter_per_ton/1000
    object[,,good_in][,,"intermediate"]=0
    # extract ethanol
    object<-extract_good_from_flow_c(
      object=object,
      good_in=good_in,
      from="other_util",
      process="distilling",
      extract="X001|Ethanol",
      report_as="ethanol1",
      extraction_quantity=extraction_quantity,
      extraction_attribute="dm",
      residual="intermediate",
      prod_attributes
      #=prod_attributes
    )
    
    object<-extract_good_from_flow_c(
      object=object,
      good_in=good_in,
      from="intermediate",
      process="intermediate",
      extract="X002|Distillers_grain",
      report_as="distillers_grain1",
      extraction_quantity="max",
      extraction_attribute="nr",
      residual="distillingloss",
      prod_attributes=prod_attributes
    )
    '  
    ethanol:
    DDGS Handbook
    U.S. Grains Council. 2013. A Guide to Distillers Dried Grains with Solubles (DDGS). http://www.grains.org/buyingselling/ddgs/handbook/20140422/comparison-different-grain-ddgs-sources-nutrient-composition.
    sugarcane: 654 l/t
    barley: 399 l/t
    corn: 408 l/t
    oats: 262 l/t
    wheat: 375 l/t
    ethanol weight per l:  789g
    similar numbers: 
    Balat M and Balat H 2009 Recent trends in global production and utilization of bio-ethanol fuel Applied Energy 86 2273-82
    '
    object[,,good_in][,,"intermediate"]=0
    return(object)
  }
  
  ethanol_processing_c <- cmpfun(ethanol_processing)
  
 
  
  oil_processing<-function(
    object,
    goods_in=c("2555|Soyabeans"),
    oil_out=c("2571|Soyabean Oil"),
    cake_out=c("2590|Soyabean Cake"),
    prod_attributes
  ){
    object[,,goods_in][,,"intermediate"]=0
    # extract oil
    object<-processing_global_c(
      object=object,
      goods_in=goods_in,
      from="processed",
      process="extracting",
      # the order matters!
      goods_out=oil_out,
      report_as=c("oil1"),
      lossname="intermediate"
    )
    calc_goods_in <- function(good_in){
      object<<-extract_good_from_flow_c(
        object=object,
        good_in=good_in,
        from="intermediate",
        process="intermediate",
        extract=cake_out,
        report_as="oilcakes1",
        extraction_quantity="max",
        extraction_attribute="dm",
        residual="extractionloss",
        prod_attributes=prod_attributes
      )
    }
    
    calc_goods_in_c <- cmpfun(calc_goods_in)
    
    invisible(lapply(goods_in, calc_goods_in_c))
    
    # for (good_in in goods_in) {
    #   object<-extract_good_from_flow(
    #     object=object,
    #     good_in=good_in,
    #     from="intermediate",
    #     process="intermediate",
    #     extract=cake_out,
    #     report_as="oilcakes1",
    #     extraction_quantity="max",
    #     extraction_attribute="dm",
    #     residual="extractionloss",
    #     prod_attributes=prod_attributes
    #   )
    # }
    object[,,goods_in][,,"intermediate"]=0
    return(object)
  }
  
  oil_processing_c <- cmpfun(oil_processing)
  
  oilpalm_processing<-function(
    object,
    prod_attributes
  ){
    newproduct<-dimSums(object[,,"production"][,,c("2577|Palm Oil","2576|Palmkernel Oil","2595|Palmkernel Cake" )][,,"dm"],dim=c("ItemCodeItem","ElementShort","attributes"))
    newproduct<-prod_attributes[,,"X003|Palmoil_Kerneloil_Kernelcake"]*newproduct
    object[,,"X003|Palmoil_Kerneloil_Kernelcake"][,,c("production","domestic_supply","processed")]<-newproduct
    object[,,"X003|Palmoil_Kerneloil_Kernelcake"][,,"intermediate"]=0  
    # extract oil
    object<-processing_global_c(
      object=object,
      goods_in=c("X003|Palmoil_Kerneloil_Kernelcake"),
      from="processed",
      process="extracting",
      # the order matters!
      goods_out=c("2577|Palm Oil","2576|Palmkernel Oil"),
      report_as=c("oil1","oil2"),
      lossname="intermediate"
    )
    
    object<-extract_good_from_flow_c(
      object=object,
      good_in="X003|Palmoil_Kerneloil_Kernelcake",
      from="intermediate",
      process="intermediate",
      extract="2595|Palmkernel Cake",
      report_as="oilcakes1",
      extraction_quantity="max",
      extraction_attribute="dm",
      residual="extractionloss",
      prod_attributes=prod_attributes
    )
    object[,,"intermediate"]=0
    return(object)
  }
  
  oilpalm_processing_c <- cmpfun(oilpalm_processing)
  
  # Read in Commodity Balance
  CBC <- calcOutput(type = "FAOharmonized",aggregate = F)
  getSets(CBC)<-c("region","year","ItemCodeItem.ElementShort")
  if (any(duplicated(dimnames(CBC)[[3]])==T)) {
    stop(paste("The folowing dimnames are duplicated:",dimnames(CBC)[[3]][which(duplicated(dimnames(CBC)[[3]])==T)],sep=""))
  }

  #CBC<-CBC[,,-which(duplicated(dimnames(CBC)[[3]])==T)]
  # remove double counting
  removethem<-c(
    "2903|Vegetal Products + (Total)", 
    "2905|Cereals - Excluding Beer + (Total)",
    "2907|Starchy Roots + (Total)",
    "2908|Sugar Crops + (Total)",             
    "2909|Sugar & Sweeteners + (Total)",
    "2911|Pulses + (Total)",                  
    "2912|Treenuts + (Total)",
    "2913|Oilcrops + (Total)",                
    "2914|Vegetable Oils + (Total)",           
    "2918|Vegetables + (Total)",              
    "2919|Fruits - Excluding Wine + (Total)",  
    "2922|Stimulants + (Total)",              
    "2923|Spices + (Total)",                   
    "2924|Alcoholic Beverages + (Total)",     
    "2928|Miscellaneous + (Total)",            
    "2941|Animal Products + (Total)",         
    "2943|Meat + (Total)",                     
    "2945|Offals + (Total)",                  
    "2946|Animal fats + (Total)",              
    "2948|Milk - Excluding Butter + (Total)", 
    "2949|Eggs + (Total)",                     
    "2960|Fish, Seafood + (Total)",
    "2961|Aquatic Products, Other + (Total)",
    "2805|Rice (Milled Equivalent)",
    "2556|Groundnuts (Shelled Eq)",
    "2827|Sugar, Raw Equivalent",
    "2542|Sugar (Raw Equivalent)",
    "2815|Roots & Tuber Dry Equiv",
    "2562|Palm kernels",
    "2901|Grand Total + (Total)",
    "2747|Silk",
    "2739|Milk, Skimmed",
    "2738|Milk, Whole",
    "2741|Cheese",
    "2672|Rubber",
    "2742|Whey", 
    "2671|Tobacco"
  )
  
  CBC<-CBC[,,fulldim(CBC)[[2]][[3]][!fulldim(CBC)[[2]][[3]] %in% removethem]]

  CBC<-complete_magpie(CBC,fill = 0)
  
  missingproducts<-c("X001|Ethanol","X002|Distillers_grain","X003|Palmoil_Kerneloil_Kernelcake","X004|Brewers_grain")
  CBC<-add_columns(CBC,addnm = missingproducts,dim=3.1)
  
  prod_attributes <- calcOutput("Attributes",aggregate = F)
  reduced<-fulldim(prod_attributes)[[2]][[4]][!fulldim(prod_attributes)[[2]][[4]]%in%c("betr","begr","pasture","scp","res_cereals","res_fibrous","res_nonfibrous","wood","woodfuel")]
  prod_attributes<-prod_attributes[,,reduced]

  relationmatrix <- toolGetMapping("FAOitems.rda", type = "sectoral", where="moinput")
  relationmatrix <- relationmatrix[,which(names(relationmatrix)%in%c("FoodBalanceItem","k"))]
  relationmatrix <- relationmatrix[-which(duplicated(relationmatrix[,1])==T),]
  
  other_crops<-as.vector(relationmatrix[which(relationmatrix[,2]=="others"),1])
  tece<-as.vector(relationmatrix[which(relationmatrix[,2]=="tece"),1])
  trce<-as.vector(relationmatrix[which(relationmatrix[,2]=="trce"),1])
  potato<-as.vector(relationmatrix[which(relationmatrix[,2]=="potato"),1])
  cassava_sp<-as.vector(relationmatrix[which(relationmatrix[,2]=="cassav_sp"),1])
  sugar<-as.vector(relationmatrix[which(relationmatrix[,2]=="sugar"),1])
  molasse<-as.vector(relationmatrix[which(relationmatrix[,2]=="molasses"),1])
  brans<-as.vector(relationmatrix[which(relationmatrix[,2]=="brans"),1])
  
  prod_attributes<-toolAggregate(x = prod_attributes,rel =relationmatrix,dim = 3.2,from = "k",to = "FoodBalanceItem", partrel=TRUE)
  getSets(prod_attributes)<-c("region","year","attributes","ItemCodeItem")
  attributes_wm<-(prod_attributes/dimSums(prod_attributes[,,"wm"],dim="attributes"))
   if (!(all(fulldim(CBC)[[2]][[3]]%in%fulldim(attributes_wm)[[2]][[4]]))) {
     #stop("somethins wrong with the product mapping!")
     vcat(verbosity = 2, "The following items were removed from the dataset because of missing prod_attributes:", paste(fulldim(CBC)[[2]][[3]][!fulldim(CBC)[[2]][[3]]%in%fulldim(attributes_wm)[[2]][[4]]]), "\n")
     CBC <- CBC[,,fulldim(CBC)[[2]][[3]][!fulldim(CBC)[[2]][[3]]%in%fulldim(attributes_wm)[[2]][[4]]], invert=T]
   }
  if (!all(fulldim(attributes_wm)[[2]][[4]]%in%fulldim(CBC)[[2]][[3]])) {
    stop("For the following items there were entries in prod_attributes but no respective data:", paste(fulldim(attributes_wm)[[2]][[4]][!fulldim(attributes_wm)[[2]][[4]]%in%fulldim(CBC)[[2]][[3]]] ), "\n")
  }
  
  if (is.null(years)) {
    years<-getYears(CBC)
  }
  if (nchar(years[[1]])<5) {
    years<-paste0("y",years)
  }
  # create groups of 5 year periods
  
  #print("Start estimating processing flows")
  
  
  massbalance_in_yeargroups <- function(year_x){
    CBCflows<-CBC[,year_x,]*attributes_wm
    
    
    names_processing<-c("production_estimated",
                        "milling","brans1","branoil1","flour1",
                        "refining","sugar1","molasses1","refiningloss",
                        "extracting","oil1","oil2","oilcakes1","extractionloss",
                        "fermentation","alcohol1","alcohol2","alcohol3","brewers_grain1","alcoholloss",
                        "distilling","ethanol1","distillers_grain1","distillingloss",
                        "intermediate",
                        "households")
    CBCflows<-add_columns(CBCflows,addnm =names_processing,dim = 3.2)
    
    CBCflows[,,"households"][,,"ge"] <- CBC[,year_x,"food_supply_kcal"]*4.184
    CBCflows[,,"households"][,,"nr"] <- CBC[,year_x,"protein_supply"]/6.25
    CBCflows[,,"households"][,,"wm"] <- CBC[,year_x,"food_supply"]
    
    CBCflows<-CBCflows[,,setdiff(getNames(CBCflows,dim = "ElementShort"),c("food_supply_kcal","protein_supply","food_supply","fat_supply"))]
    
    CBCflows[is.na(CBCflows)]<-0
    CBCflows[is.nan(CBCflows)]<-0
    
    out<-CBCflows
    
    ############ Food processing calculations
    
    ### Calculations
    
    #print("Cereal milling")
    
    out<-cereal_milling_global(out)
    
    
    #print("Ethanol production...")
    
    #print("... from temperate cereals")
    
    # Wheat would be more correct, but we need to have homogenous products
    add_ethanol_processing <- function(i){
      out<<-ethanol_processing_c(
        object=out,
        good_in=i,
        ethanol_yield_liter_per_ton=375,
        prod_attributes
      )
    }
    
    add_ethanol_processing_c <- cmpfun(add_ethanol_processing)
    
    invisible(lapply(tece, add_ethanol_processing_c))


    # for(i in tece){
    #   out<-ethanol_processing(
    #     object=out,
    #     good_in=i,
    #     ethanol_yield_liter_per_ton=375,
    #     prod_attributes
    #   )
    # }
    
    
    #print("... from maize")
    
    out<-ethanol_processing_c(
      object=out,
      good_in="2514|Maize and products",
      ethanol_yield_liter_per_ton=408,
      prod_attributes
    )
    
    #print("... from sugarcane")
    
    out<-extract_good_from_flow_c(
      object=out,
      good_in="2536|Sugar cane",
      from="other_util",
      process="distilling",
      extract="X001|Ethanol",
      report_as="ethanol1",
      extraction_quantity=0.654*0.789,
      extraction_attribute="dm",
      residual="distillingloss",
      prod_attributes=prod_attributes
    )
    
    #print("Beer brewing")
    # Barley would be more correct, but we need to have homogenous products
    beercereals<-(tece)
    
    out<-processing_global_c(
      out,
      goods_in=beercereals,
      from="processed",
      process="fermentation",
      # the order matters!
      goods_out=c("2656|Beer"),
      report_as=c("alcohol1"),
      lossname="intermediate"
    )  
    
    add_beercereals <- function(x){
      out<<-extract_good_from_flow_c(
        object=out,
        good_in=x,
        from="intermediate",
        process="intermediate",
        extract="X004|Brewers_grain" ,
        report_as="brewers_grain1",
        extraction_quantity="max",
        extraction_attribute="dm",
        residual="alcoholloss",
        prod_attributes=prod_attributes
      )
    }
    
    add_beercereals_c <- cmpfun(add_beercereals)
    
    invisible(lapply(beercereals, add_beercereals_c))
    
    # for (x in beercereals) {  
    #   out<-extract_good_from_flow(
    #     object=out,
    #     good_in=x,
    #     from="intermediate",
    #     process="intermediate",
    #     extract="X004|Brewers_grain" ,
    #     report_as="brewers_grain1",
    #     extraction_quantity="max",
    #     extraction_attribute="dm",
    #     residual="alcoholloss",
    #     prod_attributes=prod_attributes
    #   )
    # }
    out[,,"intermediate"]<-0
    
    #print("Sugar refining")
    
    #print("... from sugarcane and sugarbeet")
    
    out<-processing_global_c(
      out,
      goods_in=c("2536|Sugar cane","2537|Sugar beet"),
      from="processed",
      process="refining",
      # the order matters!
      goods_out=c("2818|Sugar, Refined Equiv","2544|Molasses"),
      report_as=c("sugar1","molasses1"),
      lossname="refiningloss"
    )  
    
    #print("... from maize (glucose)")
    
    out<-processing_global_c(
      object=out,
      goods_in=c("2514|Maize and products"),
      from="processed",
      process="refining",
      # the order matters!
      goods_out=c("2543|Sweeteners, Other"),
      report_as=c("sugar1"),
      lossname="refiningloss"
    )  
    
    #print("Oil milling")
    
    #print("... from oilpalms")
    out<-oilpalm_processing_c(out,prod_attributes=prod_attributes)
    
    #print("... from soy")
    out<-oil_processing_c(
      object=out,
      goods_in=c("2555|Soyabeans"),
      oil_out=c("2571|Soyabean Oil"),
      cake_out=c("2590|Soyabean Cake"),
      prod_attributes=prod_attributes
    )
    #print("... from groundnuts")
    out<-oil_processing_c(
      object=out,
      goods_in=c("2820|Groundnuts (in Shell Eq)"),
      oil_out=c("2572|Groundnut Oil"),
      cake_out=c("2591|Groundnut Cake"),
      prod_attributes=prod_attributes
    )
    
    #print("... from sunflower")
    out<-oil_processing_c(
      object=out,
      goods_in=c("2557|Sunflower seed"),
      oil_out=c("2573|Sunflowerseed Oil"),
      cake_out=c("2592|Sunflowerseed Cake"),
      prod_attributes=prod_attributes
    )
    
    #print("... from cottonseed")
    out<-oil_processing_c(
      object=out,
      goods_in=c("2559|Cottonseed"),
      oil_out=c("2575|Cottonseed Oil"),
      cake_out=c("2594|Cottonseed Cake"),
      prod_attributes=prod_attributes
    )
    
    #print("... from rapeseed group")
    
    #print("... from Rape and Mustardseed")
    
    out<-oil_processing_c(
      object=out,
      goods_in=c("2558|Rape and Mustardseed"),
      oil_out=c("2574|Rape and Mustard Oil"),
      cake_out=c("2593|Rape and Mustard Cake"),
      prod_attributes=prod_attributes
    )
    
    #print("... from coconuts")
    out<-oil_processing_c(
      out,
      goods_in=c("2560|Coconuts - Incl Copra"),
      oil_out=c("2578|Coconut Oil"),
      cake_out=c("2596|Copra Cake"),
      prod_attributes=prod_attributes
    )
    
    #print("... from sesameseed")
    out<-oil_processing_c(
      out,
      goods_in=c("2561|Sesame seed"),
      oil_out=c("2579|Sesameseed Oil"),
      cake_out="2597|Sesameseed Cake",
      prod_attributes=prod_attributes
    )
    
    #print("... from other oilcrops")
    out<-oil_processing_c(
      out,
      goods_in=c("2570|Oilcrops, Other","2563|Olives (including preserved)"),
      oil_out="2586|Oilcrops Oil, Other",
      cake_out=c("2598|Oilseed Cakes, Other"),
      prod_attributes=prod_attributes
    )
    
    #print("... harmonizing conversion factors within the rapeseed group")
    # ziemlich hässlich
    rapseed_group_in<-c("2558|Rape and Mustardseed","2560|Coconuts - Incl Copra",
                        "2561|Sesame seed","2570|Oilcrops, Other","2563|Olives (including preserved)")
    rapseed_group_oil<-c("2574|Rape and Mustard Oil","2578|Coconut Oil","2579|Sesameseed Oil","2586|Oilcrops Oil, Other")
    rapseed_group_cake<-c("2593|Rape and Mustard Cake","2596|Copra Cake","2597|Sesameseed Cake","2598|Oilseed Cakes, Other")
    rapseed_group_oil_factor<-dimSums(out[,,rapseed_group_in][,,"oil1"],dim=c(1,3.1,3.2))/dimSums(out[,,rapseed_group_in][,,"extracting"],dim=c(1,3.1,3.2))
    rapseed_group_oilcake_factor<-dimSums(out[,,rapseed_group_in][,,"oilcakes1"],dim=c(1,3.1,3.2))/dimSums(out[,,rapseed_group_in][,,"extracting"],dim=c(1,3.1,3.2))
    rapseed_group_losses_factor<-dimSums(out[,,rapseed_group_in][,,"extractionloss"],dim=c(1,3.1,3.2))/dimSums(out[,,rapseed_group_in][,,"extracting"],dim=c(1,3.1,3.2))
    out[,,rapseed_group_in][,,"oil1"]=dimSums(out[,,rapseed_group_in][,,"extracting"],dim=3.2)*rapseed_group_oil_factor
    out[,,rapseed_group_in][,,"oilcakes1"]=dimSums(out[,,rapseed_group_in][,,"extracting"],dim=3.2)*rapseed_group_oilcake_factor
    out[,,rapseed_group_in][,,"extractionloss"]=dimSums(out[,,rapseed_group_in][,,"extracting"],dim=3.2)*rapseed_group_losses_factor
    out[,,"2574|Rape and Mustard Oil"][,,"production_estimated"]<-dimSums(out[,,"2558|Rape and Mustardseed"][,,"oil1"],dim=c(3.1,3.2))
    out[,,"2593|Rape and Mustard Cake"][,,"production_estimated"]<-dimSums(out[,,"2558|Rape and Mustardseed"][,,"oilcakes1"],dim=c(3.1,3.2)) 
    out[,,"2578|Coconut Oil"][,,"production_estimated"]<-dimSums(out[,,"2560|Coconuts - Incl Copra"][,,"oil1"],dim=c(3.1,3.2))
    out[,,"2596|Copra Cake"][,,"production_estimated"]<-dimSums(out[,,"2560|Coconuts - Incl Copra"][,,"oilcakes1"],dim=c(3.1,3.2))
    out[,,"2579|Sesameseed Oil"][,,"production_estimated"]<-dimSums(out[,,"2561|Sesame seed"][,,"oil1"],dim=c(3.1,3.2))
    out[,,"2597|Sesameseed Cake"][,,"production_estimated"]<-dimSums(out[,,"2561|Sesame seed"][,,"oilcakes1"],dim=c(3.1,3.2))
    out[,,"2586|Oilcrops Oil, Other"][,,"production_estimated"]<-dimSums(out[,,c("2570|Oilcrops, Other","2563|Olives (including preserved)")][,,"oil1"],dim=c(3.1,3.2))
    out[,,"2598|Oilseed Cakes, Other"][,,"production_estimated"]<-dimSums(out[,,c("2570|Oilcrops, Other","2563|Olives (including preserved)")][,,"oilcakes1"],dim=c(3.1,3.2))
    
    
    #print("Alcohol production")
    out<-processing_global_c(
      out,
      goods_in=c(other_crops,trce,"2804|Rice (Paddy Equivalent)",potato,cassava_sp,sugar,molasse,"2600|Brans"),
      
      goods_out=c("2655|Wine","2657|Beverages, Fermented","2658|Beverages, Alcoholic"),
      from="processed",
      process="fermentation",
      report_as=c("alcohol1","alcohol2","alcohol3"),
      lossname="alcoholloss"
    )
  #if(!parallel){
   # massbalance<<-mbind(massbalance,out)
    #}
    #else{
      return(out)
    #}
    #assign("massbalance", mbind(massbalance,out), envir = .GlobalEnv)
  }
  
  massbalance_in_yeargroups_c <- cmpfun(massbalance_in_yeargroups)
#  envMbInYeargroupsC <- environment(fun=massbalance_in_yeargroups_c)

  massbalance <- mbind(madlapply(X=years,FUN= massbalance_in_yeargroups_c#, exports=list(list(
    # c("CBC", "attributes_wm", "cereal_milling_global", "tece", "ethanol_processing_c"
    #        ,"extract_good_from_flow_c", "prod_attributes","processing_global_c",
    #        "oilpalm_processing_c","oil_processing_c", "other_crops", "trce", "potato", 
    #        "massbalance", "cassava_sp", "sugar", "molasse")
    #, expression(environment())))
    #,evals=c("compiler","magclass")
    ))

  
  # define use of products that are not existing in FAOSTAT
  
  massbalance[,,c("X002|Distillers_grain","X004|Brewers_grain")][,,c("production","domestic_supply","feed")]<-collapseNames(massbalance[,,c("X002|Distillers_grain","X004|Brewers_grain")][,,"production_estimated"],collapsedim = 2)
  massbalance[,,"X001|Ethanol"][,,c("production","domestic_supply","other_util")]<-collapseNames(massbalance[,,c("X001|Ethanol")][,,"production_estimated"],collapsedim = 2)
  
  #print("add remaining 'processed' to 'other_util'")
  
  massbalance[,,"other_util"]=dimSums(massbalance[,,c("other_util","processed")],dim=3.2)
  massbalance[,,"processed"]<-0
  
  #print("remove empty columns")
  massbalance<-massbalance[,,setdiff(getNames(massbalance,dim = 2),c("processed","intermediate"))]
  
  relationmatrix <- toolGetMapping("FAOitems.rda", type = "sectoral", where="moinput")
  
  relationmatrix <- relationmatrix[,which(names(relationmatrix)%in%c("FoodBalanceItem","k"))]
  relationmatrix <- relationmatrix[-which(duplicated(relationmatrix[,1])==T),]

  massbalance<-toolAggregate(x = massbalance,rel =relationmatrix,dim = 3.1,from = "FoodBalanceItem",to = "k", partrel=TRUE)
  return(list(
    x=massbalance,
    weight=NULL,
    unit="Mt DM, Mt WM, PJ, Mt Nr, Mt P, Mt K",
    description="FAO massbalance calculates all conversion processes within the FAO CBS/FBS and makes them explict. More complete version can be found in calcFAOmassbalance"))
}

# for (year_x in yeargroups) {
# 
#   #print(c("Years: ", year_x))
# 
#   CBCflows<-CBC[,year_x,]*attributes_wm
# 
# 
#   names_processing<-c("production_estimated",
#                       "milling","brans1","branoil1","flour1",
#                       "refining","sugar1","molasses1","refiningloss",
#                       "extracting","oil1","oil2","oilcakes1","extractionloss",
#                       "fermentation","alcohol1","alcohol2","alcohol3","brewers_grain1","alcoholloss",
#                       "distilling","ethanol1","distillers_grain1","distillingloss",
#                       "intermediate",
#                       "households")
#   CBCflows<-add_columns(CBCflows,addnm =names_processing,dim = 3.2)
# 
#   CBCflows[,,"households"][,,"ge"] <- CBC[,year_x,"food_supply_kcal"]*4.184
#   CBCflows[,,"households"][,,"nr"] <- CBC[,year_x,"protein_supply"]/6.25
#   CBCflows[,,"households"][,,"wm"] <- CBC[,year_x,"food_supply"]
# 
#   CBCflows<-CBCflows[,,setdiff(getNames(CBCflows,dim = "ElementShort"),c("food_supply_kcal","protein_supply","food_supply","fat_supply"))]
# 
#   CBCflows[is.na(CBCflows)]<-0
#   CBCflows[is.nan(CBCflows)]<-0
# 
#   out<-CBCflows
# 
#   ############ Food processing calculations
# 
#   ### Calculations
# 
#   #print("Cereal milling")
# 
#   out<-cereal_milling_global(out)
# 
# 
#   #print("Ethanol production...")
# 
#   #print("... from temperate cereals")
# 
#   # Wheat would be more correct, but we need to have homogenous products
#   add_ethanol_processing <- function(i){
#     out<<-ethanol_processing(
#       object=out,
#       good_in=i,
#       ethanol_yield_liter_per_ton=375,
#       prod_attributes
#     )
#   }
#   invisible(lapply(tece, add_ethanol_processing))
# 
#   # for(i in tece){
#   #   out<-ethanol_processing(
#   #     object=out,
#   #     good_in=i,
#   #     ethanol_yield_liter_per_ton=375,
#   #     prod_attributes
#   #   )
#   # }
# 
# 
#   #print("... from maize")
# 
#   out<-ethanol_processing(
#     object=out,
#     good_in="2514|Maize and products",
#     ethanol_yield_liter_per_ton=408,
#     prod_attributes
#   )
# 
#   #print("... from sugarcane")
# 
#   out<-extract_good_from_flow(
#     object=out,
#     good_in="2536|Sugar cane",
#     from="other_util",
#     process="distilling",
#     extract="X001|Ethanol",
#     report_as="ethanol1",
#     extraction_quantity=0.654*0.789,
#     extraction_attribute="dm",
#     residual="distillingloss",
#     prod_attributes=prod_attributes
#   )
# 
#   #print("Beer brewing")
#   # Barley would be more correct, but we need to have homogenous products
#   beercereals<-(tece)
# 
#   out<-processing_global(
#     out,
#     goods_in=beercereals,
#     from="processed",
#     process="fermentation",
#     # the order matters!
#     goods_out=c("2656|Beer"),
#     report_as=c("alcohol1"),
#     lossname="intermediate"
#   )
# 
#   add_beercereals <- function(x){
#     out<<-extract_good_from_flow(
#       object=out,
#       good_in=x,
#       from="intermediate",
#       process="intermediate",
#       extract="X004|Brewers_grain" ,
#       report_as="brewers_grain1",
#       extraction_quantity="max",
#       extraction_attribute="dm",
#       residual="alcoholloss",
#       prod_attributes=prod_attributes
#     )
#   }
#   invisible(lapply(beercereals, add_beercereals))
# 
#   # for (x in beercereals) {
#   #   out<-extract_good_from_flow(
#   #     object=out,
#   #     good_in=x,
#   #     from="intermediate",
#   #     process="intermediate",
#   #     extract="X004|Brewers_grain" ,
#   #     report_as="brewers_grain1",
#   #     extraction_quantity="max",
#   #     extraction_attribute="dm",
#   #     residual="alcoholloss",
#   #     prod_attributes=prod_attributes
#   #   )
#   # }
#   out[,,"intermediate"]<-0
# 
#   #print("Sugar refining")
# 
#   #print("... from sugarcane and sugarbeet")
# 
#   out<-processing_global(
#     out,
#     goods_in=c("2536|Sugar cane","2537|Sugar beet"),
#     from="processed",
#     process="refining",
#     # the order matters!
#     goods_out=c("2818|Sugar, Refined Equiv","2544|Molasses"),
#     report_as=c("sugar1","molasses1"),
#     lossname="refiningloss"
#   )
# 
#   #print("... from maize (glucose)")
# 
#   out<-processing_global(
#     object=out,
#     goods_in=c("2514|Maize and products"),
#     from="processed",
#     process="refining",
#     # the order matters!
#     goods_out=c("2543|Sweeteners, Other"),
#     report_as=c("sugar1"),
#     lossname="refiningloss"
#   )
# 
#   #print("Oil milling")
# 
#   #print("... from oilpalms")
#   out<-oilpalm_processing(out,prod_attributes=prod_attributes)
# 
#   #print("... from soy")
#   out<-oil_processing(
#     object=out,
#     goods_in=c("2555|Soyabeans"),
#     oil_out=c("2571|Soyabean Oil"),
#     cake_out=c("2590|Soyabean Cake")
#   )
#   #print("... from groundnuts")
#   out<-oil_processing(
#     object=out,
#     goods_in=c("2820|Groundnuts (in Shell Eq)"),
#     oil_out=c("2572|Groundnut Oil"),
#     cake_out=c("2591|Groundnut Cake")
#   )
# 
#   #print("... from sunflower")
#   out<-oil_processing(
#     object=out,
#     goods_in=c("2557|Sunflower seed"),
#     oil_out=c("2573|Sunflowerseed Oil"),
#     cake_out=c("2592|Sunflowerseed Cake")
#   )
# 
#   #print("... from cottonseed")
#   out<-oil_processing(
#     object=out,
#     goods_in=c("2559|Cottonseed"),
#     oil_out=c("2575|Cottonseed Oil"),
#     cake_out=c("2594|Cottonseed Cake")
#   )
# 
#   #print("... from rapeseed group")
# 
#   #print("... from Rape and Mustardseed")
# 
#   out<-oil_processing(
#     object=out,
#     goods_in=c("2558|Rape and Mustardseed"),
#     oil_out=c("2574|Rape and Mustard Oil"),
#     cake_out=c("2593|Rape and Mustard Cake")
#   )
# 
#   #print("... from coconuts")
#   out<-oil_processing(
#     out,
#     goods_in=c("2560|Coconuts - Incl Copra"),
#     oil_out=c("2578|Coconut Oil"),
#     cake_out=c("2596|Copra Cake")
#   )
# 
#   #print("... from sesameseed")
#   out<-oil_processing(
#     out,
#     goods_in=c("2561|Sesame seed"),
#     oil_out=c("2579|Sesameseed Oil"),
#     cake_out="2597|Sesameseed Cake"
#   )
# 
#   #print("... from other oilcrops")
#   out<-oil_processing(
#     out,
#     goods_in=c("2570|Oilcrops, Other","2563|Olives (including preserved)"),
#     oil_out="2586|Oilcrops Oil, Other",
#     cake_out=c("2598|Oilseed Cakes, Other")
#   )
# 
#   #print("... harmonizing conversion factors within the rapeseed group")
#   # ziemlich hässlich
#   rapseed_group_in<-c("2558|Rape and Mustardseed","2560|Coconuts - Incl Copra",
#                    "2561|Sesame seed","2570|Oilcrops, Other","2563|Olives (including preserved)")
#   rapseed_group_oil<-c("2574|Rape and Mustard Oil","2578|Coconut Oil","2579|Sesameseed Oil","2586|Oilcrops Oil, Other")
#   rapseed_group_cake<-c("2593|Rape and Mustard Cake","2596|Copra Cake","2597|Sesameseed Cake","2598|Oilseed Cakes, Other")
#   rapseed_group_oil_factor<-dimSums(out[,,rapseed_group_in][,,"oil1"],dim=c(1,3.1,3.2))/dimSums(out[,,rapseed_group_in][,,"extracting"],dim=c(1,3.1,3.2))
#   rapseed_group_oilcake_factor<-dimSums(out[,,rapseed_group_in][,,"oilcakes1"],dim=c(1,3.1,3.2))/dimSums(out[,,rapseed_group_in][,,"extracting"],dim=c(1,3.1,3.2))
#   rapseed_group_losses_factor<-dimSums(out[,,rapseed_group_in][,,"extractionloss"],dim=c(1,3.1,3.2))/dimSums(out[,,rapseed_group_in][,,"extracting"],dim=c(1,3.1,3.2))
#   out[,,rapseed_group_in][,,"oil1"]=dimSums(out[,,rapseed_group_in][,,"extracting"],dim=3.2)*rapseed_group_oil_factor
#   out[,,rapseed_group_in][,,"oilcakes1"]=dimSums(out[,,rapseed_group_in][,,"extracting"],dim=3.2)*rapseed_group_oilcake_factor
#   out[,,rapseed_group_in][,,"extractionloss"]=dimSums(out[,,rapseed_group_in][,,"extracting"],dim=3.2)*rapseed_group_losses_factor
#   out[,,"2574|Rape and Mustard Oil"][,,"production_estimated"]<-dimSums(out[,,"2558|Rape and Mustardseed"][,,"oil1"],dim=c(3.1,3.2))
#   out[,,"2593|Rape and Mustard Cake"][,,"production_estimated"]<-dimSums(out[,,"2558|Rape and Mustardseed"][,,"oilcakes1"],dim=c(3.1,3.2))
#   out[,,"2578|Coconut Oil"][,,"production_estimated"]<-dimSums(out[,,"2560|Coconuts - Incl Copra"][,,"oil1"],dim=c(3.1,3.2))
#   out[,,"2596|Copra Cake"][,,"production_estimated"]<-dimSums(out[,,"2560|Coconuts - Incl Copra"][,,"oilcakes1"],dim=c(3.1,3.2))
#   out[,,"2579|Sesameseed Oil"][,,"production_estimated"]<-dimSums(out[,,"2561|Sesame seed"][,,"oil1"],dim=c(3.1,3.2))
#   out[,,"2597|Sesameseed Cake"][,,"production_estimated"]<-dimSums(out[,,"2561|Sesame seed"][,,"oilcakes1"],dim=c(3.1,3.2))
#   out[,,"2586|Oilcrops Oil, Other"][,,"production_estimated"]<-dimSums(out[,,c("2570|Oilcrops, Other","2563|Olives (including preserved)")][,,"oil1"],dim=c(3.1,3.2))
#   out[,,"2598|Oilseed Cakes, Other"][,,"production_estimated"]<-dimSums(out[,,c("2570|Oilcrops, Other","2563|Olives (including preserved)")][,,"oilcakes1"],dim=c(3.1,3.2))
# 
# 
#   #print("Alcohol production")
#   out<-processing_global(
#     out,
#     goods_in=c(other_crops,trce,"2804|Rice (Paddy Equivalent)",potato,cassava_sp,sugar,molasse,"2600|Brans"),
# 
#     goods_out=c("2655|Wine","2657|Beverages, Fermented","2658|Beverages, Alcoholic"),
#     from="processed",
#     process="fermentation",
#     report_as=c("alcohol1","alcohol2","alcohol3"),
#     lossname="alcoholloss"
#   )
# 
#   massbalance<-mbind(massbalance,out)
# }
