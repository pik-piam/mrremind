#' @title calcResFieldBalancePast
#' @description Calculates data for aboveground and belowground residues production with other usage
#' 
#' @param cellular If TRUE calculation and output on cellular level
#' @param products "sum" (default) or "kres"
#' @return data
#' @author Benjamin Bodirsky
#' @seealso \code{\link{calcOutput}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ResFieldBalancePast")
#' 
#' }
#'

calcResFieldBalancePast<-function(cellular = FALSE, products = "sum"){
  
  if(products=="kres"){
    past              <- findset("past")
    relevant_nutrients <- c("nr","p","k","c")  # after burning, unclear what dm and ge may be
    
    production        <- collapseNames(calcOutput("ResBiomass", cellular=cellular, plantparts="ag", aggregate = FALSE))[,,relevant_nutrients]
    
    burnshr           <- calcOutput("ResCombustEff",aggregate = FALSE)[,,getNames(production,dim=1)]
    dev_state_past    <- collapseNames(calcOutput("DevelopmentState",aggregate = F)[,past,"SSP2"])
    if(cellular){
      
      options(magclass_expand_version=1)
      on.exit(options(magclass_expand_version=2))
      
      dev_state_past    <- toolIso2CellCountries(dev_state_past)
    }
    
    # if the following parameters are changed, they also have to be changed
    # in the GAMS code!
    burn    <-  ash               <- production * (dev_state_past*0.15+(1-dev_state_past)*0.25)
    ash[,,c("c","nr")] <- ash[,,c("c","nr")]*(1-burnshr)  ## assuming the same for C and Nr, maybe has to be updated
    burn                          <- burn - ash  
    
  
      
    mapping       <- toolMappingFile(type = "sectoral",name = "kcr_kres.csv",readcsv = TRUE)
    burn          <- toolAggregate(burn,rel = mapping,from = "kcr",to="kres",dim=3.1)
    ash           <- toolAggregate(ash,rel = mapping,from = "kcr",to="kres",dim=3.1)
    production    <- toolAggregate(production,rel = mapping,from = "kcr",to="kres",dim=3.1)

    
    ### estimate removal
    
    if(cellular==TRUE){
 
      # to avoid negative values, take the regional share of removal by product
      fieldbalance <- calcOutput("ResFieldBalancePast",cellular=FALSE,aggregate=FALSE,products="kres")
      # use nr for removalshare decision
      removalshare<-collapseNames((fieldbalance[,,"removal"]/(fieldbalance[,,"biomass"]-fieldbalance[,,"burned"]-fieldbalance[,,"ash"]))[,,"nr"])
      removalshare[is.nan(removalshare)]<-1
      CellToCellIso  <- toolMappingFile("cell","CountryToCellMapping.csv",readcsv = TRUE)
      removalshare<-toolAggregate(x=removalshare,rel=CellToCellIso,from = "iso",to="celliso",partrel=T)
      removal <- (production-burn-ash)*removalshare
    } else {
      removal      <- collapseNames(calcOutput("ResDemand", cellular=FALSE, aggregate = FALSE)[,,"domestic_supply"])[,,relevant_nutrients]
      removal      <- add_columns(removal,addnm = c("res_nouse"),dim = 3.1)
      removal[,,c("res_nouse")] <- 0
    }
    
    recycle <- production - removal - burn  
    
    ### check for negative recycling shares and decrease removal if nesseccary
    correct_removal <- recycle
    correct_removal[correct_removal>0] <- 0
    
    removal <- removal + correct_removal
    recycle <- round(production - removal - burn, 8)
    removal <- round(removal, 8)
    
    
    if(any(correct_removal!=0)){
      vcat(2,"Residue removal was corrected in areas, where there was not enough residue biomass available.")
    }
    
    ### generate output
    out<-mbind(
      add_dimension(production,dim = 3.1,nm = "biomass"),
      add_dimension(removal,dim = 3.1,nm ="removal"),
      add_dimension(burn,dim = 3.1,nm ="burned"),
      add_dimension(ash,dim = 3.1,nm ="ash"),
      add_dimension(recycle,dim = 3.1,nm ="recycle")) 
    
  } else if(products=="sum"){
  
    out<-calcOutput("ResFieldBalancePast", cellular = cellular, products = "kres",aggregate=FALSE)
    out<-dimSums(out,dim=3.2)
  
  } else {stop("Product category not avaiable!")}
  
  return(list(x=out,
              weight=NULL,
              unit="Mt DM, Nr, P, K, WM, Pj Energy",
              description="Crop Residues Field Production and use",
              min = 0,
              isocountries =!cellular))
}