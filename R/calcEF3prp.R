#' @title calcEF3prp
#' @description Returns emission factor for manure excreted during pasture range and paddock. Differs depending on the share of small ruminants.
#'
#' @return list of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @param select_years if only one year is selected, years is set to NULL
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EF3prp")
#' }
#' @importFrom utils read.csv


calcEF3prp<-function(select_years="y2005"){
  excretion<-collapseNames(calcOutput("ExcretionIPCC",aggregate = F)[,,"pasture_range_paddock"])
  #excretion<-collapseNames(calcOutput("Excretion",aggregate = F)[,,"grazing"][,,"nr"])
  #excretion<-stocks
  cpp<-c("dairy cows","other cattle","dairy buffalo","other buffalo",  
         "market swine","breeding swine","poultry layers","broilers",       
         "turkey","ducks")
  
  so<-c("dairy sheep","other sheep",    
        "dairy goats","other goats",
        "dairy camels","other camels",   
        "horses","mules and asses")
  ef3prp_cpp<-0.02
  ef3prp_so<-0.01
  emis<-excretion
  emis[,,cpp]<-excretion[,,cpp]*ef3prp_cpp
  emis[,,so]<-excretion[,,so]*ef3prp_so
  
  relationmatrix <- read.csv(toolMappingFile("sectoral","IPCCitems.csv")) 
  emis<-toolAggregate(x = emis,rel =relationmatrix,dim = 3.1,from = "ipcc",to = "magpie", partrel=TRUE)
  excretion<-toolAggregate(x = excretion,rel =relationmatrix,dim = 3.1,from = "ipcc",to = "magpie", partrel=TRUE)
  
  ef3prp=emis/excretion
  ef3prp[is.na(ef3prp)]<-0.02
  
  ef3prp<-add_dimension(ef3prp,dim=3.1,add="n_pollutants_direct",nm="n2o_n_direct")
  ef3prp<-add_columns(ef3prp,addnm = c("no3_n","nh3_n","no2_n"),dim = 3.1)
  ipcc<-setYears(readSource("IPCC","emissionfactors",convert = FALSE),NULL)
  fracleach=calcOutput("IPCCfracLeach",aggregate = FALSE,cellular=FALSE)[,,"past"]
  ef3prp[,,"no3_n"]<-fracleach
  ef3prp[,,"nh3_n"]<-ipcc[,,"frac_gasm"]*(1-ipcc[,,"no2_share_volat"])
  ef3prp[,,"no2_n"]<-ipcc[,,"frac_gasm"]*ipcc[,,"no2_share_volat"]
    
  vcat(verbosity = 2, "As there are only zeros in weight of aggregation, I will set it to one for pigs. dangerous!")
  weight<-ef3prp
  weight[,,]<-0
  weight<-excretion
  weight[,,"livst_pig"]<-1
  
  ef3prp<-toolHoldConstantBeyondEnd(ef3prp)
  weight<-toolHoldConstantBeyondEnd(ef3prp)
  
  ef3prp=ef3prp[,select_years,]
  weight=weight[,select_years,]
  
  # this is for using it as magpie input without time dimension
  if(length(select_years)==1){
    ef3prp=setYears(ef3prp,NULL)
    weight=setYears(weight,NULL)
  }
  
  
  return(list(x=ef3prp,
              weight=weight,
              unit="Tg N2O-N/Tg N",
              min=0,
              max=0.4,
              description="direct soil emissions from manure excreted during pasture")
  ) 
}
