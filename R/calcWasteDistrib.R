#' @title calcWasteDistrib
#' @description 
#' rule-based distribution of waste by composition type to disposal type. 
#' returns list of magpie object, share of total disposal
#' @return Magpie object of waste types to waste distribution,  percentage
#' @author David Chen
#' @examples
#' 
#' \dontrun{ a <- calcOutput(type="WasteDistrib")
#' }
#' @importFrom magclass add_columns
#' @importFrom dplyr rename

calcWasteDistrib <- function(){
  
  gen <-readSource("Waste", subtype="Generation", convert=T)
  comp <-readSource("Waste", subtype="Composition", convert = T)
  treat <-readSource("Waste", subtype="Treatment", convert = T)

  #take out countries that don't have composition data that matches generation data???

org_total <- dimSums(comp[,,c("food","yard")], na.rm=T)
getNames(org_total) <- "organic"
org_total[org_total[,,"organic"]== 0] <- NA

comp <- comp[,,c("glass","metal","other","paper_cardboard","plastic","rubber_leather","wood_waste")]   

comp <- mbind(org_total, comp)
  
  
compost <- dimSums(treat[,,c("compost","anaerobic_digestion")], na.rm=T)
getNames(compost) <- "compost"
compost[compost[,,"compost"]== 0] <- NA

dumps <- dimSums(treat[,,c("open_dump","unaccounted_for",
                           "waterways_marine","other")], na.rm=T)
getNames(dumps) <- "dumps"
dumps[dumps[,,"dumps"]== 0] <- NA

treat <- treat[,,c("controlled_landfill","landfill_unspecified",
                   "sanitary_landfill_landfill_gas_system", "recycling",
                   "incineration")]
treat <- mbind(treat, compost, dumps)

comp_shr <- comp/gen
treat_shr <- treat/gen


#send food_yard to compost first 
treat[is.na(treat[,,"compost"])] <- 0
comp[is.na(comp[,,"organic"])] <- 0
org_rem <- comp[,,"organic"] - treat[,,"compost"]
org_rem[org_rem[,,"compost"]<0] <- 0 #the negatives set to 0
getNames(org_rem) <- "org_rem"
org_rem_shr <- org_rem/gen
comp_shr <- mbind(comp_shr, org_rem_shr)

compost_rem <- treat[,,"compost"] - org_total
compost_rem[compost_rem[,,"compost"]<0] <- 0 
getNames(compost_rem) <- "compost_rem"
compost_rem_shr <- compost_rem/gen
treat_shr <- mbind(treat_shr, compost_rem_shr)


distribute <- function(input, include, exclude) {
  total <- dimSums(treat_shr[,,include], na.rm=T)
  compensated_shr <- treat_shr[,,include]/total
  compensated_shr <- add_columns(compensated_shr, addnm=exclude)
  compensated_shr[,,exclude] <- 0
  x <-comp_shr[,,input] * compensated_shr

  x<- collapseNames(x, collapsedim=c(2))
  x <- x[,,c("controlled_landfill", "landfill_unspecified","sanitary_landfill_landfill_gas_system",
             "recycling", "incineration","compost_rem", "dumps")]                                
  return(x)
}
  

org_treat<- distribute(input=c("org_rem"), include= c("controlled_landfill","landfill_unspecified",
                                                 "sanitary_landfill_landfill_gas_system", "dumps", "incineration"),
                        exclude= c("recycling", "compost", "compost_rem"))

glass_metal_treat <- distribute(input=c("glass","metal"), include= c("controlled_landfill","landfill_unspecified",
                                                                   "sanitary_landfill_landfill_gas_system", "dumps", "recycling"),
                                exclude= c("compost","incineration", "compost_rem"))

#wood also goes to everything?
wood_treat <- distribute(input="wood_waste", include= c("controlled_landfill","landfill_unspecified", "incineration", "recycling",
                                                              "sanitary_landfill_landfill_gas_system", "dumps", "compost_rem"),
                         exclude= c("compost"))

# paper_cardboard goes to everything?
paper_cardboard_treat <- distribute(input="paper_cardboard", include= c("controlled_landfill","landfill_unspecified", "incineration",
                                                              "sanitary_landfill_landfill_gas_system", "dumps", "compost_rem", "recycling"),
                              exclude="compost")
                        

#no compost or recycling for other
other_treat <- distribute(input="other", include= c("controlled_landfill","landfill_unspecified", "incineration",
                                                     "sanitary_landfill_landfill_gas_system", "dumps"),
                         exclude= c("recycling","compost", "compost_rem"))

plastic_rubber_treat <- distribute(input=c("plastic", "rubber_leather"), include= c("controlled_landfill","landfill_unspecified", 
                                                                                    "incineration", "sanitary_landfill_landfill_gas_system", 
                                                                                    "dumps", "recycling"),
                                          exclude= c("compost", "compost_rem"))

org_compost <- treat_shr[,,"compost"]
org_compost[is.na(org_compost[,,"compost"])] <- 0

#because INDIA (and slovenia and Madagascar) have treatment data for compost, but no composition data,
# this messes with the regressions because their proportion of composted organic VS other treatments of organic
#is only 1, because there is no other data for the other treatments for compost, since it is not distributed
#other solution is to distribute generation data over all compositions somehow
a<- gen - dimSums(comp, na.rm=T)
tmp <- where(a[,,1] > (100))  # find where generation and composition data are way off, no big negatives
org_compost[c(tmp$true$regions),,] <-0 #set these countries to 0




x <- mbind(org_treat, glass_metal_treat, wood_treat, paper_cardboard_treat, other_treat, plastic_rubber_treat)

x[,,"org_rem.compost_rem"] <- org_compost

getNames(x) <- gsub("org_rem", replacement="organic",getNames(x))
getNames(x) <- gsub("compost_rem", replacement="compost",getNames(x))



return(list(
     x=x,
     unit="percentage",
     description="WB What a Waste 2.0 dataset waste types distributed by treatment type"))

}

#No glass or metal to WTE or compost
# total_disp_shr_no_wte <- dimSums(disp_shr[,,c("dumps","landfills","recycled")])
# non_wte_disp_shr <- disp_shr[,,c("dumps","landfills","recycled")]/ total_disp_shr_no_wte
# non_wte_disp_shr <- add_columns(non_wte_disp_shr, addnm=c("wte", "compost"))
# non_wte_disp_shr[,,c("wte","compost")] <- 0
# 
# glass_disp <- comp_gen[c(commonregions),,"glass"] * non_wte_disp_shr[c(commonregions),,]
# metal_disp <- comp_gen[c(commonregions),,"metal"] * non_wte_disp_shr[c(commonregions),,]
# 


# }
  
  
  
# gen <- readSource("Waste", subtype="Generation", convert=F)
# comp_shr <- readSource("Waste", subtype="Composition", convert=F)
# disp_shr <- readSource("Waste", subtype="Disposal", convert=F)
# 
# comp_gen<- comp_shr[,,c("organic","paper","plastic","glass","metal","other_comp")]*comp_shr[,,"gen_comp"]/100
# comp_gen <- collapseNames(comp_gen)
# disp_gen<- disp_shr[,,c("dumps","landfills","compost","recycled","wte")]*disp_shr[,,"gen_disp"]/100
# disp_gen <- collapseNames(disp_gen)
# 
# #common regions for non-converted data, can take out all if full regions
# commonregions <- intersect(getRegions(comp_shr), getRegions(disp_shr))
# 
# #assuming all compost comes from organic (not paper)
# org_rem <- comp_gen[c(commonregions),,"organic"] - disp_gen[c(commonregions),,"compost"]
# getNames(org_rem) <- "organic"
# #for the countries that have more compost than organic, set the remaining organic to 0
# org_rem[org_rem[,,"organic"]<0] <- 0
# 
# #Assuming remaining organic waste is not recycled, and compost added later
# total_disp_shr_no_recyc <- dimSums(disp_shr[,,c("dumps","landfills","wte")])
# non_recyc_disp_shr <- disp_shr[,,c("dumps","landfills","wte")]/ total_disp_shr_no_recyc
# non_recyc_disp_shr <- add_columns(non_recyc_disp_shr, addnm=c("recycled", "compost"))
# non_recyc_disp_shr[,,c("recycled", "compost")] <- 0
# 
# org_rem_disp <- org_rem * non_recyc_disp_shr[c(commonregions),,]
# 
# #compost from organic is simply amount of original compost, except when bigger than organic, then reduce to equal to organic
# org_compost <- disp_gen[c(commonregions),,"compost"]
# 
# for(i in c(commonregions)){
#   if (org_compost[i,,"compost"]>comp_gen[i,,"organic"]) {
#     org_compost[i,,"compost"] <- comp_gen[i,,"organic"]}}
# 
# org_rem_disp[,,"compost"] <- org_compost
# org_disp <- org_rem_disp
# 
# #No glass or metal to WTE or compost
# total_disp_shr_no_wte <- dimSums(disp_shr[,,c("dumps","landfills","recycled")])
# non_wte_disp_shr <- disp_shr[,,c("dumps","landfills","recycled")]/ total_disp_shr_no_wte
# non_wte_disp_shr <- add_columns(non_wte_disp_shr, addnm=c("wte", "compost"))
# non_wte_disp_shr[,,c("wte","compost")] <- 0
# 
# glass_disp <- comp_gen[c(commonregions),,"glass"] * non_wte_disp_shr[c(commonregions),,]
# metal_disp <- comp_gen[c(commonregions),,"metal"] * non_wte_disp_shr[c(commonregions),,]
# 
# #no compost for paper or plastic or other
# total_disp_shr_no_compost <- dimSums(disp_shr[,,c("dumps","landfills","recycled", "wte")])
# non_compost_disp_shr <- disp_shr[,,c("dumps","landfills","wte","recycled")]/ total_disp_shr_no_compost
# non_compost_disp_shr <- add_columns(non_compost_disp_shr, addnm=c("compost"))
# non_compost_disp_shr[,,c("compost")] <- 0
# 
# paper_disp <- comp_gen[c(commonregions),,"paper"] * non_compost_disp_shr[c(commonregions),,]
# plastic_disp <- comp_gen[c(commonregions),,"plastic"] * non_compost_disp_shr[c(commonregions),,]
# other <- comp_gen[c(commonregions),,"other_comp"] * non_compost_disp_shr[c(commonregions),,]
# 
# 
# 
# 
# waste_distribution_gen <- mbind(org_disp, paper_disp, plastic_disp, glass_disp, metal_disp, other)
# waste_distribution_gen <- toolCountryFill(waste_distribution_gen, fill=NA)
# 
#  if(percapita==TRUE) {
# pop <- calcOutput("Population", aggregate = F,naming="indicator.scenario")
# pop2005 <- pop[,"y2005","pop.SSP2"]
# urb_shr <- calcOutput("Urban", aggregate=F, naming="indicator.scenario")
# total_urb2005 <- pop2005*urb_shr[,"y2005","pop.SSP2"]
# waste_distribution_gen <- waste_distribution_gen/total_urb2005
# waste_distribution_gen <-collapseNames(waste_distribution_gen)
# 
#  }
# 
# return(list(
#   x=waste_distribution_gen,
#   unit="Mt or t/cap ww",
#   description="WB What a Waste dataset waste types distributed by disposal type"))
# }

