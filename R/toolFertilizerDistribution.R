#' @title toolFertilizerDistribution
#' @description Disaggregates fertilizer usage, trying to best match a certain soil nitrogen uptake efficiency (SNUpE). Also used in magpie4 library
#' 
#' @export
#'
#' @param iteration_max maximum iteration for downscaling
#' @param max_snupe the maximum level of nue or snupe
#' @param mapping mapping used for disaggregation
#' @param from name of from column in mapping
#' @param to name of to column in mapping
#' @param fertilizer total inorganic fertilizer to be distributed on regional leve
#' @param SNUpE Nitrogen use efficiency or SNUPE on regional level which should be matched best possible
#' @param withdrawals nitrogen withdrawals on cell level
#' @param organicinputs non-inroganic fertilizer inputs on cell level
#' @return magpie object with fertilizer usage on cell level
#' @author Benjamin Leon Bodirsky
#' @seealso \code{\link{calcNitrogenBudgetCropland}}
#' 
#' @export

toolFertilizerDistribution<-function(iteration_max=20, max_snupe=0.85, mapping, from, to, fertilizer, SNUpE, withdrawals, organicinputs) {
  
  for (iteration in 1:iteration_max){
    cat(paste0(" iteration: ",iteration)," ")
    #cat(paste0(" NUE ",round(NUE["DEU",2010,],2))," ")
    requiredinputs=withdrawals/SNUpE
    requiredinputs[is.nan(requiredinputs)]<-0
    requiredfertilizer = requiredfertilizer_nonnegative= requiredinputs - organicinputs
    requiredfertilizer_nonnegative[requiredfertilizer_nonnegative<0]=0
    # negative required fertilizers indicate that organic fertilizers are sufficent to satisfy the needs. 
    requiredfertilizer_nonnegative_country = toolAggregate(requiredfertilizer_nonnegative,rel=mapping,from=from,to=to,partrel=T)
    surplus_fertilizer=requiredfertilizer_nonnegative_country-fertilizer[getRegions(requiredfertilizer_nonnegative),,]
    cat(paste0("  surplus_fertilizer in 2010:",sum(abs(surplus_fertilizer)),";"))
    if(sum(abs(surplus_fertilizer),na.rm=T)>1){  # 1 is an arbitrary threshold
      SNUpE = (
        groupAggregate(withdrawals,dim = 1,query = mapping,from=from,to=to)
        /(groupAggregate(organicinputs+requiredfertilizer,dim = 1,query = mapping,from=from,to=to) 
          - surplus_fertilizer
        )
      )
      SNUpE[is.na(SNUpE)]<-max_snupe
    } else {
      break
    }
  }
  if(sum(abs(surplus_fertilizer),na.rm=T)>1){
    print(surplus_fertilizer)
    cat(1,"fertilizer distribution procedure found no equilibrium")
  }
  #snupe_cell=withdrawals/(organicinputs+requiredfertilizer_nonnegative)
  fert=requiredfertilizer_nonnegative

  return (fert)
}
