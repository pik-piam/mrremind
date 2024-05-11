#' Read RemindCesPrices
#'
#' Read-in CES derivatives/prices from former REMIND runs
#'
#'
#' @return magpie object of REMIND prices
#' @author Antoine Levesque
#' @seealso \code{\link{readSource}}
#' @param subtype Regional resolution of REMIND data which should be loaded. ccd632d33a corresponds to the REMIND-11,
#'  and 690d3718e1 to REMIND-H12
#'
#' @examples
#' \dontrun{ a <- readSource(type="RemindCesPrices", subtype = "ccd632d33a")
#' }
#' @importFrom quitte inline.data.frame
#'
readRemindCesPrices <- function(subtype = "ccd632d33a") {

  #-----FUNCTIONS--------
  loadfiles = function(regmap){
    files2load = list.files(pattern = paste0(regmap,".inc$"))

    data = NULL

    for (.file in files2load){

      settings_file = as.list(unlist(strsplit(.file,"-")))
      names_list = unlist(lapply(settings_file, function(x) sub("^([^_]+)_.+$","\\1",x)))
      names(settings_file) = names_list
      settings_file = lapply(settings_file, function(x) sub("^([^_]+)_(.+$)","\\2",x))

      tmp = readLines(.file,encoding = "UTF-8")
      tmp = gsub("(pm_cesdata\\(|\\)| |\"|;)","",tmp )
      tmp = gsub("=",",",tmp)
      tmp = grep("price",tmp,value = T)
      tmp = c("period,region,variable,parameter,value", tmp)
      tmp = inline.data.frame(tmp,sep = ",")

      data = rbind(data,tmp)
    }
    data <- data %>%
      group_by(!!!syms(setdiff(colnames(data), "value"))) %>%
      summarise(value = mean(.data$value)) %>%
      ungroup()

    data$parameter = NULL
    return(data)
  }
  #-----END FUNCTIONS----


  if (!subtype %in% c("ccd632d33a", "690d3718e1"))  stop("valid subtypes are 'ccd632d33a', '690d3718e1'")

  mdata = as.magpie(loadfiles(subtype))



  return(mdata)
}
