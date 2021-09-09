#' @importFrom magclass getNames

calcLabour <- function() {
  
  aged <- c( "Population|Female|Aged15-19",
             "Population|Female|Aged20-24",
             "Population|Female|Aged25-29",
             "Population|Female|Aged30-34",
             "Population|Female|Aged35-39",
             "Population|Female|Aged40-44",
             "Population|Female|Aged45-49",
             "Population|Female|Aged50-54",
             "Population|Female|Aged55-59",
             "Population|Female|Aged60-64",
             "Population|Male|Aged15-19",
             "Population|Male|Aged20-24",
             "Population|Male|Aged25-29",
             "Population|Male|Aged30-34",
             "Population|Male|Aged35-39",
             "Population|Male|Aged40-44",
             "Population|Male|Aged45-49",
             "Population|Male|Aged50-54",
             "Population|Male|Aged55-59",
             "Population|Male|Aged60-64" )
 
  data <- collapseNames(readSource("SSP",subtype="all")[,,aged][,,"IIASA-WiC POP"])
  data <- dimSums(data,dim=3.2)
  
  getNames(data) <- paste("pop_",gsub("_v[[:alnum:],[:punct:]]*","",getNames(data)),sep="")
  # change name of "SSP4d" to "SSP4
  getNames(data)<-sub("SSP4d","SSP4",getNames(data))
  
  time_extend <- c("y2105","y2110","y2115","y2120","y2125","y2130","y2135","y2140","y2145","y2150")
  data <- time_interpolate(data,time_extend,extrapolation_type="constant",integrate_interpolated_years=TRUE)
  
  # delete 0s
  data <- data[,c("y2000","y2005"),,invert=TRUE]
  # extrapolate data for 2005
  data <- time_interpolate(data,c("y2005"),extrapolation_type="linear",integrate_interpolated_years=TRUE)
  
  # add SDP, SDP_EI, SDP_MC, SDP_RC scenario as copy of SSP1, might be substituted by real data later
  if("pop_SSP1" %in% getNames(data,dim=1)){
     if(!("pop_SDP" %in% getNames(data,dim=1))){
      data_SDP <- data[,,"pop_SSP1"]
      getNames(data_SDP) <- gsub("pop_SSP1","pop_SDP",getNames(data_SDP))
      data <- mbind(data,data_SDP)
    }
     if(!("pop_SDP_EI" %in% getNames(data,dim=1))){
      data_SDP_EI <- data[,,"pop_SSP1"]
      getNames(data_SDP_EI) <- gsub("pop_SSP1","pop_SDP_EI",getNames(data_SDP_EI))
      data <- mbind(data,data_SDP_EI)
    }
     if(!("pop_SDP_MC" %in% getNames(data,dim=1))){
      data_SDP_MC <- data[,,"pop_SSP1"]
      getNames(data_SDP_MC) <- gsub("pop_SSP1","pop_SDP_MC",getNames(data_SDP_MC))
      data <- mbind(data,data_SDP_MC)
    }
     if(!("pop_SDP_RC" %in% getNames(data,dim=1))){
      data_SDP_RC <- data[,,"pop_SSP1"]
      getNames(data_SDP_RC) <- gsub("pop_SSP1","pop_SDP_RC",getNames(data_SDP_RC))
      data <- mbind(data,data_SDP_RC)
    }
  }
  # add SSP2Ariadne scenario as copy of SSP2, might be substituted by real data later
  if("pop_SSP2" %in% getNames(data,dim=1)){
     if(!("pop_SSP2Ariadne" %in% getNames(data,dim=1))){
      data_SSP2Ariadne <- data[,,"pop_SSP1"]
      getNames(data_SSP2Ariadne) <- gsub("pop_SSP1","pop_SSP2Ariadne",getNames(data_SSP2Ariadne))
      data <- mbind(data,data_SSP2Ariadne)
    }
  }
  
  return(list(x=data,weight=NULL,unit="million",description="Labour data"))
}
