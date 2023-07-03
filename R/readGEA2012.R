#' @title read GEA 2012
#' @description Read in datafiles comprising fossil fuel data from the Global Energy Assessment 2012
#' @param subtype Type of fossil fuel and type of data (oil, coal, or gas + costs, qtys, or dec)
#' @return MAgPIE object of the GEA data
#' @author Stephen Bi
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("GEA2012","coal")
#' }
#' 
#' @importFrom readxl read_excel
#' @importFrom madrat toolNAreplace
#' @importFrom dplyr relocate mutate

readGEA2012 <- function(subtype) {
  EJ_2_TWyr <- 1/31.536
  ts1 <- 5
  ts2 <- 10
  ttot <- c(seq(2005,2055,ts1),seq(2060,2150,ts2))
  t_cutoff <- 2050
  t0 <- 2020
  t_trans <- (t_cutoff - t0)
  #================================================================
  # Data retrieval and processing function selection
  #================================================================
  ffTypeData <- list()
  ffTypeScenData <- list()
  tmp <- NULL
  enty <- NULL
  scenario <- NULL
  
  #Ordering of SSPs in this vector corresponds to ordering of coded scenarios in "Scenario data XX.xlsx"
  scen <- c('SSP5','SSP2','SSP1','SSP3','SSP4')
  if ("gas" %in% subtype)  ffType <- c('SHG-rv','COG-rv','CMG-rv','TIG-rv','HYG-rv','DEG-rv','SHG-rs','COG-rs','CMG-rs','TIG-rs','HYG-rs','DEG-rs')
  if ("oil" %in% subtype)  ffType <- c('TAO-rv','SHO-rv','EHO-rv','COO-rv','TAO-rs','SHO-rs','EHO-rs','COO-rs')
  #subtype <- c('SHG-rv','COG-rv','CMG-rv','TIG-rv','HYG-rv','DEG-rv','SHG-rs','COG-rs','CMG-rs','TIG-rs','HYG-rs','DEG-rs',
  #             'TAO-rv','SHO-rv','EHO-rv','COO-rv','TAO-rs','SHO-rs','EHO-rs','COO-rs','HAC','LIC')
  
  if ("coal" %in% subtype) {
    rawData <- read.csv2("Scenario Data HAC_LIC.csv",header=TRUE,as.is = T)
    rawData$grade <- as.factor(rawData$grade)
    rawData$value <- as.numeric(rawData$value)
    rawData <- rawData %>% mutate(enty="pecoal") %>% relocate(enty,.before=scenario)
    out <- setYears(as.magpie(rawData),ttot[1])
    tmp <- out
    #out <- new.magpie(unique(rawData$region),ttot,names=paste(rawData$scenario,rawData$xi,rawData$grade,sep="."),fill=0)
    for (rlf in 2:length(unique(rawData$grade))) {
      mselect(out,grade=rlf,xi="xi3") <- mselect(tmp,grade=rlf,xi="xi3") - mselect(tmp,grade=rlf-1,xi="xi3")
    }
    for (ts in ttot[-1])  out <- mbind(out,setYears(out[,ttot[1],],ts))
    out[,,"xi3"] <- out[,,"xi3"]/EJ_2_TWyr
    out[,,c("xi1","xi2")] <- out[,,c("xi1","xi2")]*EJ_2_TWyr
    
  }else {
    # Loop over FF types
    for (i in ffType) {
      #Read FF type data
      typeFilename <- paste0("FF data ",i,".xlsx")
      rawData <- as.data.frame(readxl::read_excel(typeFilename))
      rawData <- rawData[,which(!is.na(rawData[1,]))]
      if (i==ffType[1]) {
        regions <- rawData[,"Region code"]
        nreg <- dim(rawData)[1]
      }
      #Read appropriate pre-processing function from the file
      ppFunc <- as.character(rawData[1,"R Pre-Proc Function"])
      ppFunc <- source(file=paste0(ppFunc,".r"))[[1]]
      #Retain only numerical data
      numData <- rawData[1:nreg,5:dim(rawData)[2]]
      #Execute pre-processing function
      ffTypeData[[i]] <- ppFunc(numData)
      
      #Read Scenario data (cost and quantity mark-ups/factors)
      scenFilename <- paste0("Scenario data ",i,".xlsx")
      scenData <- as.data.frame(readxl::read_excel(scenFilename))
      scenData <- scenData[,which(!is.na(scenData[1,]))]
      
      #Some data files are associated with 2 scenario adjustment functions -- these must be handled differently (EHO and TAO)
      n_scenFuncs <- length(unique(scenData[,"R Scenario Function"]))
      #Loop over scenarios
      for (j in 1:length(scen)) {
        #Case 1: only 1 scenario adjustment function for the FF type
        if (n_scenFuncs==1) {
          #The Scenario processing func requires the full data frame and the scenario multiplier columns as arguments
          scenFunc <- as.character(unique(scenData[,"R Scenario Function"]))
          scenFunc <- source(file=paste0(scenFunc,".r"))[[1]]
          ffTypeScenData[[i]][[scen[j]]] <- scenFunc(ffTypeData[[i]],scenData[((j-1)*nreg+1):(j*nreg),which(grepl("Data",colnames(scenData)))])
        }else {
          #Case 2: 1 scenario adustment function for this scenario of the FF type
          if (length(unique(scenData[((j-1)*nreg+1):(j*nreg),"R Scenario Function"]))==1) {
            scenFunc <- as.character(scenData[1+(j-1)*nreg,"R Scenario Function"])
            scenFunc <- source(file=paste0(scenFunc,".r"))[[1]]
            ffTypeScenData[[i]][[scen[j]]] <- scenFunc(ffTypeData[[i]],scenData[((j-1)*nreg+1):(j*nreg),which(grepl("Data",colnames(scenData)))])
            #Case 3: Different scenario adjustment functions across regions within the scenario of the FF type 
          }else {
            ffTypeScenData[[i]][[scen[j]]] <- array(NA,dim=dim(ffTypeData[[i]]))
            #Read and use the scenario function for each region
            for (k in 1:nreg) {
              scenFunc <- as.character(scenData[k+(j-1)*nreg,"R Scenario Function"])
              scenFunc <- source(file=paste0(scenFunc,".r"))[[1]]
              ffTypeScenData[[i]][[scen[j]]][k,,] <- scenFunc(array(ffTypeData[[i]][k,,],dim=c(1,dim(ffTypeData[[i]])[2],2)),
                                                              scenData[k+(j-1)*nreg,which(grepl("Data",colnames(scenData)))])
            }
          }
        }
      }
    }
    for (ii in names(ffTypeScenData)) {
      for (jj in scen) {
        grades <- dim(ffTypeScenData[[ii]][[jj]])[2]
        tmp <- mbind(tmp,new.magpie(regions,NULL,paste(rep(c("costs","qtys"),each=grades),ii,jj,paste0("\"",as.character(1:grades),"\""),sep="."),
                                    ffTypeScenData[[ii]][[jj]]))
      }
    }
    getSets(tmp) <- c("region","year","xi","type","scen","grade")
    tmp <- toolNAreplace(tmp,replaceby=0)[[1]]
    
    
    #Conversion to REMIND-readable data
    
    #Store regions, FF types, data type (xi), and scenarios from input data
    regions <- getRegions(tmp)
    types <- getNames(tmp,fulldim=TRUE,"type")
    scens <- getNames(tmp,fulldim=TRUE,"scen")
    #Move SSP2 to the front - important for loop below
    scens <- c("SSP2",scens[which(scens!="SSP2")])
    sets <- c("region","year","type","scen","xi","grade")
    xis <- c(paste0('xi',1:3),"dec")
    
    # Cost grades taken from expert judgment of production cost curves by JH & NB (FFECCM) in 2012
    costGrades = list(
      SSP1 = list(    # US$(2005)/TWa
        "oil_mea"  = c(0.054909051, 0.093408501, 0.204015527, 0.747400879, 1.390996080, 2.076113553, 2.480955695, 3.170979198),
        "oil_row"  = c(0.109818102, 0.186817002, 0.276754241, 0.415222711, 0.747400879, 1.141862454, 2.480955695, 3.170979198),
        "gas_mea"  = c(0.022089848, 0.060589298, 0.080943515, 0.236355064, 0.286536888, 0.863137859, 3.170979198),
        "gas_row"  = c(0.044179696, 0.121178596, 0.157626845, 0.236355064, 0.350155651, 0.863137859, 3.170979198),
        "coal"     = c(0.0158548960, 0.0761035008, 0.0919583968, 0.1331811263, 0.1997716895, 0.2980720446, 0.9512937595)
      ),
      SSP2 = list(    # US$(2005)/TWa
        "oil_mea"  = c(0.048594510, 0.082666523, 0.165612325, 0.328856387, 0.493284580, 0.918057413, 1.637430759, 3.170979198),
        "oil_row"  = c(0.069185405, 0.117694711, 0.174355172, 0.257059564, 0.493284580, 0.918057413, 1.637430759, 3.170979198),
        "gas_mea"  = c(0.019549516, 0.036585522, 0.071635011, 0.201339499, 0.262616738, 0.779742897, 3.170979198),
        "gas_row"  = c(0.027833209, 0.076342515, 0.108871395, 0.198808634, 0.262616738, 0.779742897, 3.170979198),
        "coal"     = c(0.0158548960, 0.0761035008, 0.0919583968, 0.1331811263, 0.1997716895, 0.2980720446, 0.9512937595)
      ),
      SSP5 = list(    # US$(2005)/TWa
        "oil_mea"  = c(0.027454526, 0.046704250, 0.093566286, 0.304322191, 0.463490897, 0.909855311, 1.240477848, 3.170979198),
        "oil_row"  = c(0.027454526, 0.046704250, 0.102007763, 0.166089084, 0.332003705, 0.546535439, 1.240477848, 3.170979198),
        "gas_mea"  = c(0.011044924, 0.030294649, 0.040471758, 0.094670778, 0.175077825, 0.416974811, 3.170979198),
        "gas_row"  = c(0.011044924, 0.030294649, 0.043864127, 0.094670778, 0.175077825, 0.416974811, 3.170979198),
        "coal"     = c(0.0158548960, 0.0761035008, 0.0919583968, 0.1331811263, 0.1997716895, 0.2980720446, 0.9512937595)
      )  
    )
    
    if ("grades2poly" %in% subtype) {
      #More granular function for grades2poly parametrization
      for (ssp in names(costGrades)) {
        for (type in names(costGrades[[ssp]])) {
          costGrades[[ssp]][[type]] <- seq(min(costGrades[[ssp]][[type]]),max(costGrades[[ssp]][[type]]),length.out = 50)
        }
      }
    }

    # IEA decline rate data from WEO 2008/09
    if (subtype=="oil") {
      sp_IEADecRat <- new.magpie(c("MEA", "EUR", "USA", "JPN", "RUS", "LAM", "CHN", "IND", "OAS", "AFR", "ROW"),years=NULL,names=c("conv","unconv"),fill=0)
      sp_IEADecRat[,,"conv"] <- c(0.034,    0.119,    0.097,    0.126,    0.058, 0.066,    0.067,    0.067,    0.067,    0.068,    0.067)
      sp_IEADecRat[,,"unconv"] <- c(0.150,    0.150,    0.150,    0.150,    0.150, 0.150,    0.150,    0.150,    0.150,    0.150,    0.150)
    }else if (subtype=="gas") {
      sp_IEADecRat <- new.magpie(c("MEA", "EUR", "USA", "JPN", "RUS", "LAM", "CHN", "IND", "OAS", "AFR", "ROW"),years=NULL,names=c("conv","unconv"),fill=0)
      sp_IEADecRat[,,"conv"] <- c(0.041,    0.111,    0.111,    0.111,    0.041, 0.111,    0.082,    0.082,    0.082,    0.082,    0.111)
      sp_IEADecRat[,,"unconv"] <- c(0.150,    0.150,    0.150,    0.150,    0.150, 0.150,    0.150,    0.150,    0.150,    0.150,    0.150)
    }
    mappingREM11 <- toolGetMapping("regionmappingREMIND.csv","regional", where = "mappingfolder")
    mappingGEA <- toolGetMapping("regionmappingGEA2012.csv","regional", where = "mappingfolder")
    w <- read.csv(paste0(getConfig("sourcefolder"),"/BGR/",subtype,"_reserves.csv"),header=TRUE,sep=";")[,c("Land_Region","Reserves","Resources")]
    #Remove NAs
    w[is.na(w)] <- 0
    #Convert to magpie for use as a disaggregation weight, convert countries to ISO code and set missing countries to 0
    w <- as.magpie(w,spatial=1,temporal=0,datacol=2)
    getRegions(w) <- toolCountry2isocode(getRegions(w))
    w <- toolNAreplace(toolCountryFill(w,fill=0))[[1]]
    #Disaggregate the GEA data according to the BGR data on country-level oil/gas combined reserves + resources
    w <- dimSums(w,dim=3)
    sp_IEADecRat <- toolAggregate(sp_IEADecRat,mappingREM11,weight=NULL)
    sp_IEADecRat <- toolAggregate(sp_IEADecRat,mappingGEA,weight=w)
    #ordered_names <- list(SSP1=getNames(tmp[,,'qtys'][,,"SSP1"]),SSP2=getNames(tmp[,,'qtys'][,,"SSP2"]),SSP5=getNames(tmp[,,'qtys'][,,"SSP5"]))
    ordered_names <- list(list())
    cum_qtys <- list()
    for (scen in names(costGrades)) {
      for (r in regions) {
        tmp[r,,'qtys'][,,scen] <- tmp[r,,'qtys'][,,scen][order(tmp[r,,'costs'][,,scen])] * EJ_2_TWyr
        ordered_names[[scen]][[r]] <- getNames(tmp[r,,'qtys'][,,scen])[order(tmp[r,,'costs'][,,scen])]
        tmp[r,,'costs'][,,scen] <- tmp[r,,'costs'][,,scen][order(tmp[r,,'costs'][,,scen])] * EJ_2_TWyr
      }
      
      #For plotting cumulative production cost curve
      #cum_qtys[[scen]] <- cumsum(as.numeric(dimSums(tmp[,,'qtys'][,,scen],dim=1)))
    }
    
    #For plotting cumulative production cost curve
    #return(data.frame(qtys=cum_qtys$SSP2/EJ_2_TWyr,costs=as.numeric(tmp[r,,'costs'][,,"SSP2"])/EJ_2_TWyr,type=substr(ordered_names[["SSP2"]][[r]],6,8)))
    
    row <- paste0(subtype,'_row')
    mea <- paste0(subtype,'_mea')
    ngrades <- length(costGrades[['SSP2']][[row]])-1
    out <- new.magpie(regions,ttot,paste(paste0("pe",subtype),rep(names(costGrades),each=ngrades*length(xis)),rep(xis,each=ngrades),as.character(1:ngrades),sep="."),fill=0)
    conv <- new.magpie(regions,ttot,paste(rep(names(costGrades),each=ngrades),"dec",as.character(1:ngrades),sep="."),fill=0)
    unconv <- new.magpie(regions,ttot,paste(rep(names(costGrades),each=ngrades),"dec",as.character(1:ngrades),sep="."),fill=0)
    
    for (s in scens) {
      if (s %in% names(costGrades)) {
        #if (s=="SSP2") {
        t <- ttot
        #}else {
        #  t <- ttot[which(ttot>=t_cutoff)]
        #}
        for (r in regions) {
          i <- 1
          costs <- as.numeric(tmp[r,,s][,,'costs'])
          if (r=="MEE" || r=="FSU") {
            grades <- costGrades[[s]][[mea]]
          }else {
            grades <- costGrades[[s]][[row]]
          }
          ngrades <- length(grades)-1
          for (g in 1:ngrades) {
            if (g==1) {
              grades[g] <- min(costs[1],grades[1])
            }else if (g==ngrades) {
              grades[g+1] <- max(costs[i],grades[g+1])
            }
            out[r,t,paste0(s,'.xi1')][,,g] <- grades[g]
            out[r,t,paste0(s,'.xi2')][,,g] <- grades[g+1]
            for (c in i:(length(costs))) {
              if (costs[c] <= grades[g+1] && costs[c] >= grades[g] && tmp[r,,'qtys'][,,s][,,c]>0) {
                out[r,t,paste0(s,'.xi3')][,,g] <- out[r,t,paste0(s,'.xi3')][,,g] + tmp[r,,'qtys'][,,s][,,c]
                #Distinguish between conventional and unconventional reservoirs in each cost grade to calculate decline rates
                if (grepl('CO',ordered_names[[s]][[r]][c])) {
                  conv[r,t,s][,,g] <- conv[r,t,s][,,g] + tmp[r,,'qtys'][,,s][,,c]
                }else {
                  unconv[r,t,s][,,g] <- unconv[r,t,s][,,g] + tmp[r,,'qtys'][,,s][,,c]
                }
              }else if (costs[c] > grades[g+1]) {
                i <- c
                break
              }
            }
          }
        }
        #out[,,s][,,'xi3'] <- conv[,,s] + unconv[,,s]
        #Introduce time dependence of grades: costs start at SSP2 levels in initial year and change linearly until the cutoff year
        if (s!="SSP2") {
          m1 <- (out[,t_cutoff,'xi1'][,,s] - out[,t0,'xi1'][,,'SSP2'])/t_trans
          m2 <- (out[,t_cutoff,'xi2'][,,s] - out[,t0,'xi2'][,,'SSP2'])/t_trans
          m3 <- (out[,t_cutoff,'xi3'][,,s] - out[,t0,'xi3'][,,'SSP2'])/t_trans
          out[,getYears(out)<paste0("y",t0),!"dec"%in%getNames(out)][,,s] <- out[,getYears(out)<paste0("y",t0),!"dec"%in%getNames(out)][,,"SSP2"]
          for (t1 in seq(t0,t_cutoff-ts1,ts1)) {
            out[,t1,'xi1'][,,s] <- out[,t1,'xi1'][,,'SSP2'] + m1*(t1-t0)
            out[,t1,'xi2'][,,s] <- out[,t1,'xi2'][,,'SSP2'] + m2*(t1-t0)
            out[,t1,'xi3'][,,s] <- out[,t1,'xi3'][,,'SSP2'] + m3*(t1-t0)
            #for (r1 in regions) {
            #  j <- 1
            #  for (g1 in 1:ngrades) {
            #    for (c1 in j:length(tmp[r1,,'costs'][,,s])) {
            #      if (tmp[r1,,'costs'][,,s][,,c1] >= out[r1,t1,'xi1'][,,s][,,g1] && tmp[r1,,'costs'][,,s][,,c1] <= out[r1,t1,'xi2'][,,s][,,g1]) {
            #        out[r1,t1,'xi3'][,,s][,,g1] <- out[r1,t1,'xi3'][,,s][,,g1] + tmp[r1,,'qtys'][,,s][,,c1]
            #      }else if (tmp[r1,,'costs'][,,s][,,c1] > out[r1,t1,'xi2'][,,s][,,g1]) {
            #        j <- c1
            #        break
            #      }
            #    }
            #  }
            #}
          }
        }
        #Calculate decline rates
        convRatio <- conv[,,s]/(unconv[,,s]+conv[,,s])
        convRatio[which(unconv[,,s]==0)] <- 1
        convRatio[which(conv[,,s]==0)] <- 0
        out[,,s][,,'dec'] <- (sp_IEADecRat[,,"conv"] * convRatio) + (sp_IEADecRat[,,"unconv"] * (1-convRatio))
        for (regi in regions) {
          out[regi,,paste0(s,".dec.1")][which(out[regi,,paste0(s,".dec.1")]==sp_IEADecRat[regi,,"unconv"])] <- 
            sp_IEADecRat[regi,,"conv"]
        }
      }
    }
  }
  return(out)
}
