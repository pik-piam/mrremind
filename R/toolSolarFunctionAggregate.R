#' toolSolarFunctionAggregate
#' 
#' Aggregate Solar data into regions
#'  
#' @param x magclass object that should be aggregated
#' @param rel relation matrix containing a region mapping.
#' A mapping object should contain 2 columns in which each element of x
#' is mapped to the category it should belong to after (dis-)aggregation
#' 
#' @return return: returns region aggregated solar data
#' 
#' @author Renato Rodrigues
#' @export
#' @importFrom magclass is.magpie as.data.frame
#'


toolSolarFunctionAggregate <- function(x, rel=NULL){
  
  # original bins
  bins <- structure(list(PV = c(21L, 62L, 104L, 146L, 187L, 229L, 271L, 
                                312L, 354L, 396L, 437L, 479L, 521L, 562L, 604L, 646L, 687L, 729L, 
                                771L, 812L, 854L, 895L, 937L, 979L, 1020L, 1062L, 1104L, 1145L, 
                                1187L, 1229L, 1270L, 1312L, 1354L, 1395L, 1437L, 1479L, 1520L, 
                                1562L, 1604L, 1645L, 1687L, 1728L, 1770L, 1812L, 1853L, 1895L, 
                                1937L, 1978L, 2020L, 2062L, 2103L, 2145L, 2187L, 2228L, 2270L, 
                                2312L, 2353L, 2395L, 2437L, 2478L, 2520L, 2561L, 2603L, 2645L, 
                                2686L, 2728L, 2770L, 2811L, 2853L, 2895L), 
                         CSP = c(69L, 207L, 
                                 345L, 483L, 621L, 759L, 897L, 1034L, 1172L, 1310L, 1448L, 1586L, 
                                 1724L, 1862L, 2000L, 2138L, 2276L, 2414L, 2552L, 2690L, 2827L, 
                                 2965L, 3103L, 3241L, 3379L, 3517L, 3655L, 3793L, 3931L, 4069L, 
                                 4207L, 4345L, 4483L, 4621L, 4758L, 4896L, 5034L, 5172L, 5310L, 
                                 5448L, 5586L, 5724L, 5862L, 6000L, 6138L, 6276L, 6414L, 6551L, 
                                 6689L, 6827L, 6965L, 7103L, 7241L, 7379L, 7517L, 7655L, 7793L, 
                                 7931L, 8069L, 8207L, 8344L, 8482L, 8620L, 8758L, 8896L, 9034L, 
                                 9172L, 9310L, 9448L, 9586L)), 
                    .Names = c("PV", "CSP"), class = "data.frame", row.names = c(NA, -70L))
  
  bins.pv <- as.character(bins$PV)
  bins.csp <- as.character(bins$CSP)
  
  ### aggregate to regions here
  x <- toolAggregate(x,rel)
  getSets(x)[1] <- "Region"
  
  ### share of area only usable by pv with formula: (area pv-area csp) / area pv (includes offset for countries with 0 values)
  area.pv <- dimSums(x[,,"area"][,,"PV"][,,c("0-50", "50-100")], dim=c(3.4,3.3)) # sum over bins and distance classes
  area.csp <- dimSums(x[,,"area"][,,"CSP"][,,c("0-50", "50-100")], dim=c(3.4, 3.3))
  
  # share of only pv usable area
  area.only.pv.share <- ((area.pv+1)-(area.csp+1)) / (area.pv+1)
  
  
  ### sum distance bins 1-50 and 50-100, create new distance class: 1-100red
  x1.pv <- x[,,"0-50"][,,"PV"][,,bins.pv]
  x1.csp <- x[,,"0-50"][,,"CSP"][,,bins.csp]
  
  x2.pv <- x[,,"50-100"][,,"PV"][,,bins.pv]
  x2.csp <- x[,,"50-100"][,,"CSP"][,,bins.csp]
  
  # equivalents of bins in the two distance classes
  offset.pv <- 4
  offset.csp <- 2
  
  bins.pv.d2 <- c(rep(head(bins.pv, 1), offset.pv), bins.pv[1:(length(bins.pv)-offset.pv)])
  bins.pv.agg <- cbind(bins.pv, bins.pv.d2)
  colnames(bins.pv.agg) <- c("d1", "d2")
  
  bins.csp.d2 <- c(rep(head(bins.csp, 1), offset.csp), bins.csp[1:(length(bins.csp)-offset.csp)])
  bins.csp.agg <- cbind(bins.csp, bins.csp.d2)
  colnames(bins.csp.agg) <- c("d1", "d2")
  
  x2.pv <- toolAggregate(x2.pv, rel=bins.pv.agg, dim=3.4)
  getSets(x2.pv) <- getSets(x1.pv)
  x2.csp <- toolAggregate(x2.csp, rel=bins.csp.agg, dim=3.4)
  getSets(x2.csp) <- getSets(x1.csp)
  
  missing.bins.pv <- fulldim(x1.pv)[[2]]$Bin[which(!(fulldim(x1.pv)[[2]]$Bin %in% fulldim(x2.pv)[[2]]$Bin))]
  missing.bins.csp <- fulldim(x1.csp)[[2]]$Bin[which(!(fulldim(x1.csp)[[2]]$Bin %in% fulldim(x2.csp)[[2]]$Bin))]
  
  
  x2.pv <- add_columns(x2.pv, missing.bins.pv, 3.4)
  x2.pv[,,missing.bins.pv] <- 0
  
  x2.csp <- add_columns(x2.csp, missing.bins.csp, 3.4)
  x2.csp[,,missing.bins.csp] <- 0
  
  
  # do summation
  x <- add_columns(x, "1-100red", 3.3)
  
  x[,,"PV"][,,"1-100red"] <- x1.pv[,,"0-50"]+ x2.pv[,,"50-100"]
  x[,,"CSP"][,,"1-100red"] <- x1.csp[,,"0-50"]+ x2.csp[,,"50-100"]
  
  
  # calculate energy in gigawatt hours using formula: capacity*bin/1000 
  
  x <- add_columns(x, addnm = c("Energy"), dim = 3.1)
  
  bins <- sapply(strsplit(dimnames(x[,,"capacity"])[[3]], ".", fixed=TRUE), function(x){as.numeric(tail(x, 1))})
  x[,,"Energy"] <- (x[,,"capacity"]*rep(bins, each= length(getRegions(x))))/1000
  
  
  
  ### split up pv in PVall PVcomp PVonly
  x <- add_columns(x, c("PVcomp", "PVonly"), 3.2)
  
  x[,,"PVonly"] <- x[,,"PV"]*area.only.pv.share[,,"PV"]
  x[,,"PVcomp"] <- x[,,"PV"]-(x[,,"PV"]*area.only.pv.share[,,"PV"])
  
  
  ### split up into fine bins
  splitfactor <- 10
  
  original_bins <- fulldim(x)[[2]]$Bin
  
  fine_bins <- sapply(original_bins, function(x) {paste0(x,"_", 1:10)})
  
  mlist <- lapply(seq_along(original_bins), function(i) {
    
    x_sub <- x[,,original_bins[i]][,,"area"][,,"1-100red"]
    x_sub <- add_columns(x_sub, fine_bins[,i], dim=3.4)
    
    for (j in seq_along(fine_bins[,i])) {
      
      x_sub[,,fine_bins[,i][j]] <- x_sub[,,original_bins[i]]*(1/splitfactor)
      
    }
    
    x_sub[,,fine_bins[,i]]
    
  })
  
  x_fine <- do.call(mbind, mlist)
  
  x_fine_csp <- x_fine[,,"CSP"]
  x_fine_pvcomp <- x_fine[,,"PVcomp"]
  
  ### find grade class borders
  borders <- c(25, 45, 60, 76, 86, 93.8, 97, 99.5, 99.9999) / 100
  
  
  # area totals
  total_csp <- dimSums(x[,,"area"][,,"1-100red"][,,"CSP"], 3.4)
  total_pvcomp <- dimSums(x[,,"area"][,,"1-100red"][,,"PVcomp"], 3.4)
  
  
  bins_f_csp <- fulldim(x_fine_csp)[[2]]$Bin
  bins_f_pvcomp <- fulldim(x_fine_pvcomp)[[2]]$Bin
  
  csp_mat <- matrix(NA, nrow=nrow(x_fine_csp), ncol=length(bins_f_csp))
  pvcomp_mat <- matrix(NA, nrow=nrow(x_fine_pvcomp), ncol=length(bins_f_pvcomp))
  
  
  # add exception for countries with 0 total area
  zero_csp <-  which(getRegions(total_csp) %in% where(total_csp== 0)$true$regions)
  zero_pvcomp <- which(getRegions(total_pvcomp) %in% where(total_pvcomp == 0)$true$regions)
  
  nonzero_csp <- setdiff(1:length(getRegions(total_csp)), zero_csp)
  nonzero_pvcomp <- setdiff(1:length(getRegions(total_pvcomp)), zero_pvcomp)
  
  for(i in seq_along(bins_f_csp)) {
    b <- bins_f_csp[1:i]
    csp_mat[nonzero_csp,i] <- as.matrix(dimSums(x_fine_csp[,,b], dim=3.4) / total_csp)[nonzero_csp,]
  }
  
  for(i in seq_along(bins_f_pvcomp)) {
    b <- bins_f_pvcomp[1:i]
    pvcomp_mat[nonzero_pvcomp,i] <- as.matrix(dimSums(x_fine_pvcomp[,,b], dim=3.4) / total_pvcomp)[nonzero_pvcomp,]
  }
  
  bins_positions_csp <- matrix(NA, nrow=nrow(x_fine_csp), ncol=length(borders))
  bins_positions_pvcomp <- matrix(NA, nrow=nrow(x_fine_pvcomp), ncol=length(borders))
  
  bins_positions_csp[nonzero_csp,] <- t(apply(csp_mat[nonzero_csp,], 1, function(y) {
    sapply(borders, function(x){ min(which(y-x>0))})
  }))
  
  bins_positions_pvcomp[nonzero_pvcomp,] <- t(apply(pvcomp_mat[nonzero_pvcomp,], 1, function(y) {
    sapply(borders, function(x){ min(which(y-x>0))})
  }))
  
  # grade borders 
  bins_grades_csp <- apply(bins_positions_csp, 2, function(x) bins_f_csp[x])
  bins_grades_pvcomp <- apply(bins_positions_pvcomp, 2, function(x) bins_f_pvcomp[x])
  
  rownames(bins_grades_csp) <- getRegions(x)
  rownames(bins_grades_pvcomp) <- getRegions(x)
  
  
  # split up in fine bins including all data 
  
  mlist <- lapply(seq_along(original_bins), function(i) {
    
    x_sub <- x[,,original_bins[i]]
    x_sub <- add_columns(x_sub, fine_bins[,i], dim=3.4)
    
    for (j in seq_along(fine_bins[,i])) {
      
      x_sub[,,fine_bins[,i][j]] <- x_sub[,,original_bins[i]]*(1/splitfactor)
      
    }
    
    x_sub[,,fine_bins[,i]]
    
  })
  
  x_fine <- do.call(mbind, mlist)
  
  
  # loop through regions
  
  regions <- getRegions(x)
  regionlist <- vector("list", length(regions))
  
  
  for (i in 1:length(regions)) {
    
    r <- regions[i]
    
    # create relation matrix for toolAggregate
    rel.mat.csp <- data.frame(bin=bins_f_csp, grade=NA)
    b <- sapply(bins_grades_csp[r,], function(x){which(bins_f_csp %in% x)})
    dup <- which(duplicated(b))
    b[dup-1] <- b[dup-1]-seq_along(dup)
    b <- sort(b)
    times <- diff(c(0,b))
    rel.mat.csp$grade[1:max(b)] <- Reduce(rep,list(9:1, times))
    
    
    rel.mat.pvcomp <- data.frame(bin=bins_f_pvcomp, grade=NA)
    b <- sapply(bins_grades_pvcomp[r,], function(x){which(bins_f_pvcomp %in% x)})
    dup <- which(duplicated(b))
    b[dup-1] <- b[dup-1]-seq_along(dup)
    b <- sort(b)
    times <- diff(c(0,b))
    rel.mat.pvcomp$grade[1:max(b)] <- Reduce(rep,list(9:1, times))
    
    rel <- rbind(rel.mat.csp, rel.mat.pvcomp)
    
    # aggregate region-wise to grades
    m <- toolAggregate(x_fine[r,,], rel = rel, dim=3.4)
    
    # calculate average bin value
    
    m <- add_columns(m, dim=3.1, addnm = "binval")
    m[,,"binval"] <- (m[,,"Energy"]*1000) / m[,,"capacity"]
    m[is.nan(m)]  <- 0
    # convert to yearly values
    m[,,"binval"] <- m[,,"binval"] /8760  
    # add to list of regions
    regionlist[[i]] <- m
  }
  
  # mbind aggregated regions
  out <- do.call(mbind, regionlist)
  
  # convert GWh into EJ
  out[,,"Energy"] <- out[,,"Energy"] * 0.0000036
  
  # subset / format output object
  out <- out[,,as.character(1:9)]
  out <- out[,,"1-100red"]
  out <- out[,,c("PV", "CSP")]
  getYears(out) <- NULL
  out <- suppressWarnings(collapseNames(out)) 
  getSets(out) <- c("region", "year", "type",  "technology" , "rlf")
  
  # calculate luse
  out <- add_columns(out, addnm = c("luse"), dim = 3.1)
  out[,,"luse"] <- dimSums(out, dim = 3.3)[,,"capacity"][,,"PV"]/ dimSums(out, dim = 3.3)[,,"area"][,,"PV"]
  
  # remove capacity
  out <- out[,,"capacity",invert=TRUE]
  
  # rename types of the data
  dimnames(out)[[3]] <- gsub(pattern = "area", replacement = "limitGeopot", dimnames(out)[[3]])
  dimnames(out)[[3]] <- gsub(pattern = "binval", replacement = "nur", dimnames(out)[[3]])
  dimnames(out)[[3]] <- gsub(pattern = "Energy", replacement = "maxprod", dimnames(out)[[3]])
  dimnames(out)[[3]] <- gsub(pattern = "PV", replacement = "spv", dimnames(out)[[3]])
  dimnames(out)[[3]] <- gsub(pattern = "CSP", replacement = "csp", dimnames(out)[[3]])
  
  return(out)
  
}
