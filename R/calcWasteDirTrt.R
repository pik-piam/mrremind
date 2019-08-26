#' @title calcWasteDirTrt
#' @description Calculates shares of waste treatments by type based on Dirichlet regression on gdp using WhataWaste2.0 data
#' note that each type is independent - treatments for each type all sum to 1
#' @param weight population weights or "none" 
#' @author David Chen
#' @return magpie object of waste treatment by type shares


calcWasteDirTrt <- function(weight="pop"){

pred_pop <- function (object, newdata, mu = TRUE, alpha = FALSE, phi = FALSE) 
{
  if (missing(newdata)) 
    return(fitted(object, mu, alpha, phi))
  repar <- object$parametrization == "alternative"
  dims <- ncol(object$Y)
  model_formula <- object$mf_formula
  model_formula$formula <- as.Formula(deparse(model_formula$formula))
  model_formula$data <- as.name("newdata")
  model_formula$lhs <- 0
  model_formula$weights = NULL
  if (repar && (length(model_formula$formula)[2L] == 1L)) {
    model_formula$formula <- as.Formula(paste0(deparse(model_formula$formula), 
                                               " | 1"))
  }
  if (!repar && (length(model_formula$formula)[2L] == 1L)) {
    model_formula$formula <- as.Formula(paste0(deparse(model_formula$formula), 
                                               " | ", paste0(rep(deparse(model_formula$formula[[3]]), 
                                                                 dims - 1L), collapse = " | ")))
  }
  model_formula[["drop.unused.levels"]] <- FALSE
  mf <- eval(model_formula)
  if (!repar) {
    X <- lapply(seq_len(dims), function(i) {
      model.matrix(Formula(terms(model_formula$formula, 
                                 data = newdata, rhs = i)), mf)
    })
    Z <- NULL
  }
  else {
    X <- model.matrix(Formula(terms(model_formula$formula, 
                                    data = newdata, rhs = 1L)), mf)
    Z <- model.matrix(Formula(terms(model_formula$formula, 
                                    data = newdata, rhs = 2L)), mf)
  }
  cc <- coef(object)
  if (repar) {
    base <- object$base
    cc[[1L]] <- split(unlist(cc[[1L]]), factor(seq_len(dims))[rep(seq_len(dims)[-base], 
                                                                  each = ncol(X))])
    cc[[2L]] <- unlist(cc[[2L]])
    ETA <- matrix(0, nrow = nrow(newdata), ncol = dims)
    for (i in seq_len(dims)[-base]) {
      ETA[, i] <- X %*% cc[[1]][[i]]
    }
    MU <- exp(ETA)/rowSums(exp(ETA))
    PHI <- exp(Z %*% cc[[2L]])
    ALPHA <- MU * as.numeric(PHI)
  }
  else {
    ALPHA <- matrix(0, nrow = nrow(newdata), ncol = dims)
    for (i in seq_len(dims)) {
      ALPHA[, i] <- exp(X[[i]] %*% cc[[i]])
    }
    PHI <- rowSums(ALPHA)
    MU <- ALPHA/PHI
  }
  if (!any(mu || alpha || phi)) 
    stop("Either mu, alpha or phi has to be requested.")
  if (sum(mu + alpha + phi) == 1) {
    if (mu) 
      return(MU)
    if (alpha) 
      return(ALPHA)
    if (phi) 
      return(PHI)
  }
  else {
    res <- list()
    if (mu) 
      res[["mu"]] <- MU
    if (alpha) 
      res[["alpha"]] <- ALPHA
    if (phi) 
      res[["phi"]] <- PHI
    return(res)
  }
}

DirRegTrt <-function(type, weight="pop"){

x <- calcOutput("NlWasteDistrib", aggregate=F)
x <- dimOrder(x, perm=c(2,1))
#gdp
gdppc <- calcOutput("GDPpc",aggregate=F)
gdp<- time_interpolate(gdppc, interpolated_year= getYears(x) , extrapolation_type = "linear")
gdp <- as.data.frame(gdp[,,"SSP2"])
colnames(gdp) <- c( "cell", "region", "year", "data1", "gdp")
#pop
pop <- calcOutput("Population",aggregate=F)
pop<- time_interpolate(pop, interpolated_year= getYears(x) , extrapolation_type = "linear")
pop <- as.data.frame(pop[,,"pop_SSP2"])
colnames(pop) <- c( "cell", "region", "year", "data1", "pop")

prepDirReg <- function(input, remove, agg=TRUE){
#specify type and remove
  #sum up wood and paper, and rubber and other
  if(agg==TRUE) {
    x[,,"other"] <- x[,,"rubber"] + x[,,"wood"] + x[,,"other"]
  }
  
  a<-as.data.frame(x[,,input]) %>% 
    select(-1) %>% 
    na.omit() 
  
  #some optim values (have small negative values solved, round and then  make the others 0 for now,,get closed anwyays)
  a[,5] <- round(a[,5],4)
  negs <- which(a[,5] <0) 
  a[negs,5] <- 0
  a <- spread(a, key=Data2, value=Value) %>% 
    select(-c(3, remove)) %>% 
    unite(col="reg_year", c(Region, Year))
  
  a <- unite(gdp,col="reg_year", c(region, year)) %>% 
    select(-c(cell,data1)) %>% 
    inner_join(a, ., by="reg_year") 
  
  a <- unite(pop,col="reg_year", c(region, year)) %>% 
    select(-c(cell,data1)) %>% 
    inner_join(a, ., by="reg_year") %>% 
    filter(gdp < 90000)
  
WD1 <- DR_data(a[,2:(ncol(a)-2)])
WD <- WD1[-which(is.na(rowSums(WD1))),]
WD <- DR_data(WD)
a <- a[-which(is.na(rowSums(WD1))),]
return(list(a,WD))
}

organic <- prepDirReg(input="organic", remove="recyc", agg=TRUE)
glass <- prepDirReg(input="glass", remove=c("compost", "incineration"))
metal<- prepDirReg(input="metal", remove=c("compost", "incineration"))
other<- prepDirReg(input="other", remove=c("compost"), agg=TRUE)
paper<- prepDirReg(input="paper", remove=c("compost"))
plastic<- prepDirReg(input="plastic", remove=c("compost"), agg=TRUE)
rubber<- prepDirReg(input="rubber", remove=c("compost"))
wood<- prepDirReg(input="wood", remove=NULL)
#paper with wood and Other with Rubber
paperW <-prepDirReg(input="paper", remove=NULL, agg=TRUE)
otherR <- prepDirReg(input="paper", remove="compost", agg=TRUE)

results_list <- c(organic,glass,metal,other,paper,plastic,rubber,wood, paperW, otherR)
names <- c("organic","glass","metal","other","paper","plastic","rubber","wood", "paperW", "otherR")

WD1 <- as.list(results_list[seq(from=2,to=20,by=2)] )
names(WD1) <- names
a1 <- as.list(results_list[seq(from=1,to=20,by=2)] )
names(a1) <- names
# 
# plot(WD)
# plot(WD, cex=.5,
#      a2d=list(colored=FALSE,
#               c.grid=FALSE), dims=1:3)
WD <<- WD1[[type]]
a <<- a1[[type]]

if(weight=="pop"){
  reg <- DirichReg(WD ~ gdp, data=a, weights=a$pop, verbosity=4)
  Xnew <- data.frame(gdp = seq(0,200000, by=1000))
  pred <- pred_pop(reg, newdata = Xnew)
  pred <- cbind(pred, Xnew)
}
 if (weight=="none"){
  reg <- DirichReg(WD~gdp, a, verbosity=4)
  Xnew <- data.frame(gdp = seq(0,200000, by=1000))
  pred <- predict(reg, newdata=Xnew)
  pred <- cbind(pred,Xnew)
}
# par(mfrow=c(2,2), oma=c(1,1,2,1))
# loop.vector <- c(1:ncol(WD))
# for (i in loop.vector){
#   plot(rep(a$gdp, 1), as.numeric(WD[,i]), pch = 16, 
#        xlab = "gdp", ylab = "share",
#        ylim=c(0, 1.1),
#        xlim=c(0,200000),
#        main= colnames(WD)[i])
#    text(a$gdp[order(a$pop)][110:length(a$gdp)], WD[,i][order(a$pop)][110:length(WD[,i])], 
#         labels=a$reg_year[order(a$pop)][110:length(a$gdp)], cex= 0.7, pos=3)
#   # lines(cbind(pred[,(ncol(pred))], pred[,i]), col = "#BEAED4")
#    lines(Xnew$gdp, pred[,i], col= "#BEAED4")
# }
return(reg)
}

gdppc <- calcOutput("GDPpc", aggregate=F)
gdppc <- as.data.frame(gdppc)

types <- c("organic", "paper","plastic","glass","metal","other")
if(weight=="pop"){
tmp <- lapply(types, DirRegTrt, weight="pop")
tmp2 <- lapply(tmp, pred_pop, newdata=data.frame(gdp=gdppc$Value))
}
if(weight=="none"){
  tmp <- lapply(types, DirRegTrt, weight="none")
  tmp2 <- lapply(tmp, predict, newdata=data.frame(gdp=gdppc$Value))
}

names(tmp2) <- types
colnames(tmp2[[1]]) <- c("compost","incineration","landfills","dumps")
colnames(tmp2[[2]]) <- c("recyc","incineration","landfills","dumps")
colnames(tmp2[[3]]) <- c("recyc","incineration","landfills","dumps")
colnames(tmp2[[4]]) <- c("recyc","landfills","dumps")
colnames(tmp2[[5]]) <- c("recyc","landfills","dumps")
colnames(tmp2[[6]]) <- c("recyc","incineration", "landfills","dumps")

tmp2 <- as.data.frame(tmp2)
#set impossible combinations to 0
zeros <- c("organic.recyc", "paper.compost","plastic.compost", "metal.compost","metal.incineration",
           "glass.compost","glass.incineration","other.compost" )
tmp2[zeros] <-0

df <- cbind(gdppc, tmp2)
df <- df[,-c(1,5)]
colnames(df)[3] <- "scenario"
df <- gather(df, key="trt",value = "value", organic.compost:other.compost)

x <- as.magpie(df)

return(list(
  x=x,
  weight=NULL,
  unit="share",
  description="Share of waste treatments by types"))
}

