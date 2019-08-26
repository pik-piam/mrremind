#' @title calcWasteDirType
#' @description Calculates shares of waste types based on Dirichlet regression on gdp using WhataWaste2.0 data
#' @param weight population weights or other weights or NULL
#' @param WRagg aggregates Wood and Rubber waste small no-trend quantities to "other"
#' @author David Chen
#' @return magpie object of waste shares



calcWasteDirType <- function(weight="pop", WRagg=TRUE){
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

tmp <- readSource("Waste", "Composition")

gdppc <- calcOutput("GDPpc",aggregate=F)
gdp<- time_interpolate(gdppc, interpolated_year= getYears(tmp) , extrapolation_type = "linear")
gdp <- as.data.frame(gdp[,,"SSP2"])
colnames(gdp) <- c( "cell", "region", "year", "data1", "gdp")

pop <- calcOutput("Population",aggregate=F)
pop<- time_interpolate(pop, interpolated_year= getYears(tmp) , extrapolation_type = "linear")
pop <- as.data.frame(pop[,,"pop_SSP2"])
colnames(pop) <- c( "cell", "region", "year", "data1", "pop")

#closure
tmp <- tmp/dimSums(tmp, na.rm=T, dim=3)
#years where data exists
years <- as.data.frame(where(dimSums(tmp, dim=3, na.rm=T)==0)$false$`individual`)
years$year <- gsub("y", x=years$year, replacement="")
tmp <- as.data.frame(tmp)
colnames(tmp) <-  c("cell","region","year","type","value") 
colnames(years)[1] <- "region"

#only the years that have value
tmp <- merge(tmp, years[c("region", "year")])
tmp[which(is.na(tmp$value)),"value"] <- 0
tmp<- select(tmp, -cell)

tmp <- unite(tmp, reg_year, c("region","year")) %>% 
        spread(key=type, value=value)

df <- unite(gdp,col="reg_year", c(region, year)) %>% 
  select(-c(cell,data1)) %>% 
  inner_join(tmp, ., by="reg_year") 
# %>% 
  # filter(.,gdp<100000)

pop <- unite(pop,col="reg_year", c(region, year)) %>% 
  select(-c(cell,data1))
df<- inner_join(df, pop, by="reg_year") %>% 
      filter(gdp<100000)
     
if (WRagg==TRUE){
 df[,"other"] <- df[,"rubber_leather"] + df[,"wood_waste"]  + df[,"other"]
 WD <- DR_data(df[,2:7])
}
else if (WRagg==FALSE){
 WD <- DR_data(df[,2:9])
}
Xnew <- data.frame(gdp = seq(0,200000, by=1000))
 
if (weight=="other"){
    #or with making other the weight
    df <- subset(df, select=c(1:4,6:ncol(df),other))
    df$other <- -1*(df$other-1)
    WD <- DR_data(df[,2:8])
    reg_pop <- DirichReg(WD ~ gdp, df, weights=other, verbosity=4)
    pred <- pred_pop(reg_pop, newdata = Xnew)
    pred <- cbind(pred, Xnew)
}

if (weight== "pop") {
   reg <- DirichReg(WD ~ gdp, df, weights=pop, verbosity=4)
   pred <- pred_pop(reg, newdata = Xnew)
   pred <- cbind(pred, Xnew)
 }
else if (weight == "none") {
  reg <- DirichReg(WD~gdp, df, verbosity=4)
   pred <- predict(reg, newdata=Xnew)
   pred <- cbind(pred, Xnew)
 }

# PLOTTING BLOCK not for moinput build
#  if(WRagg==TRUE){
# par(mfrow=c(2,3))
#  }
#  else if (WRagg==FALSE){
#    par(mfrow=c(3,3))
#  }
#  
# loop.vector <- c(1:ncol(WD))
# par(oma=c(1,1,2,1))
# for (i in loop.vector){
# plot(rep(df$gdp, 1), as.numeric(WD[,i]), pch = 16, 
#               xlab = "gdp", ylab = "Proportion",
#      ylim=c(0, 1.1),
#      xlim=c(0,200000),
#      main= colnames(WD)[i])
#    text(df$gdp[order(df$pop)][110:length(df$gdp)], WD[,i][order(df$pop)][110:length(WD[,i])],
#         labels=df$reg_year[order(df$pop)][110:length(df$gdp)], cex= 0.7, pos=3)
#   lines(Xnew$gdp, pred[,i], col= "#BEAED4", lwd=2)
# }
# title("Type POPweight", outer = TRUE, cex = 1.5)

gdppc <- as.data.frame(gdppc)

tmp <- pred_pop(reg, newdata=data.frame(gdp=gdppc$Value))
colnames(tmp) <- c("organic","glass","metal", "other", "paper", "plastic")
df <- cbind(gdppc, tmp)
df <- df[,-c(1,5)]
colnames(df)[3] <- "scenario"
df <- gather(df, key="type",value = "value", organic:plastic)
x <- as.magpie(df)
x <- dimOrder(x, c(2,1))
# 
# #calibrate?? shares doesn't work like this
# real <- readSource("Waste", "Composition")
# real <- real/dimSums(real, na.rm=T, dim=3)
# real[,,"other"] <- real[,,"other"] + real[,,"rubber_leather"] + real[,,"wood_waste"]
# real <- real[,,-c(7,8)]
# getNames(real)[5] <- "paper"
# tmp <- time_interpolate(x,interpolated_year= getYears(real))
# c_factor <- dimSums(real/tmp[,,"SSP2"], dim=2, na.rm=T)
# #make no data ones 1
# c_factor[which(c_factor==0)] <- 1
# x <- x*collapseNames(setYears(c_factor,NULL))

return(list(
  x=x,
  weight=NULL,
  unit="share",
  description="Share of waste types"))

}
