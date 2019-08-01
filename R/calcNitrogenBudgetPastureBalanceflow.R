calcNitrogenBudgetPastureBalanceflow<-function(){
  out<-calcOutput("NitrogenBudgetPasture",deposition="Nsurplus2",aggregate = F)

  out<-collapseNames(toolHoldConstantBeyondEnd(out)[,,"balanceflow"])

  out<-convergence(origin = out,aim = 0,start_year = "y2010",end_year = "y2050",type = "s")
  
  return(list(
    x=out,
    weight=NULL,
    unit="Mt Nr",
    description="Balancelfow to account for unrealistically high NUEs on pastures"))
}



