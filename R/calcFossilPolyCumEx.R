calcFossilPolyCumEx <- function() {
  x <- readSource("REMIND_11Regi", subtype = "ffPolyCumEx")

  # Historically, Turkey is able to provide most of its coal demand locally.
  # However, the BGR numbers used as disaggregation weight for ROW numbers in
  # REMIND11regi are so low in comparison to Canada (which is also part of ROW)
  # that Turkey gets too little coal resources in REMIND and therefore needs to
  # import most of its coal consumption, leading to extreme coal prices in 2010-2020
  # when import amounts are bounded. As a hotfix, we increase the resources
  # available to Turkey by a factor 20.

  x["TUR", , "pecoal.max.medCoal"] <- x["TUR", , "pecoal.max.medCoal"] * 20
  x["TUR", , "pecoal.max.highCoal"] <- x["TUR", , "pecoal.max.highCoal"] * 20
  x["TUR", , "pecoal.max.lowCoal"] <- x["TUR", , "pecoal.max.lowCoal"] * 20

  return(list(
    x = x,
    weight = NULL,
    unit = "none",
    description = "vintages, installed capacities"
  ))
}
