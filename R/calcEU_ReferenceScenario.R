calcEU_ReferenceScenario <- function() {
  
  EU_27 <- c("ALA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
             "FRO", "FIN", "FRA", "DEU", "GIB", "GRC", "GGY", "HUN", "IRL",
             "IMN", "ITA", "JEY", "LVA", "LTU", "LUX", "MLT", "NLD", "POL",
             "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")

  EU_28 <- c(EU_27, "GBR") 

  EU_ReferenceScenario_2016 <- readSource("EU_ReferenceScenario", subtype = "2016")
  EU_ReferenceScenarioEU <- EU_ReferenceScenario_2016[EU_28,,]
  EU_ReferenceScenarioEU[is.na(EU_ReferenceScenarioEU)] <- 0
  EU_ReferenceScenario_2016[EU_28, , ] <- EU_ReferenceScenarioEU[EU_28, , ]
  EU_ReferenceScenario_2016 <- add_dimension(EU_ReferenceScenario_2016, dim = 3.1, add = "model", nm = "EU_ReferenceScenario_2016")

  EU_ReferenceScenario_2020 <- readSource("EU_ReferenceScenario", subtype = "2020")
  EU_ReferenceScenarioEU <- EU_ReferenceScenario_2020[EU_27, , ]
  EU_ReferenceScenarioEU[is.na(EU_ReferenceScenarioEU)] <- 0
  EU_ReferenceScenario_2020[EU_27, , ] <- EU_ReferenceScenarioEU[EU_27, , ]
  EU_ReferenceScenario_2020 <- add_dimension(EU_ReferenceScenario_2020, dim = 3.1, add = "model", nm = "EU_ReferenceScenario_2020")
  EU_ReferenceScenario_2020 <- add_columns(EU_ReferenceScenario_2020, "y2000", dim = 2)
  
  x <- mbind(EU_ReferenceScenario_2016, EU_ReferenceScenario_2020)
  
  weights <- x
  weights[,,] <- NA
  weights[,,"Price|Secondary Energy|Electricity (US$2005/GJ)"] <- 1
  
  return(list(x = x, weight = weights, mixed_aggregation = T, unit = "Various", description = "Historical Data"))
}
