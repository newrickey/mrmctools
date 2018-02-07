### prepare estimates

 prepareestimates <- function(JAFROCresults, refname){
  
  
  workingresults <- JAFROCresults
  
  
  ## Get the point estimates and CIs
  p1 <- workingresults$ciAvgRdrEachTrtRRRC
  p1$method <- "RRRC"
  
  p2 <- workingresults$ciAvgRdrEachTrtFRRC
  p2$method <- "FRRC"
  
  
  p3 <- workingresults$ciAvgRdrEachTrtRRFC
  p3$method <- "RRFC"
  
  
  pointestimates <- dplyr::bind_rows(p1,p2,p3)
  
  pointestimates$FOM_CI <- binCI(pointestimates$Area, pointestimates$`CI Lower`, pointestimates$`CI Upper`, the100=1,thedigits=3)
  pointestimates <- dplyr::select(pointestimates, method, Treatment, Area, `CI Lower`, `CI Upper`, FOM_CI)
  ## Get the differences in estimates
  
  d1 <- workingresults$ciDiffTrtRRRC
  d1$method <- "RRRC"
  
  d2 <- workingresults$ciDiffTrtFRRC
  d2$method <- "FRRC"
  
  d3 <- workingresults$ciDiffTrtRRFC
  d3$method <- "RRFC"
  
  deltaestimates <- dplyr::bind_rows(d1,d2,d3)
  
  treatments <- strsplit(deltaestimates$Treatment, " - ")
  treatment1 <- NULL
  treatment2 <- NULL
  
  
  for (i in 1:length(deltaestimates$Treatment)){
    treatment1[i] <-treatments[[i]][1]
    treatment2[i] <-treatments[[i]][2]
  }
  
  deltaestimates <- data.frame(deltaestimates, treatment1, treatment2)
  deltaestimates$treatment1 <- as.character(deltaestimates$treatment1)
  deltaestimates$treatment2 <- as.character(deltaestimates$treatment2)
  
  # now filter to have only the estimates for the reference dose included
  deltaestimates <- filter(deltaestimates, deltaestimates$treatment1 == refname | deltaestimates$treatment2==refname)
  
  
  
  deltaestimates$reversal <-ifelse(deltaestimates$treatment1 == refname, 1,0)  # refname is defined in the include file
  
  ## now create the reverse scoring as needed
  raw_cidiffs<- deltaestimates
  method <- raw_cidiffs$method
  comparison <- ifelse(raw_cidiffs$reversal == 0, raw_cidiffs$Treatment, paste0(raw_cidiffs$treatment2," - ", raw_cidiffs$treatment1))
  Treatment <- ifelse(raw_cidiffs$reversal == 0, raw_cidiffs$treatment1, raw_cidiffs$treatment2)
  estimate <- ifelse(raw_cidiffs$reversal == 0, raw_cidiffs$Estimate, -raw_cidiffs$Estimate)
  lowerci <-  ifelse(raw_cidiffs$reversal == 0, raw_cidiffs$CI.Lower, -raw_cidiffs$CI.Upper)
  upperci <-  ifelse(raw_cidiffs$reversal == 0, raw_cidiffs$CI.Upper, -raw_cidiffs$CI.Lower)
  
  cidiffs <- data.frame(method, comparison, Treatment, estimate, lowerci, upperci)  
  cidiffs$deltaFOM_CI <- binCI(cidiffs$estimate, cidiffs$lowerci, cidiffs$upperci, the100=1,thedigits=3)
  
  return(list(pointestimates, cidiffs))
  
}