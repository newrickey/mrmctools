## apply flow chart

applyflowchart <- function(rawdata, PTcodes, PTCthreshold,includeA=T,includeC=T){
  
  
  ## Create an indicator variable to test if the reference code is in the primary task codes (defined in the preamble)
  rawdata$RefPTLesion <- ifelse(rawdata$RefCode %in% PTcodes, 1,0)
  rawdata$ObsPTLesion <- ifelse(rawdata$ObserverCode %in% PTcodes, 1,0)
  rawdata$PTCthreshold <- PTCthreshold
  rawdata$OverPTCThresdhold <- ifelse(rawdata$ObserverPTC >= PTCthreshold, 1, 0)
  rawdata$includeA <- includeA
  rawdata$includeC <- includeC
  
## split data in half for processing
  
  nomarks <- dplyr::filter(rawdata, is.na(rawdata$ObserverROIID))
  marks <- dplyr::filter(rawdata, !is.na(rawdata$ObserverROIID))
  
  ## Using the flowchart for lesion classification, there are 6 dispositions for 
  #   reader ROIs (denoted as A - F)
  
  
  
  # Case A: Reader mark does not match reference, reader does not call it a PT lesion
  marks$LesionClassA <- ifelse(is.na(marks$RefID) & marks$ObsPTLesion == 0 , 1, 0)
  
  # Case B: Reader mark does not match reference, reader calls it a PT lesion
  marks$LesionClassB <- ifelse(is.na(marks$RefID) & marks$ObsPTLesion == 1, 1, 0)
  
  # Case C: Reader mark matches reference, reader calls it not a PT lesion and the reference is not PT lesion
  marks$LesionClassC <- ifelse(!is.na(marks$RefID) & marks$RefPTLesion == 0 & marks$ObsPTLesion == 0, 1, 0)
  
  # Case D: Reader mark matches reference, reader calls it not a PT lesion and the reference is PT lesion
  marks$LesionClassD <- ifelse(!is.na(marks$RefID) & marks$RefPTLesion == 1 & marks$ObsPTLesion == 0, 1, 0)
  
  # Case E: Reader mark matches reference, reader calls it a PT lesion and the reference is not PT lesion
  marks$LesionClassE <- ifelse(!is.na(marks$RefID) & marks$RefPTLesion == 0 & marks$ObsPTLesion == 1, 1, 0)
  
  # Case F: Reader mark matches reference, reader calls it a PT lesion and the reference is not PT lesion
  marks$LesionClassF <- ifelse(!is.na(marks$RefID) & marks$RefPTLesion == 1 & marks$ObsPTLesion == 1, 1, 0)
  
  
  newdata <- dplyr::bind_rows(marks, nomarks)
  
  
  return(newdata)
}