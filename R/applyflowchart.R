#' Annotate and label each reader mark based on a decision tree
#' 
#' This function normalizes the readerworkstation file (the data that is passed to this function) and prepares it for further analysis.  
#' The coding used to designate the primary task ROIs is a requried parameter. 
#' The includeA and includeC parameters define some special predetermined exclusions from the function. See the vignette for further elaboration of these parameters.
#' 
#' @param rawdata The output datafile from the function readworkstation file.
#' @param PTcodes An R set object that contains the list of database ID codes that are included in the primary task.  If a reader mark is in this set, the ObsPTLesion variable in the output dataset will be coded as a 1. Else, it will be coded as 0. The same logic is applied to the classification of reference lesions and the outputted variable name is RefPTLesion.
#' @param PTCthreshold A numeric value used to determine the lower limit for any primary task confidence score to be considered in the analysis. The limit is applied as a greater than equal to. So specification of 1 will include all reader ROIs with a score greater than or equal to 1.
#' @param includeA A parameter, defaulted to True, that allows one to exclude type A reader marks. These are reader marks where they do not match any lesions in the reference set and the reader classifies them as a non-primary task lesion. Given the reader's confidence could be non-negigble, under most circumstations, these reader marks should be retained. Setting this to false will exclude these markings from all subsequent analyses. 
#' @param includeC A parameter, defaulted to True, that allows one to exclude type C reader marks. These are reader marks where both the reader and reference localize the same non-primary task lesion.  Given the reader's confidence could be non-negigble, under most circumstations, these reader marks should be retained. Setting this to false will exclude these markings from all subsequent analyses.
#' @return A new dataframe that appends several helper columns onto the input dataset. 
#' @examples
#' applyflowchart(raw1, PTcodes=c("91"), PTCthreshold = 1, includeA=T,includeC=T)


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