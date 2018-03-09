#' Read and normalize reader workstation file.
#' 
#' This is the first function called when conducting a MRMC study using the Discovery Workstation computer system. 
#' The workstation has algorithm that match reader marks with reference ROIs. The output file is study-specific.
#' To help generalize the analysis and all of the functions that follow, this function renames the study-specific variables into 
#' standardized names.
#' 
#' The function requires the use of an anonymization file. This file links back the datasets (image files) back to doses and patient cases.
#' See the vignette for an example and elaboration of this file. 
#' 
#' @param workstationdata A dataframe with the raw Discovery Workstation export file
#' @param deidentificationdata A dataframe with columns for patient ID a column with dataset IDs for each modality in the study
#' @param refname Text string with the name of the reference modality
#' @param caseindex Text string with the name of the patient ID variable in the deidentification file
#' @param RefID Text string with the variable name for the reference marking unique identifier in the workstation file
#' @param RefDataID Text string with the variable name for the reference dose dataset ID in the workstation file
#' @param RefDetConf Text string with the variable name for the confidence score related to the reference detection (not primary task)
#' @param RefCode Text string with the varaible name for diagnosis code assigned by the reference reader for the the lesion
#' @param RefPTC Text string with the varaible name for the primary task confidence (PTC). 
#' @param datasetname Text string with the variable name for the reader's dataset name pertaining to the ROI being evaluated
#' @param ObserverPTC Text string with the variable name for the reader's assigned primary task confidence (PTC). This is the main confidence score used for the analysis.
#' @param ObserverCode Text string with the variable name for the reader's assigned diagnosis of the ROI.
#' @param ObserverROIID Text string for the variable name for the reader's unique ROI database ID.
#' @param ReaderFullID Text string with the username (typically) used to log into the study. This represents the starting point for the reader identification in the dataset.
#' @param anonymizereader A logical to replace the internal reader ID with an anonymized reader ID by default. Setting this to false will possibly identify the data in the analysis.
#' @param expandcases A logical to ensure that the joins that happen in the program include all combinations of readers and modalities. Generally this needs to remain as True.
#' @return A dataframe with standardized names
#' @import ggplot2
#' @import magrittr
#' @export


readworkstationfile <- function(workstationdata, deidentificationdata, refname, caseindex,
                            RefID, RefDataID, RefDetConf, RefCode, RefPTC, 
                            datasetname, ObserverPTC, ObserverCode, ObserverROIID, ReaderFullID,  anonymizereader=T, expandcases=F){

rawdata <- dplyr::rename_(workstationdata, 
                          RefID = RefID,
                          RefDataID = RefDataID,
                          RefDetConf = RefDetConf,
                          RefCode = RefCode,
                          RefPTC = RefPTC,
                          datasetname = datasetname,
                          ObserverPTC = ObserverPTC,
                          ObserverCode = ObserverCode,
                          ObserverROIID = ObserverROIID,
                          ReaderFullID = ReaderFullID)

rawdata <- dplyr::select(rawdata, RefID, RefDataID, RefDetConf, RefCode, RefPTC, datasetname, ObserverPTC, ObserverCode, ObserverROIID, ReaderFullID)
  
  
  ## force all lanids to be lower case (sometimes there is mixed case which R will treat
  #    as different readers)
  rawdata$ReaderFullID <- tolower(rawdata$ReaderFullID)
  
  
  ## Block of code to anonymize the reader ID. If true, assign number. If F, use the FullID that was avaialble on import
  if (anonymizereader){
    #pull out unique IDs
    ReaderFullID <- as.character( unique(rawdata$ReaderFullID))  #create a vector for the reader names
    set.seed(23432)  #set a seed to randomly sort the readers
    rannum <- runif(length(ReaderFullID))  # the sorting number
    readers <- data.frame(ReaderFullID, rannum)
    readers <- dplyr::arrange(readers, rannum)
    ReaderID <- seq(1:length(ReaderFullID))
    
    readerkey <- data.frame(ReaderID, readers)
    readerkey$ReaderFullID <- as.character(readerkey$ReaderFullID)
    # merge ReaderID back onto raw data file
    
    rawdata <- dplyr::inner_join(rawdata, readerkey, by="ReaderFullID")
    rawdata <- dplyr::select(rawdata, -rannum, -ReaderFullID)
   } 
  else {
    rawdata$ReaderID <- as.character(rawdata$ReaderFullID)
    rawdata <- dplyr::select(rawdata, -ReaderFullID)
  } 
  
## Do some clean up of NULL values that don't translate well from SQL
  
  ## convert some character values into numerics. Convert NULL into NA (missing)
  ### note: some fields have NULL and others have null
  rawdata$RefID <- ifelse(toupper(rawdata$RefID) %in% c("NULL"), NA, rawdata$RefID)
  rawdata$RefDataID <- ifelse(toupper(rawdata$RefDataID) %in% c("NULL"), NA, rawdata$RefDataID)
  rawdata$RefPTC <- ifelse(toupper(rawdata$RefPTC) %in% c("NULL"), NA, rawdata$RefPTC)
  rawdata$RefPTC <- as.numeric(rawdata$RefPTC)
  rawdata$RefCode <- ifelse(toupper(rawdata$RefCode) %in% c("NULL"), NA, rawdata$RefCode)
  
  rawdata$ObserverPTC <- ifelse(toupper(rawdata$ObserverPTC) %in% c("NULL"), NA, rawdata$ObserverPTC)
  rawdata$ObserverPTC <- as.numeric(rawdata$ObserverPTC)
  rawdata$ObserverROIID <-ifelse(toupper(rawdata$ObserverROIID) %in% c("NULL"), NA, rawdata$ObserverROIID)
  rawdata$ObserverCode <-ifelse(toupper(rawdata$ObserverCode) %in% c("NULL"), NA, rawdata$ObserverCode)
  
  
  ## now pull out some information on the de-identification data
  
  deidentify_t1 <- dplyr::rename_(deidentificationdata, caseindex =caseindex)
  
  caseindex <- deidentify_t1$caseindex
  CaseID <- seq(1:length(caseindex))
  
  casekey <- data.frame(CaseID, caseindex)
  
  
  #flip the data to tall format
  deidentify_tall <- tidyr::gather(deidentify_t1, Modality, datasetname, -caseindex)
  
  deidentify_tall <- dplyr::inner_join(casekey, deidentify_tall, by="caseindex")
  
  
  #determine unique modality names
  Modality <-unique(deidentify_tall$Modality)
  ModalityID <-unique(deidentify_tall$Modality)
  
  
  Modalitydf <- data.frame(ModalityID, Modality)
  Modalitydf$ModalityID <- as.character(Modalitydf$ModalityID)
  Modalitydf$Modality <- as.character(Modalitydf$Modality)
  
  
  #set up merge file with reference numbers and dataset IDs
  modalitykey <- dplyr::inner_join(deidentify_tall, Modalitydf, by="Modality")
  
 
  
  rawdata <- dplyr::inner_join(rawdata, modalitykey, by=c("datasetname"))
 
  
  
  
  if (expandcases){
    dummyROIs <- dplyr::filter(deidentify_tall, Modality == refname)
    
    dummyROIs$RefID <- "9999"
    dummyROIs$RefDataID <- dummyROIs$datasetname
    dummyROIs$ModalityID <- dummyROIs$datasetname
    dummyROIs$RefCode <- "-99"
    dummyROIs$RefPTC <- 100
    dummyROIs$RefDetConf <- "dummy code"
    rawdata <- dplyr::bind_rows(rawdata, dummyROIs)
  } 
  
  rawdata$RefDataset <- ifelse(rawdata$Modality == refname, T, F)
#return(deidentify_tall)  
 return(rawdata)
} # end of function
  


    
