################## Process Raw Workstation File #################

## input Requirements: 
### R data object of the CSV file from discovery workstation (to account for some expectation of preprocessing of data)
### Mapping of the names to the workstation


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
  


    
