### Determine Essential Lesions based on JAFROC data file and Min PTC confidence



essentiallesions <- function(JAFROCfilename,refname,essentiallesionfilename=NA){
  
  # The reader has to localize and correctly classify the nodule as being related to the 
  # primary task for it to be considered an essential lesion
  
  # to read in Truth, TP and FP sheets in the JAFROC file
  sheets <- readxl::excel_sheets(JAFROCfilename)
  
  for(i in unique(sheets)) {
    # print(paste0("Reading in ...", i))
    newtable <- readxl::read_excel(JAFROCfilename,sheet=i)
    #now save the table with a unique name
    assign(i, newtable)
    rm(newtable)
  }
  
  ## code reference sheets into character values--default import sometimes creates them as numeric
  Truth$CaseID <- as.character(Truth$CaseID)
  Truth$LesionID <- as.character(Truth$LesionID)

  TP$ReaderID <- as.character(TP$ReaderID)
  TP$ModalityID <- as.character(TP$ModalityID)
  TP$CaseID <- as.character(TP$CaseID)
  TP$RefID <- as.character(TP$RefID)
  
  FP$ReaderID <- as.character(FP$ReaderID)
  FP$ModalityID <- as.character(FP$ModalityID)
  FP$CaseID <- as.character(FP$CaseID)
  
  
  ## pull out some constants for later processing
  numreaders <- length(unique(TP$ReaderID))
  readerdf <- dplyr::distinct(TP, ReaderID)
  casedf <- dplyr::distinct(Truth, CaseID)  
  modalitydf <- dplyr::distinct(TP, ModalityID)  #note: requires at least 1 TP for each modality
  
  
  ## build list of true lesions
  truelesions <- dplyr::distinct(Truth, CaseID, LesionID)
  truelesions <- dplyr::filter(truelesions, LesionID != 0)
  truelesions$RefID <- as.character(truelesions$LesionID)
  
  
  casemodreaderdf <- expand.grid(CaseID = as.character(casedf$CaseID), ReaderID = as.character(readerdf$ReaderID), ModalityID = as.character(modalitydf$ModalityID), stringsAsFactors = F)
  
  
  ## make another grid with all lesions and modalities
  lesmodreddf <- expand.grid(RefID = as.character(truelesions$RefID), ReaderID = as.character(readerdf$ReaderID), ModalityID = as.character(modalitydf$ModalityID), stringsAsFactors = F)
  lesmodreddf <- dplyr::inner_join(lesmodreddf, truelesions, by=c("RefID"))
                                    
  tp_det <- dplyr::left_join(lesmodreddf, TP, by=c("ReaderID", "RefID", "ModalityID","CaseID"))
  tp_det$detection <- ifelse(!is.na(tp_det$TP_Rating), 1,0)
  tp_det$rating <-ifelse(is.na(tp_det$TP_Rating), 0, tp_det$TP_Rating)
  tp_det_count <- tp_det %>% dplyr::group_by(CaseID, RefID, ModalityID) %>% dplyr::summarize(meanrating=mean(rating),ndetection = sum(detection))
  detections <- tp_det_count
  detections$nreaders <- numreaders
  detections$pctdetect <- 100* detections$ndetection / detections$nreaders
  
  ## detections will be updated below to include the essential lesion flag
  
  ## filter the true positives based on correct classification
  pt_tp_rois <- dplyr::filter(TP, ObsPTLesion ==1)
  
  
  # now pull out the reference markings to work towards defining the essential lesions
  pt_tp_rois_ref <- dplyr::filter(pt_tp_rois, ModalityID == refname)
  pt_tp_rois_ref_count <- pt_tp_rois_ref %>% dplyr::group_by(RefID) %>% dplyr::summarize(ndetections = n())
  pt_tp_rois_ref_count$numreaders <- numreaders
  pt_tp_rois_ref_count$essentiallesion <- ifelse(pt_tp_rois_ref_count$ndetections / pt_tp_rois_ref_count$numreaders >= 0.5, 1,0)
  
  
  essentiallesions <- dplyr::filter(pt_tp_rois_ref_count, essentiallesion == 1)
  essentiallesionvector <- dplyr::distinct(essentiallesions, RefID)
  
  
  elmerge <- essentiallesionvector
  elmerge$essentiallesion <- 1
  detections <- dplyr::left_join(detections, elmerge, by="RefID")
  detections$essentiallesion <- ifelse(is.na(detections$essentiallesion), 0, 1)
  
  
  ## now merge back in the case IDs and count the number of essential lesions per case
  ptessential <- dplyr::inner_join(truelesions, essentiallesions, by="RefID")
  ptessential_count <- ptessential %>% dplyr::group_by(CaseID) %>% dplyr::summarize(nessential = n())
  ptessential_count <- dplyr::left_join( casedf, ptessential_count, by="CaseID")
  ptessential_count$nessential <- ifelse(is.na(ptessential_count$nessential), 0, ptessential_count$nessential)
  
  ptessential_count <- dplyr::inner_join(casemodreaderdf, ptessential_count, by="CaseID")
  #### the above data file has a case by modality array that has the number of essential lesions in it
  #### this datafile will be used for counting below
  
  
  
  ############################################################################
  ### Now, determine the number of correct detections by readers
  
  ## filter the TPs  based on the essential lesion vector
  workingtps <- dplyr::filter(TP, RefID %in% unique(essentiallesionvector$RefID))

  ## remove the reference reads from the listing  (commented out to allow the reference to be tabulated)
  ##workingtps <- dplyr::filter(workingtps, !(ModalityID == refname))
  
  ## remove the non-primary task ROIs
  workingtps <- dplyr::filter(workingtps, ObsPTLesion ==1)
  
  ## Now count the number of detections by reader
  workingdetections <- workingtps %>% dplyr::group_by(ReaderID, ModalityID, CaseID) %>% dplyr::summarize(ndetections = n())
  
  ## and now merge back the detections to complete the df
  essentialdf <- dplyr::left_join(ptessential_count, workingdetections, by=c("CaseID", "ReaderID", "ModalityID"))
  
  
  
  ### Create a file for the FPs
  ### Build list of FPs that were classified as the primary task
  ##workingfps <- dplyr::filter(FP, !(ModalityID == refname))  #commented out to allow the references to be tabulated, replaced with line below
  workingfps <- FP
  workingfps <- dplyr::filter(workingfps, ObsPTLesion ==1)
  fps_count <- workingfps %>% dplyr::group_by(CaseID, ReaderID, ModalityID) %>% dplyr::summarize(nFPs = n())
  ## merge the FPs to the df that contains the essential lesion counts (the expanded grid df)
  essentialdf <- dplyr::left_join(essentialdf, fps_count, by=c("CaseID", "ReaderID", "ModalityID"))
  
  
  
  ## change the nas to zeros
  essentialdf$nFPs <- ifelse(is.na(essentialdf$nFPs), 0, essentialdf$nFPs)
  essentialdf$ndetections <-ifelse(is.na(essentialdf$ndetections),0,essentialdf$ndetections)
  essentialdf$iscase <- ifelse(essentialdf$nessential >0,T,F)
  essentialdf$cond1 <-ifelse(essentialdf$ndetections == essentialdf$nessential, T,F)
  essentialdf$cond2 <-ifelse(essentialdf$nFPs == 0, T,F)
  essentialdf$correctread <- ifelse( (essentialdf$iscase  & essentialdf$cond1) | ( !essentialdf$iscase  & essentialdf$cond2), 1,0)
  
  # now group over readers
  essentialdf_count <- essentialdf %>% dplyr::group_by(CaseID, ModalityID, iscase) %>% dplyr::summarize(ncorrectreads = sum(correctread))
  essentialdf_count$numreaders <-numreaders
  essentialdf_count$majority <- ifelse(essentialdf_count$ncorrectreads/ essentialdf_count$numreaders >=0.5, 1,0)
 
  ## now compute the fraction of cases correctly read
  correctreads <- essentialdf_count %>% dplyr::group_by(ModalityID) %>% dplyr::summarize(ncases = n(), ncorrect = sum(majority) )
  correctreads$source <- "All"
  correctreads2 <- essentialdf_count %>% dplyr::group_by(ModalityID, iscase) %>% dplyr::summarize(ncases = n(), ncorrect = sum(majority) )
  correctreads2$source <- ifelse(correctreads2$iscase, "Cases with essential lesions", "No essential lesions")
  
  finaldf <- dplyr::bind_rows(correctreads, correctreads2)
  finaldf$pctcorrect <- finaldf$ncorrect / finaldf$ncases
  finaldf <- dplyr::select(finaldf, -iscase)
  finaldf <- dplyr::arrange(finaldf, ModalityID, source)
  
  
  if (!is.na(essentiallesionfilename)){
    ellist <- c("finaldf","detections", "essentiallesions")
    WriteXLS(ellist, ExcelFileName=essentiallesionfilename)
  }
  
  
  return( detections)
# return(  finaldf)
}


