#' Produce JAFROC analysis storage file.
#' 
#' This is a key function in the JAFROC analysis paradigm. This is the datasource for the external package RJafroc. It also holds the data in a normalize function to support many analyses for this package. 
#' 
#' @param flowchartdata This is a dataframe produced by the function applyflowchart. 
#' @param outputfile A text string for an excel file to be produced by the function. The file name can have the path and needs to end in .xlsx. 
#' @return This function does not have a return. It produces a saved output file for later use.
#' @examples 
#' makeJAFROCfile(flowchartdata=raw2,outputfile="testJAFROC.xlsx")


makeJAFROCfile <- function(flowchartdata, outputfile){
  
  ## remove rows where there is no observer mark
  observerdata <- dplyr::filter(flowchartdata, !is.na(ObserverROIID))
  tflowchart <- flowchartdata
  ## include only ROIs above the PTC threshold
  observerdata <- dplyr::filter(observerdata, OverPTCThresdhold == 1)

  #### set aside true positives
  tp_t1 <-  dplyr::filter(observerdata, observerdata$LesionClassD ==1 | observerdata$LesionClassF ==1)  
  tp_t1 <-  dplyr::rename(tp_t1, TP_Rating=ObserverPTC)
  tp_t2 <-  dplyr::select(tp_t1, ReaderID, ModalityID, CaseID, RefID,TP_Rating, ObsPTLesion)
  ## if a multiple reader ROIs match reference, average the confidence
  tp_t3 <-  tp_t2 %>%  dplyr::group_by(ReaderID, ModalityID, CaseID, RefID) %>%  
               dplyr::summarize(TP_Rating = mean(TP_Rating), ObsPTLesion = max(ObsPTLesion))
  
  
  TP <-  dplyr::arrange(tp_t3, ReaderID, ModalityID, CaseID, RefID)
  
  
  
  
  ##############################################################
  ### False Positives will be of two classes
  ### --program will also apply delta threshold and exclusion as defined in the include.R program
  
  ### Class 1 -- reader calls it a PT lesion (cases B & E) (no exclusion applies)
  fp_case1_t1 <-  dplyr::filter(observerdata, observerdata$LesionClassB ==1 | observerdata$LesionClassE ==1)
  fp_case1_t1 <-  dplyr::rename(fp_case1_t1, FP_Rating=ObserverPTC )
  fp_case1 <- dplyr::select(fp_case1_t1, ReaderID, ModalityID, CaseID, FP_Rating, ObsPTLesion)
  
  
  ### Case 2 -- reader does not call it a PT lesion (cases A & C)
  fp_case2_t1 <-  dplyr::filter(observerdata, observerdata$LesionClassA ==1 | observerdata$LesionClassC ==1)
  fp_case2_t1$bininclusion <- ifelse( (fp_case2_t1$includeA  & fp_case2_t1$LesionClassA==1) | (fp_case2_t1$includeC  & fp_case2_t1$LesionClassC==1),T,F)
  
  fp_case2_t1 <-  dplyr::rename(fp_case2_t1, FP_Rating=ObserverPTC )
  
  
  fp_case2_t1$keepers <- ifelse(fp_case2_t1$bininclusion, T,F)
  
  fp_case2_keepers <-  dplyr::filter(fp_case2_t1, keepers)
  
  fp_case2 <-  dplyr::select(fp_case2_keepers, ReaderID, ModalityID, CaseID, FP_Rating, ObsPTLesion)
  
  
  # fp_combined is for the jafroc analysis -- limited fields, turns into FP
  fp_combined <-  dplyr::bind_rows(fp_case1, fp_case2)
  FP <-  dplyr::arrange(fp_combined, ReaderID, ModalityID, CaseID)
  
  
  
  ########################################################################################
  #### Step 3: Establish the reference lesions
  
  ### Read in the listing of reference cases used during the randomization process
  
  
  reflist_t1 <- dplyr::filter(flowchartdata, RefDataset==T)  #select just the rows where the reference modality is used
  
  reflist <- dplyr::distinct(reflist_t1, CaseID)  # vector of unique case ids.
  
  raw_ref <-  dplyr::select(flowchartdata, RefID, RefDataID, RefPTC, RefCode, RefPTLesion)
  raw_ref <-  dplyr::filter(raw_ref, !is.na(RefID) & RefPTLesion ==1)
  raw_ref <-  dplyr::distinct(raw_ref, RefID, RefDataID)  #unique lesion IDs for all cases
  
  # count the number lesions per case (for the cases with lesions)
  ref_counts <- raw_ref %>%  dplyr::group_by(RefDataID) %>%  dplyr::summarize(numlesions = n())
  ref_counts$Weight <- 1/ref_counts$numlesions
  
  
  ## create a linking file to put the caseID on the reference lesions
  refcaseid <-  dplyr::distinct(dplyr::select(reflist_t1, datasetname, CaseID))
  refcaseid <-  dplyr::rename(refcaseid, RefDataID = datasetname)
  
  
  
  # merge the weights onto the reference lesions
  reference_lesions <-  dplyr::inner_join(raw_ref, ref_counts, by="RefDataID")
  reference_lesions <-  dplyr::rename(reference_lesions, LesionID =RefID)
  
  
  
  reference_lesions <-  dplyr::full_join(refcaseid, reference_lesions, by=c("RefDataID"))
  
  ### for the normal cases, assign LesionID = 0 and Weight = 0...a JAFROC software need
  reference_lesions$LesionID <- ifelse(is.na(reference_lesions$LesionID),0, reference_lesions$LesionID)
  reference_lesions$Weight <- ifelse(is.na(reference_lesions$Weight),0, reference_lesions$Weight)
  
  
  Truth <-  dplyr::select(reference_lesions, CaseID, LesionID, Weight)
  Truth <-  dplyr::arrange(Truth, CaseID,LesionID)
  
  ### make dataset with the keys to be exported 
  # this is a list of the de-identified case id and the original caseindex
  keys <- dplyr::distinct(tflowchart, CaseID, caseindex) 
  keys <- dplyr::arrange(keys, CaseID)
  
  refdata <- dplyr::filter(tflowchart, RefDataset==T)
  refdata_dataset <- dplyr::distinct(refdata, CaseID, RefDataID)
  
  keys <- dplyr::left_join(keys, refdata_dataset, by="CaseID")
  
  filelist <-c("Truth", "TP", "FP", "keys")
  WriteXLS::WriteXLS(filelist, ExcelFileName=outputfile)
}