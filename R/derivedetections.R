#' Tabulate reader detections.
#' 
#' This function tabulates the number of true positives dections for each lesion at each modality.
#' The function reads the standard excel file that contains data to support JAFROC analyses. 
#' 
#' @param JAFROCfilename The is the excel file produced by the makeJAFROCfile function. This file is the source of most JAFROC analyses. 
#' @param wide  Logical flag that is used to indicate whether a wide (columns for various modalities) or tall (data rows are indexed by modality) dataset is returned. The default is a wide datafile.
#' @return  A dataframe with the detection results. For this determination, a detection is any localization where the primary task confidence is greater than or equal to the study minimum detection confidence as set in the \code{applyflowchart} function. 
#' @examples
#' derivedetections("testJAFROC.xlsx")
#' derivedetections("testJAFROC.xlsx", wide=F)

derivedetections <- function(JAFROCfilename,wide=T){
datafilename <- JAFROCfilename

Truthsaved <- readxl::read_excel(datafilename, "Truth")
TPsaved <- readxl::read_excel(datafilename, "TP")


allids <- dplyr::distinct(Truthsaved, LesionID)
allids <- dplyr::filter(allids, LesionID != 0)
allids <- dplyr::rename(allids, RefID = LesionID)


readerids <- dplyr::distinct(TPsaved, ReaderID)
modalityids <- dplyr::distinct(TPsaved, ModalityID) #requires at least one detection for each modality


datamarks <- TPsaved



mergegrid_t1 <- expand.grid(RefID = allids$RefID,  ModalityID = modalityids$ModalityID, ReaderID = readerids$ReaderID, stringsAsFactors = F)

numdetection <- datamarks %>% dplyr::group_by(RefID, ModalityID, ReaderID) %>% dplyr::summarize(numdetections = n())

mergegrid_t2 <- dplyr::left_join(mergegrid_t1, numdetection, by=c("RefID", "ModalityID", "ReaderID"))

mergegrid_t2$numdetections <- ifelse(is.na(mergegrid_t2$numdetections), 0, mergegrid_t2$numdetections)

detections <- mergegrid_t2 %>% dplyr::group_by(RefID, ModalityID) %>% dplyr::summarize(totaldetections = sum(numdetections), reader = n(), pctdetect = mean(numdetections))


detections$fracdetection <- paste0(detections$totaldetections, "/", detections$reader)

#removed to avoid messy output
detections <- dplyr::select(detections, RefID, ModalityID, totaldetections)
detections$RefID <- factor(detections$RefID)
detections$ModalityID <- factor(detections$ModalityID)
detectionswide <- tidyr::spread(detections, ModalityID, totaldetections)

if (wide) {
return(detectionswide)
} else {
  return(detections)
}
}