## function to calculate detection rates by lesions

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