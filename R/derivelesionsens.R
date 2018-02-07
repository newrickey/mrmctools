### derive lesion specific detection rates


derivelesionsens <- function(JAFROCfilename,sensitivitythreshold=0){
  
  # to read in Truth, TP and FP sheets in the JAFROC file
  sheets <- readxl::excel_sheets(JAFROCfilename)
  
  for(i in unique(sheets)) {
    # print(paste0("Reading in ...", i))
    newtable <- readxl::read_excel(JAFROCfilename,sheet=i)
    #now save the table with a unique name
    assign(i, newtable)
    rm(newtable)
  }
 
  
  ## requires each reader and modality to have at least one TP
  readerids <- unique(TP$ReaderID)
  modalityids <- unique(TP$ModalityID)
  
  abnormals <- dplyr::filter(Truth, LesionID != 0)
  lesionids <- unique(abnormals$LesionID)
  
  
  lesiondf_t1 <- expand.grid(RefID = lesionids, ReaderID = readerids, ModalityID = modalityids, stringsAsFactors = F)
  
  lesiondf <- dplyr::left_join(lesiondf_t1, TP, by=c("ReaderID", "ModalityID", "RefID"))
  
  lesiondf$TP <- ifelse(lesiondf$TP_Rating > sensitivitythreshold, 1,0)
  lesiondf$TP <- ifelse(is.na(lesiondf$TP), 0, lesiondf$TP)
  
  
  ###  Derive Reader - Modality Sensitivities
  
  sens_reader <- lesiondf %>% dplyr::group_by(ModalityID, ReaderID) %>% dplyr::summarize(rawsensitivity = mean(TP), n=n(),y=sum(TP))
  
  sens_reader_def1 <- binom::binom.confint(sens_reader$y, sens_reader$n, conf.level = 0.95, methods = "wilson")
  sens_reader_def1 <- dplyr::rename(sens_reader_def1, y1_check = x, n1_check = n, p1_check = mean, sens_lci = lower, sens_uci = upper)
  sens_reader_def1 <- dplyr::select(sens_reader_def1, - method)
  
  sens_reader <- dplyr::bind_cols(sens_reader, sens_reader_def1)
  ## make some confidence intervals ##.#% (##.#%, ##.#%)
  sens_reader$sensitivity<-binCI(sens_reader$rawsensitivity, sens_reader$sens_lci, sens_reader$sens_uci,the100=100, thedigits=1)
  
  sens_reader$sensfrac <- paste0(sens_reader$y, "/", sens_reader$n)
  
  sens_reader <- dplyr::select(sens_reader, ModalityID, ReaderID, sensfrac, sensitivity, rawsensitivity, sens_lci, sens_uci)
  sens_reader$sensROIthreshold <- paste0("ROIs > ", sensitivitythreshold)
  
  
  
  
 
  sensitivity_gee <-NULL
  for (mods in modalityids){
    thedf <- dplyr::filter(lesiondf, ModalityID == mods)
    #print(head(thedf))
    thegee1 <-geepack::geese(TP ~  1, data=thedf, id=RefID, family=binomial("identity"))
    #  print(thegee1)
    rawsensitivity <- thegee1$beta
    sens_lci<- rawsensitivity - 1.96 *sqrt(thegee1$vbeta[1,1])
    sens_uci<- rawsensitivity + 1.96 *sqrt(thegee1$vbeta[1,1])
    
    tempdf <- data.frame(rawsensitivity,sens_lci,sens_uci)
    tempdf$ReaderID <- "GEE"
    tempdf$ModalityID <- mods
    sensitivity_gee <- dplyr::bind_rows(sensitivity_gee,tempdf)
  }
  
  sensitivity_gee$sensitivity <- binCI(sensitivity_gee$rawsensitivity, sensitivity_gee$sens_lci, sensitivity_gee$sens_uci,the100=100, thedigits=1)
  #print(head(sens_reader))
  #print(head(sensitivity_gee))
  
  sens_reader$ReaderID <- as.character(sens_reader$ReaderID)
  finallesion <- dplyr::bind_rows(sens_reader, sensitivity_gee)
  
  ## prepend a 0 for digits <10 and make it a character vector. Used later for GEE
  finallesion$ReaderID <- ifelse(nchar(finallesion$ReaderID) < 2, paste0("0", finallesion$ReaderID), as.character(finallesion$ReaderID)) 
  
  finallesion <- dplyr::arrange(finallesion, ModalityID, ReaderID)
  return(finallesion)
   
}