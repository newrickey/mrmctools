#' Calculate patient-level sensitivity and specificity.
#' 
#' The sensitivity denominator will be the number of cases with at least one lesion. 
#' The specificity denominator will be the number of cases without any primary task lesions. 
#' For sensitivity, the calculation requires at least one reference lesion to be found with 
#' confidence greater than what has been specified in the function call. For specificity, it 
#' requires no false positive ROIs with confidence greater than the threshold.  
#' In addition to reader-modality-specific results, the GEE estimate of the 
#' common sensitivity by modality is returned.
#' 
#' 
#' @param JAFROCfilename The is the excel file produced by the makeJAFROCfile function. This file is the source of most JAFROC analyses. 
#' @param sensitivitythreshold A numeric value representing the primary task confidence value that true positive localization need to be greater than to be considered a true detectioin. 
#' @param specificitythreshold A numeric value representing the primary task confidence value that a non-localization needs to be greater than to be counted as a false positive within a normal case.
#' @return A dataframe with results
#' @examples
#' derivesensspec("testJAFROC.xlsx", 10, 10)



derivesensspec <- function(JAFROCfilename,sensitivitythreshold=0,specificitythreshold=0){

  # to read in Truth, TP and FP sheets in the JAFROC file
sheets <- readxl::excel_sheets(JAFROCfilename)
  
  for(i in unique(sheets)) {
  # print(paste0("Reading in ...", i))
      newtable <- readxl::read_excel(JAFROCfilename,sheet=i)
    #now save the table with a unique name
    assign(i, newtable)
    rm(newtable)
  }

## code reference sheets into character values
Truth$CaseID <- as.character(Truth$CaseID)
Truth$LesionID <- as.character(Truth$LesionID)
TP$ReaderID <- as.character(TP$ReaderID)
TP$ModalityID <- as.character(TP$ModalityID)
TP$CaseID <- as.character(TP$CaseID)
TP$RefID <- as.character(TP$RefID)

FP$ReaderID <- as.character(FP$ReaderID)
FP$ModalityID <- as.character(FP$ModalityID)
FP$CaseID <- as.character(FP$CaseID)

## normal cases 
normals <- dplyr::filter(Truth, LesionID == 0)
normalids <- unique(normals$CaseID)
readerids <- unique(TP$ReaderID)
modalityids <- unique(TP$ModalityID)


# build specificity file
## First pull out any FPs in the true cases to leave only the marks in the normals
rois_normals <- dplyr::filter(FP , CaseID %in% normalids)


# build matrix with all normals and all readers--note, this is an ugly way of doing expand.grid
normaliddf_t1 <- data.frame(normalids)
normaliddf<-NULL
for (CaseID in normalids){
  for (ReaderID in readerids){
    for (ModalityID in modalityids){
      tempdf <-NULL
      tempdf$CaseID <- CaseID
      tempdf$ModalityID <- ModalityID
      tempdf$ReaderID <- ReaderID
      
      normaliddf <- dplyr::bind_rows(normaliddf, tempdf)
    }
  }
}
normaliddf <- as.data.frame(normaliddf)



## Specificity Def - No marks with PTC >= delta (ignores classification)
rois_normalsPTC <- dplyr::filter(rois_normals, FP_Rating > specificitythreshold)
specPTC <- rois_normalsPTC %>% dplyr::group_by(CaseID, ModalityID, ReaderID) %>% dplyr::summarize(nFP=n())
specPTC <- dplyr::left_join(normaliddf, specPTC, by=c("CaseID","ModalityID","ReaderID"))


specPTC$TN <- ifelse(is.na(specPTC$nFP),1,0)
specPTC<- dplyr::arrange(specPTC, CaseID, ReaderID)
specPTC$ROIspecificitythreshold <- specificitythreshold

spec <- specPTC


######### build sensitivity table now
abnormals <- dplyr::filter(Truth, LesionID != 0)
abnormalids <- unique(abnormals$CaseID)


abnormaliddf<-NULL
for (CaseID in abnormalids){
  for (ReaderID in readerids){
    for (ModalityID in modalityids){
      tempdf <-NULL
      tempdf$CaseID <- CaseID
      tempdf$ModalityID <- ModalityID
      tempdf$ReaderID <- ReaderID
      abnormaliddf <- dplyr::bind_rows(abnormaliddf, tempdf)
    }
  }
}
abnormaliddf <- as.data.frame(abnormaliddf)


rois_abnormals <- dplyr::filter(TP , CaseID %in% abnormalids)

# definition 1 - at least one TP in an abnormal case with PTC above the sens threshold: requires localization to match from workstation
rois_abnormalsPTC <- dplyr::filter(rois_abnormals, TP_Rating > sensitivitythreshold)
sensD1 <- rois_abnormalsPTC %>% dplyr::group_by(CaseID, ModalityID, ReaderID) %>% dplyr::summarize(nTP=n())

sens <- dplyr::left_join(abnormaliddf, sensD1, by=c("CaseID", "ModalityID", "ReaderID"))
sens$TP <- ifelse(is.na(sens$nTP),0,1)


###  Derive Reader - Modality Sensitivities

sens_reader <- sens %>% dplyr::group_by(ModalityID, ReaderID) %>% dplyr::summarize(rawsensitivity = mean(TP), n=n(),y=sum(TP))

sens_reader_def1 <- binom::binom.confint(sens_reader$y, sens_reader$n, conf.level = 0.95, methods = "wilson")
sens_reader_def1 <- dplyr::rename(sens_reader_def1, y1_check = x, n1_check = n, p1_check = mean, sens_lci = lower, sens_uci = upper)
sens_reader_def1 <- dplyr::select(sens_reader_def1, - method)

sens_reader <- dplyr::bind_cols(sens_reader, sens_reader_def1)

## make some confidence intervals ##.#% (##.#%, ##.#%)
sens_reader$sensitivity<-binCI(sens_reader$rawsensitivity, sens_reader$sens_lci, sens_reader$sens_uci,the100=100, thedigits=1)

sens_reader$sensfrac <- paste0(sens_reader$y, "/", sens_reader$n)

sens_reader <- dplyr::select(sens_reader, ModalityID, ReaderID, sensfrac, sensitivity, rawsensitivity, sens_lci, sens_uci)
sens_reader$sensROIthreshold <- paste0("ROIs > ", sensitivitythreshold)
###  Derive Reader - Modality Specificities

spec_reader <- spec %>% dplyr::group_by(ModalityID, ReaderID) %>% dplyr::summarize(rawspecificity = mean(TN), n=n(),y=sum(TN))

spec_reader_def1 <- binom::binom.confint(spec_reader$y, spec_reader$n, conf.level = 0.95, methods = "wilson")
spec_reader_def1 <- dplyr::rename(spec_reader_def1, y1_check = x, n1_check = n, p1_check = mean, spec_lci = lower, spec_uci = upper)
spec_reader_def1 <- dplyr::select(spec_reader_def1, - method)

spec_reader <- dplyr::bind_cols(spec_reader, spec_reader_def1)

## make some confidence intervals ##.#% (##.#%, ##.#%)
spec_reader$specificity<-binCI(spec_reader$rawspecificity, spec_reader$spec_lci, spec_reader$spec_uci,the100=100, thedigits=1)
spec_reader$specfrac <- paste0(spec_reader$y, "/", spec_reader$n)
#spec_reader <- dplyr::select(spec_reader, -n, -y, -y1_check, -n1_check, -p1_check)
spec_reader <- dplyr::select(spec_reader, ModalityID, ReaderID, specfrac, specificity, rawspecificity, spec_lci, spec_uci)

spec_reader$specROIthreshold <- paste0("ROIs > ", specificitythreshold)

rawDA <- dplyr::inner_join(sens_reader, spec_reader, by=c("ModalityID", "ReaderID"))



### Now derive the GEE estimates pooled over readers


Modality <- unique(sens$ModalityID)
sensitivity_gee <-NULL
for (mods in Modality){
  thedf <- dplyr::filter(sens, ModalityID == mods)
  #print(head(thedf))
  thegee1 <-geepack::geese(TP ~  1, data=thedf, id=CaseID, family=binomial("identity"))
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


specificity_gee <-NULL
for (mods in Modality){
  thedf <- dplyr::filter(spec, ModalityID == mods)
  #print(head(thedf))
  thegee1 <-geepack::geese(TN ~  1, data=thedf, id=CaseID, family=binomial("identity"))
  #  print(thegee1)
  rawspecificity <- thegee1$beta
  spec_lci<- rawspecificity - 1.96 *sqrt(thegee1$vbeta[1,1])
  spec_uci<- rawspecificity + 1.96 *sqrt(thegee1$vbeta[1,1])
  
  tempdf <- data.frame(rawspecificity,spec_lci,spec_uci)
  tempdf$ReaderID <- "GEE"
  tempdf$ModalityID <- mods
  specificity_gee <- dplyr::bind_rows(specificity_gee,tempdf)
}

specificity_gee$specificity <- binCI(specificity_gee$rawspecificity, specificity_gee$spec_lci, specificity_gee$spec_uci,the100=100, thedigits=1)

geeDA <- dplyr::inner_join(sensitivity_gee, specificity_gee, by=c("ReaderID", "ModalityID"))
rawDA$ReaderID <- as.character(rawDA$ReaderID)
finalDA <- dplyr::bind_rows(rawDA, geeDA)

## prepend a 0 for digits <10 and make it a character vector. Used later for GEE
  finalDA$ReaderID <- ifelse(nchar(finalDA$ReaderID) < 2, paste0("0", finalDA$ReaderID), as.character(finalDA$ReaderID)) 



finalDA <- dplyr::arrange(finalDA, ModalityID, ReaderID)

return(finalDA)
}