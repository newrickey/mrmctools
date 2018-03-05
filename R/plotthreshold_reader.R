#' Plot reader sensitivity for a particular reader.
#' 
#' This function produces a plot that shows how reader detections vary by modality as the sensitivity detection threshold increases. Note, this function
#' takes a parameter that indicates the reader number. This needs to be a two character text string with a leader "0" if necessary ("01" for reader 1).
#' @param JAFROCfilename The is the excel file produced by the makeJAFROCfile function. This file is the source of most JAFROC analyses. 
#' @param readerid Character string for the reader ID. The default is "GEE" which means the figure will only show the GEE lines by modality.
#' @param addFactor If set to true, the function will use the flevels and flabels to format the modalities
#' @param flevels Text object that contains the factor levels for formatted printing
#' @param flabels Text object that contains the factor labels for formatted printing
#' @param getminmax  A logical parameter to indicate a call to the \code{getMaxConfidence} function. If set to true, the figure will be truncated at this limit.
#' @return \code{ggplot2} figure object.


plotthreshold_reader <- function(jafrocfile, readerid="GEE",addFactor=F,flevels=NA,flabels=NA,getminmax=F){
  pooledsens <- NULL
  
  ## get the min of the maximum confidence scores by reader. This is to help avoid errors that occur when
  ## the readers don't all have at least one 100 confidence score
  if (getminmax){
    upperlim <- getMaxConfidence(jafrocfile) -1
  } else {
    upperlim <- 99 
  }
  for (i in seq(-1,upperlim,5)) {
    temp <- derivelesionsens(jafrocfile,sensitivitythreshold=i)
    temp <- dplyr::filter(temp, ReaderID ==readerid)
    temp2 <- dplyr::select(temp, ModalityID, rawsensitivity)
    temp2$threshold <- i+1
    pooledsens <- dplyr::bind_rows(pooledsens, temp2)
  }
  
  if (addFactor){
    pooledsens$Treatment <- with(pooledsens, factor(ModalityID, levels=flevels, labels=flabels))
  } else {
    pooledsens$Treatment <- pooledsens$ModalityID
  }
  
  if (readerid =="GEE"){
    readerlabel <- "Pooled (GEE) Estimates"
  } else{
    readerlabel <- paste0("Reader ", readerid)
  }
  gpooled<- ggplot(pooledsens, aes(x=threshold, y=rawsensitivity, color=Treatment)) + geom_line() +
    xlab("Primary Task Confidence Threshold")+
    ylab("Lesion Sensitivity") +
    ggtitle(paste0("Lesion Sensitivity \n ", readerlabel)) +
    scale_y_continuous(labels = scales::percent,breaks=seq(0, 1.0, 0.1) ) +
    scale_x_continuous(breaks=seq(0, 100, 10) ) +
    expand_limits(y=c(0,1), x=c(0, 100))+ theme_bw() +
    theme(legend.position = c(0.1, 0.3))
  
  return(gpooled)
}


