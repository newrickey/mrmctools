plotthreshold_modality <- function(jafrocfile, modality=NA,modalitylabel=NA, includeGEE = T, readercolor="#999999",geecolor="#000000",getminmax=F){
  workingdata <- NULL
  
  ## get the min of the maximum confidence scores by reader. This is to help avoid errors that occur when
  ## the readers don't all have at least one 100 confidence score
  if (getminmax){
  upperlim <- getMaxConfidence(jafrocfile) -1
  } else {
   upperlim <- 99 
  }
  
  for (i in seq(-1,upperlim,5)) {
    temp <- derivelesionsens(jafrocfile,sensitivitythreshold=i)
    temp <- dplyr::filter(temp, ModalityID ==modality)
    temp2 <- dplyr::select(temp, ReaderID, rawsensitivity, ModalityID)
    temp2$threshold <- i+1
    workingdata <- dplyr::bind_rows(workingdata, temp2)
  }
  
  ## Change some line colors
  numr <- unique(workingdata$ReaderID)
  
  if (includeGEE) {
    cbPalette <- c(rep(readercolor, length(numr)-1 ), geecolor)
  } else {
    workingdata <- dplyr::filter(workingdata, ReaderID !="GEE")
    cbPalette <- c(rep(readercolor, length(numr)-1) )
  }
  
  # determine the number of readers to set some colors
  numreaders <- unique(workingdata$ReaderID)
  
  gpooled<- ggplot(workingdata, aes(x=threshold, y=rawsensitivity, color=ReaderID)) +
    geom_line() +
    xlab("Primary Task Confidence Threshold")+
    ylab("Lesion Sensitivity") +
    ggtitle(paste0("Lesion Sensitivity \n ", modalitylabel)) +
    scale_y_continuous(labels = scales::percent,breaks=seq(0, 1.0, 0.1) ) +
    scale_x_continuous(breaks=seq(0, 100, 10) ) +
    scale_colour_manual(values=cbPalette) +
    expand_limits(y=c(0,1), x=c(0, 100))+ theme_bw() +
    theme(legend.position="none")
  
  return(gpooled)
}