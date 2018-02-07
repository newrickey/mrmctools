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


