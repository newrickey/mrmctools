#' Plot confidence scores by bin
#' 
#' This function provides violin plots with jittered data for data according to the six classification bins for reader marks (see vignette). There is a simple parameter to change "PT" to something else, but the nature of x-axis design is that the replacement text needs to be brief.
#' 
#' @param flowdata A dataframe that has been returned by the applyflowchart function
#' @param PTtext A text string with the characters that is used describe the primary task. The function defaults to the generic text "PT".
#' @export




plotPTCbins <- function(flowdata, PTtext="PT"){
  
  observerdata <- dplyr::filter(flowdata, !is.na(ObserverROIID))
  observerdata <- dplyr::filter(observerdata, OverPTCThresdhold == 1)
  temp <- dplyr::select(observerdata, ObserverPTC, LesionClassA,LesionClassB, LesionClassC, LesionClassD, LesionClassE, LesionClassF)
  
  # determine the A - F bin
  temp$bin <- ifelse(temp$LesionClassA == 1, "A",
                     ifelse(temp$LesionClassB == 1, "B",
                            ifelse(temp$LesionClassC == 1, "C",
                                   ifelse(temp$LesionClassD == 1, "D",
                                          ifelse(temp$LesionClassE == 1, "E",
                                                 ifelse(temp$LesionClassF == 1, "F", NA))))))
  
  
  temp$bin <- factor(temp$bin, levels=c("A","B", "C", "D","E", "F"),
                     labels=c(paste0("A) Unmatched : Not ", PTtext),
                              paste0("B) Unmatched : ", PTtext),
                              paste0("C) Not ", PTtext, " : Not ", PTtext),
                              paste0("D) ", PTtext, " : Not ", PTtext),
                              paste0("E) Not ", PTtext," : ", PTtext),
                              paste0("F) ", PTtext," : ", PTtext)))
  
  
p <- ggplot(temp, aes(x=bin,y=ObserverPTC, color=bin)) + geom_violin() +
     geom_jitter(height=0, width=.4, alpha=.5) +
     stat_summary(fun.y = "mean", 
                 colour = "black", 
                 size = 4, 
                 geom = "point") +
   stat_summary(fun.y = "max",fun.ymax = "length", 
               colour = "black", 
               size = 5, 
               geom = "text",
               aes(label = ..ymax..), vjust = 1) +
    expand_limits(y=c(0,110))  +
    ylab("Primary Task Confidence") +
    xlab("")+
    ylim(0,100) +
    ggtitle(" Reference Classification : Reader Classification ") +
    theme_bw() + theme(legend.position="none",axis.text.x = element_text(angle = 40, hjust = 1))
  
  
  
  
    return(p)
}
