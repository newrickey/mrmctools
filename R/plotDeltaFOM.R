#' Plot Delta FOM from Reference Modality.
#' 
#' This function plots the differences and 95\% CIs from the reference dose in a forest plot. 
#' The function can draw a non-inferiority limit onto the figure at a value specified.
#' 
#' @param dataframe  This the dataframe that is the second list element returned by the prepareestimates function.  The array is processed outside this function to allow for labeling and formatting the modalities before the plot is generated.
#' @param themethod A character string that uses one of the following analysis methods: RRRC (random reader random case), FRRC (fixed reader, random case), RRFC (random reader, random case)
#' @param refdose A text string specifying the name of the reference dose that should be printed on x-axis label
#' @param nonilimit A numeric value to indicate where a dashed line should be drawn for a non-inferiority line to be drawn
#' @param thetreatment A text string that indicates the name of the variable that pertains to the name of the modality. The JAFROC analysis will automatically use Treatment as this name. If for some reason the name is changed, this will allow that new name to be specified.
#' @export
#' @return \code{ggplot2} figure object.


plotDeltaFOM <- function(dataframe, themethod,refdose="Routine Dose",nonilimit=-0.05,thetreatment="Treatment"){
  
  
plotdf <- dplyr::filter(dataframe, method==themethod) 

  
deltaFOMplot <- ggplot(plotdf, aes(estimate, eval(parse(text=thetreatment)), label=deltaFOM_CI)) + geom_point(size=3) + 
  geom_vline(xintercept = nonilimit, linetype = "longdash") +
  geom_vline(xintercept = 0) +
    geom_errorbarh(aes(xmax = upperci, xmin = lowerci,height = .3)) + 
  geom_label(nudge_y = -.3, size=4, color="black",fill="white") +
  expand_limits(x=c(-0.10,0.05)) +ggtitle("Estimated Difference in JAFROC FOM") +
  xlab(paste0("Difference from  ",refdose, " (95% CI)")) +
  ylab(" ") +
  theme_bw() 

return(deltaFOMplot)
}