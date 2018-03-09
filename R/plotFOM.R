#' Plot Figure of Merit.
#' 
#' This function plots the Figure of Merits and 95\% CIs in a forest plot for each modality. 
#' 
#' @param dataframe  This the dataframe that is the first list element returned by the prepareestimates function.  The array is processed outside this function to allow for labeling and formatting the modalities before the plot is generated.
#' @param themethod A character string that uses one of the following analysis methods: RRRC (random reader random case), FRRC (fixed reader, random case), RRFC (random reader, random case)
#' @param thetreatment A text string that indicates the name of the variable that pertains to the name of the modality. The JAFROC analysis will automatically use Treatment as this name. If for some reason the name is changed, this will allow that new name to be specified.
#' @export
#' @return \code{ggplot2} figure object

plotFOM <- function(dataframe, themethod, thetreatment="Treatment"){

fomsummary <- dplyr::filter(dataframe, method==themethod) #

FOMplot <- ggplot(fomsummary, aes(Area, eval(parse(text=thetreatment)), label=FOM_CI)) + geom_point(size=3) + 
  geom_errorbarh(aes(xmax = `CI Upper`, xmin = `CI Lower`,height = .3)) +
  geom_label(nudge_y = -.35, size=4)+
  expand_limits(x=c(0.50,1.00)) +ggtitle("JAFROC Figure of Merits (FOM)") +
  xlab("Estimated FOM (95% CI)") +
  ylab(" ") +
  theme_bw(base_size = 18) 
return(FOMplot)
}

