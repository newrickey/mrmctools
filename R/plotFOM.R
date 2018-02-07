## FOM plot
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

