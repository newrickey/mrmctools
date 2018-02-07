## Delta FOM Plot


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
  theme_bw(base_size = 18) 

return(deltaFOMplot)
}