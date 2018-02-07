### Plot Distribution of Scores

plotPTC <- function(datafilename){

TPsaved <- readxl::read_excel(datafilename, "TP")

TPsaved$Source <- "Lesion Localization"

TPsaved <- dplyr::rename(TPsaved, Rating = TP_Rating)


FPsaved <- readxl::read_excel(datafilename,  "FP")
FPsaved$Source <- "Non-lesion Localization"
FPsaved <- dplyr::rename(FPsaved, Rating = FP_Rating)


datamarks <- dplyr::bind_rows(TPsaved, FPsaved)



ratinghist <- ggplot(aes(x = Rating ) , data = datamarks) + 
  geom_histogram(aes(fill = Source ), binwidth=5, colour="grey20", lwd=0.2) +
  #  stat_bin(binwidth=5, geom="text", colour="black", size=3.5, aes(label=..count.., fill=Source, y=.75*(..count..))) +
  scale_x_continuous(breaks=seq(0,100, 5)) + ylab("Frequency")+
  theme_bw(base_size = 18) +
  theme( legend.position = c(0.40, 0.85), legend.text=element_text(size=12))  +
  guides(fill=guide_legend(title="Reader Mark Classification"))

return(ratinghist)
}