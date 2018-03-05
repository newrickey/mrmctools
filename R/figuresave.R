#' Save package figure to PNG.
#' 
#' This is a simple wrapper function for \code{ggsave} to make a 4:3 ratio image that seems to produce 
#' saved PNGs that look reasonable from figures generated in this package.  
#' 
#' @param filename The desired PNG filename, with extension
#' @param figobject  The figure object (ggplot object) to be saved
#' @param figheight  The desired heigh in CM. The default is set at 15 cm, which seems to work fine without further specification
#' @examples
#' figuresave("PTCbyBins.PNG", plotPTC("testJAFROC.xlsx"))

figuresave <- function(filename,figobject, figheight=15){
paspect <- 4/3
pheight <- figheight
pwidth <- paspect*pheight
ggsave(filename, figobject,device="png",height=pheight, width=pwidth, units = "cm")
}
