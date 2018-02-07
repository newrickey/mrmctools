## Save a figure file

figuresave <- function(filename,figobject){
paspect <- 4/3
pheight <- 15
pwidth <- paspect*pheight
ggsave(filename, figobject,device="png",height=pheight, width=pwidth, units = "cm")
}
