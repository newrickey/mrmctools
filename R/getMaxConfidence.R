
getMaxConfidence <- function(JAFROCfilename){
  TP <- readxl::read_excel(JAFROCfilename,sheet="TP")
  
  ## group the reader / markings to give the maximum value
  
  maxvalues <- TP %>% dplyr::group_by(ReaderID) %>% summarize(maxRating = max(TP_Rating))
  minmax <- min(maxvalues$maxRating)
  return(minmax)
}

