#' Internal function to get maximum confidence score.
#'
#' This is a helper function to find the maximum confidence level reported by reader. 
#' This function avoids a bug where some readers will not report confidence to 100 for some lesions. 
#' This causes a for loop to error out in the \code{plotthresholds*} functions.
#' 
#' @param JAFROCfilename The is the excel file produced by the makeJAFROCfile function. This file is the source of most JAFROC analyses. 
#' @return Returns the smallest maximum confidence score across all readers (and modalities). 
#' @examples 
#' getMaxConfidence("testJAFROC.xlsx")


getMaxConfidence <- function(JAFROCfilename){
  TP <- readxl::read_excel(JAFROCfilename,sheet="TP")
  
  ## group the reader / markings to give the maximum value
  
  maxvalues <- TP %>% dplyr::group_by(ReaderID) %>% summarize(maxRating = max(TP_Rating))
  minmax <- min(maxvalues$maxRating)
  return(minmax)
}

