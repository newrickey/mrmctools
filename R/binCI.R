#' Combine three numbers into a formatted text string
#' 
#' Function to make a formatted text string version of a confidence interval.
#' 
#' @param est The estimated quantity.
#' @param lower The lower confidence interval value.
#' @param upper The upper confidence interval value.
#' @param ndigits The number of digits following the decimal point. The default value is one decimal point.
#' @param inpct If true, the estimates are multipled by 100 to form whole numbers. The default value is T.
#' @return The text string that combines the three elements of confidence inteval for easy printing.
#' @examples
#' binCI(0.2332, 0.1512, 0.3192)
#' binCI(0.2332, 0.1512, 0.3192, 2, T)
#' binCI(0.2332, 0.1512, 0.3192, 4, F)


binCI <- function(est, lower, upper,ndigits=1,inpct=T){
  if (inpct) {
    the100 <- 100
  } else {
    the100 <- 1
  }
  workingest <- formatC( round(est*the100 , ndigits),  format='f', digits=ndigits)
  workinglower <- formatC(round(lower*the100, ndigits), format='f', digits=ndigits)
  workingupper <- formatC(round(upper*the100, ndigits), format='f', digits=ndigits)
  CI <-paste0(workingest, " (", workinglower, " to ", workingupper, ")")
  return(CI)
}
