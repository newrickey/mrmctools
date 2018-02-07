
## Helper function

binCI <- function(theest, thelower, theupper,thedigits=1,the100=100){
  workingest <- formatC( round(theest*the100 , thedigits),  format='f', digits=thedigits)
  workinglower <- formatC(round(thelower*the100, thedigits), format='f', digits=thedigits)
  workingupper <- formatC(round(theupper*the100, thedigits), format='f', digits=thedigits)
  CI <-paste0(workingest, " (", workinglower, " to ", workingupper, ")")
  return(CI)
}
