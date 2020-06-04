calculateHRT <- function(flow,c) {
  hrt <- NA 
  if("PAA.2018.1" %in% colnames(c)) hrt <- 135.11*flow^-.959
  if("PAA.2018.2" %in% colnames(c)) hrt <-  1551.2*flow^-.971
  if("PAA.2019.1" %in% colnames(c)) hrt <-  56.405*flow^-0.952
  if("PAA.2019.2" %in% colnames(c)) hrt <-  945.68*flow^-0.952
  if("PAA.2019.3" %in% colnames(c)) hrt <-  1825.3*flow^-0.959
  if("PAA.2019.4" %in% colnames(c)) hrt <-  2612.6*flow^-0.952
  return(hrt)
}



