# > vec <- c(27)
# > (abs(vec-mean(vec))/sd(vec))
# [1] NA
# > vec <- c(27, 25)
# > (abs(vec-mean(vec))/sd(vec))
# [1] 0.7071068 0.7071068
# > vec <- c(27, 25, 1)
# > (abs(vec-mean(vec))/sd(vec))
# [1] 0.6450859 0.5068532 1.1519392
# > vec <- c(27, 25, 1, 49)
# > (abs(vec-mean(vec))/sd(vec))
# [1] 0.07644708 0.02548236 1.24863562 1.19767090
# > vec <- c(27, 25, 1, 149)
# > (abs(vec-mean(vec))/sd(vec))
# [1] 0.3522139 0.3821896 0.7418975 1.4763010
# > vec <- c(25,16,9, 8,7,6,5,4,1,1,1,1,2,2,2,2,3,3,3,3,4,5,6,7,8,9,15,27)
# > (abs(vec-mean(vec))/sd(vec))
# [1] 2.74673431 1.40270121 0.35734213 0.20800512 0.05866811 0.09066890 0.24000591
# [8] 0.38934292 0.83735395 0.83735395 0.83735395 0.83735395 0.68801694 0.68801694
# [15] 0.68801694 0.68801694 0.53867993 0.53867993 0.53867993 0.53867993 0.38934292
# [22] 0.24000591 0.09066890 0.05866811 0.20800512 0.35734213 1.25336420 3.04540833
outlierRemoveSalesOrder <- function(datf) {
  # Quantile1 <- quantile(datf$NumberOfCustomer, probs=.25)
  # Quantile3 <- quantile(datf$NumberOfCustomer, probs=.75)
  # iqr <- Quantile3 - Quantile1
  # uprange <- Quantile3 + (iqr*1.5)
  # lowrange <- Quantile1 - (iqr*1.5)
  # datf <- datf %>% filter(NumberOfCustomer > lowrange & NumberOfCustomer < uprange) 

  datf$zscore <- (abs(datf$NumberOfCustomer-mean(datf$NumberOfCustomer))/sd(datf$NumberOfCustomer))
  datf <- datf %>% filter(zscore < 1.8) %>% select(-zscore)
  return(datf)
}

outlierRemoveSalesQty <- function(datf) {
#  Quantile1 <- quantile(datf$SalesQty, probs=.25)
#  Quantile3 <- quantile(datf$SalesQty, probs=.75)
#  iqr <- Quantile3 - Quantile1
#  uprange <- Quantile3 + (iqr*1.5)
#  lowrange <- Quantile1 - (iqr*1.5)
#  datf <- datf %>% filter(SalesQty > lowrange & SalesQty < uprange)     
  
  datf$zscore <- (abs(datf$SalesQty-mean(datf$SalesQty))/sd(datf$SalesQty))
  datf <- datf %>% filter(zscore < 1.8) %>% select(-zscore)
  return(datf)
}

formatQYMYDMY <- function(timeFormat) {
  if(dateFormat[[2]] == "ym" || dateFormat[[2]] == "my" ||
     dateFormat[[2]] == "qy" || dateFormat[[2]] == "yq" ||
     dateFormat[[2]] == "ymd" || dateFormat[[2]] == "ydm" || 
     dateFormat[[2]] == "mdy" || dateFormat[[2]] == "dmy")
    return(TRUE)
  else
    return(FALSE)
}

formatDMYHMS <- function(timeFormat) {
  if(timeFormat == "ymd_hms" || timeFormat == "ymd_hm" || timeFormat == "ymd_h" ||
     timeFormat == "dmy_hms" || timeFormat == "dmy_hm" || timeFormat == "dmy_h" ||
     timeFormat == "mdy_hms" || timeFormat == "mdy_hm" || timeFormat == "mdy_h" ||
     timeFormat == "ydm_hms" || timeFormat == "ydm_hm" || timeFormat == "ydm_h") 
    return(TRUE)
  else
    return(FALSE)
}

minMaxLim <- function(vec) {
  maxVal <- max(vec)
  minVal <- min(vec)
  offVal <- (maxVal - minVal)/50
  maxVal = maxVal + offVal
  if(minVal != 0 && (minVal - offVal) >= 0) minVal = minVal - offVal
  return(c(minVal, maxVal))
}
