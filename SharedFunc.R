outlierRemoveSalesOrder <- function(datf) {
    Quantile1 <- quantile(datf$NumOfSalesOrder, probs=.25)
    Quantile3 <- quantile(datf$NumOfSalesOrder, probs=.75)
    iqr <- Quantile3 - Quantile1
    uprange <- Quantile3 + (iqr*1.5)
    lowrange <- Quantile1 - (iqr*1.5)
    datf <- datf %>% filter(NumOfSalesOrder > lowrange & NumOfSalesOrder < uprange)     
    return(datf)
}

outlierRemoveSalesQty <- function(datf) {
  Quantile1 <- quantile(datf$SalesQty, probs=.25)
  Quantile3 <- quantile(datf$SalesQty, probs=.75)
  iqr <- Quantile3 - Quantile1
  uprange <- Quantile3 + (iqr*1.5)
  lowrange <- Quantile1 - (iqr*1.5)
  datf <- datf %>% filter(SalesQty > lowrange & SalesQty < uprange)     
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
