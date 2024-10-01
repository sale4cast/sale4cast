hourToDailyInsight <- function(datfSplitAggByDay){
  if(NCOL(datfSplitAggByDay) == 4) names(datfSplitAggByDay)[4] <- c("NumberOfCustomer")
  else if(NCOL(datfSplitAggByDay) == 5) names(datfSplitAggByDay)[c(4,5)] <- c("NumberOfCustomer","SalesQty")
  datfSplitAggByDay <- datfSplitAggByDay %>% ungroup()
  if("SalesQty" %in% colnames(datfSplitAggByDay)) datfSplitAggByDay <- datfSplitAggByDay %>% select(-SalesQty)
  tagList(
    #renderPlot(dataAndTrendPlot),
    plotDailyInsight(datfSplitAggByDay)
  )
}

