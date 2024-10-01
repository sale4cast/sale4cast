#    tSeries %>% diff() %>% ur.kpss() -> tval
#    tval@teststat 
createForecastFromDailyData <- function(NumOfSalesOrder, freq) {
  dailyForecast <- NULL
  extraStep <- 0
  
  trainTseries <- ts(head(NumOfSalesOrder, NROW(NumOfSalesOrder) - freq), frequency = freq) %>% tsclean()
  forecastArima <- trainTseries %>% auto.arima() %>% forecast(h = freq)
  forecastETS <- trainTseries %>% ets() %>% forecast(h = freq)
  if(freq > 1) forecastSTLF <- trainTseries %>% stlf(h = freq)
  forecastNNETAR <- trainTseries %>% nnetar() %>% forecast(h = freq)
  
  trainPlusTestTseries <- ts(NumOfSalesOrder, frequency = freq) # no clean
  accuArima <- accuracy(forecastArima, trainPlusTestTseries)
  accuETS <- accuracy(forecastETS, trainPlusTestTseries)
  if(freq > 1) accuSTLF <- accuracy(forecastSTLF, trainPlusTestTseries)
  accuNNR <- accuracy(forecastNNETAR, trainPlusTestTseries)
  if(freq > 1) algoAccuOnTestData <- rbind(accuArima[2,1:7], accuETS[2,1:7], accuSTLF[2,1:7], accuNNR[2,1:7]) %>% as_tibble() %>% cbind(algoName = c("Arima", "ETS", "STLF", "NNR")) %>% select(algoName, everything())          
  else algoAccuOnTestData <- rbind(accuArima[2,1:7], accuETS[2,1:7], accuNNR[2,1:7]) %>% as_tibble() %>% cbind(algoName = c("Arima", "ETS", "NNR")) %>% select(algoName, everything())          
  #print(algoAccuOnTestData)
  
  testMAPEmin <- algoAccuOnTestData %>% select(algoName, MAPE) %>% filter(MAPE == min(algoAccuOnTestData[6]))
  #print(paste("algoName: ", testMAPEmin$algoName))
  trainPlusTestTseries <- trainPlusTestTseries %>% tsclean()
  
  if(NROW(trainPlusTestTseries) - 30 > 0) extraStep <- floor((NROW(trainPlusTestTseries) - 30) / 7) 
  totalForcastStep <- freq + extraStep
  if(testMAPEmin$algoName == "Arima")
    dailyForecast <- trainPlusTestTseries %>% auto.arima() %>% forecast(h = totalForcastStep)
  if(testMAPEmin$algoName == "ETS")
    dailyForecast <- trainPlusTestTseries %>% ets() %>% forecast(h = totalForcastStep)    
  if(testMAPEmin$algoName == "STLF")
    dailyForecast <- trainPlusTestTseries %>% stlf(h = totalForcastStep)
  if(testMAPEmin$algoName == "NNR")
    dailyForecast <- trainPlusTestTseries %>% nnetar() %>% forecast(h = totalForcastStep)    
  
  return(list(dailyForecast, totalForcastStep))
}

plotForecastFromDailyData <- function(originForecast) {
  
  if(NCOL(originForecast) > 4) geom_rib <- geom_ribbon(aes(ymin = Lo80, ymax = Hi80), fill = "steelblue2", alpha = 1/5)
  else geom_rib <- NULL
  
  if(NROW(originForecast) >= 180) captionText = "(Last 180 observation is taken as sample set.)"
  else  captionText = NULL  

  plotForecast <- ggplot(data = originForecast, mapping = aes(x = Date)) + 
    geom_line(mapping = aes(y = .data[[colnames(originForecast)[2]]]), size = 1.05, alpha = 1/3, na.rm = TRUE) + 
    geom_point(mapping = aes(y = .data[[colnames(originForecast)[2]]]), color = "orange", size = 1.3, na.rm = TRUE) + 
    geom_rib +
    geom_line(mapping = aes(y = Forecast), color = "blue", size = 1.05, alpha = 1/3, na.rm = TRUE) + 
    geom_point(mapping = aes(y = Forecast, color = format(Date, "%A")), size = 2.3, na.rm = TRUE) +
    scale_x_date(date_labels = "%d-%b-%Y", breaks = breaks_pretty(12))  + 
    scale_color_discrete(guide = guide_legend(title = "WeekDay", override.aes = list(alpha = 1, size = 2))) +
    theme(
      legend.position = "top",
      plot.title = element_text(size = rel(1.2), face = "bold"), 
      legend.title = element_text(size = rel(1),face = "bold"), 
      axis.title.x = element_text(size = rel(1.15), face = "bold"),
      axis.title.y = element_text(size = rel(1), face = "bold"),
      axis.text.x =  element_text(size = rel(1.3), face = "bold"),
      axis.text.y =  element_text(size = rel(1.3), face = "bold"),
      #plot.caption = element_text(face = "italic")      
    ) +
    guides(x = guide_axis(angle = 20))
  return(plotForecast)
}

#https://stackoverflow.com/questions/37691885/error-in-stl-series-has-less-than-two-periods-erroneous
prepareForecastData <- function(freq, filteredWeekDay, dataForAlgo, datfSplitAggByDayOU, lastDate) {
  plotSeason <- forecastedData <- timeSeries <- tsTibble <- NULL
  totalForcastStep <- seasonStrength <- 0
  nRow <- NROW(dataForAlgo) 
    ### Bug: if freq = 1, stlf() can have a problem.
    if(nRow < 2*freq)
    {
      timeSeries <- ts(data = dataForAlgo[[4]], frequency = freq)    
      forecastedData <- snaive(timeSeries, h = freq) %>% fortify() %>% .[-c(7,8)] %>% as_tibble()  # col 7 and 8 contains 95% confidence interval
      totalForcastStep = freq
    }
    else if(nRow >= 2*freq && nRow <= 3*freq + 1)
    {
      if(nRow >= 2*freq) timeSeries <- ts(data = c(dataForAlgo[[4]][freq], dataForAlgo$NumOfSalesOrder), frequency = freq)    
      else timeSeries <- ts(data = dataForAlgo[[4]], frequency = freq)    
      forecastedData <- stlf(timeSeries, h = freq) %>% fortify() %>% tail(nRow + freq) %>% .[-c(7,8)] %>% as_tibble()  # col 7 and 8 contains 95% confidence interval
      totalForcastStep = freq
    }
    else if(nRow > 3*freq + 1)
    {
      dailyForecast  <- createForecastFromDailyData(dataForAlgo[[4]], freq) 
      forecastedData   <- dailyForecast[[1]]
      totalForcastStep <- dailyForecast[[2]]
      forecastedData <- forecastedData %>% fortify() %>% .[-c(7,8)] %>% as_tibble()  # col 7 and 8 contains 95% confidence interval
    }
    originalData <- datfSplitAggByDayOU %>% mutate(Date = make_date(Year, Month, Day)) %>% .[c(5,4)] %>% add_column(Forecast = NA)
    if(NCOL(forecastedData) > 4) originalData <- originalData %>% add_column(Lo80 = NA, Hi80 = NA)
    dateCol <- lastDate + 1:(7 * ceiling(totalForcastStep/freq) + 7)
    dateCol <- head(dateCol[format(dateCol, "%A") %in% c(filteredWeekDay[[1]])], totalForcastStep)
    forecastPart <- tibble(Date = dateCol, OriData = NA, Forecast = tail(forecastedData[[4]], totalForcastStep))
    if(NCOL(forecastedData) > 4) forecastPart <- forecastPart %>% add_column(Lo80 = tail(forecastedData[[5]], totalForcastStep), Hi80 = tail(forecastedData[[6]], totalForcastStep))
    names(forecastPart)[2] <- names(originalData)[2]
    originDataAndForecast <- rbind(originalData, forecastPart)
    return(list(originDataAndForecast, totalForcastStep))
}
# case1: Imagine a time series data is sequential and has only 10 rows but cover weekNr 51, 52 and 01.
# Example: Mon1 Tue1 Wed1 Thu1 Fri1 Sat1 Sun1 Mon2 Tue2 Wed2
# case2: Imagine a time series data is not sequential and has only 10 rows. 
# Example: Mon1 Tue1 Wed1, Mon2 Tue2 Wed2, Mon3 Tue3 Wed3 Thu3
# case3: Imagine a time series data is not sequential and has only 10 rows and one day in a week. 
# Example: Mon1, Mon2, Mon3, Mon4, Mon5, Mon6, Mon7, Mon8, Mon9, Mon10

createDailyForecastAndPlot  <- function(datfSplitAggByDay) {
   freqLimitOfAdayOvernWeeks <- 0.4
   if(NROW(datfSplitAggByDay) > 1) 
     datfSplitAggByDayOU <- datfSplitAggByDay %>% mutate(zscore = (abs(.[[4]] - mean(.[[4]]))/sd(.[[4]]))) %>% filter( zscore < 1.96) %>% select(-zscore)
   else 
     datfSplitAggByDayOU <- datfSplitAggByDay 
   numOfOutlier <- NROW(datfSplitAggByDay) - NROW(datfSplitAggByDayOU)
   datfSplitAggByDay <- tail(datfSplitAggByDay, 180)

               Date <- make_date(datfSplitAggByDay$Year, datfSplitAggByDay$Month, datfSplitAggByDay$Day) 
 totWdayWithinAweek <- tibble(WeekDay = format(Date, "%A"), WeekNr = format(Date, "%V")) %>% group_by(WeekNr) %>% summarise(nDays = n())               
             nWeeks <- NROW(totWdayWithinAweek)
   totWdayByUniqueWdayinAweek <- NROW(Date) / max(totWdayWithinAweek$nDays)
   if(totWdayByUniqueWdayinAweek < 2) freqLimitOfAdayOvernWeeks <- 0.3
             
    filteredWeekDay <- tibble(WeekDay = format(Date, "%A")) %>% group_by(WeekDay) %>% summarise(freqRateOfAdayOvernWeeks = n()/nWeeks) 
    filteredWeekDay <- filteredWeekDay %>% filter(freqRateOfAdayOvernWeeks > freqLimitOfAdayOvernWeeks) %>% select(WeekDay)
           firstRow <- head(datfSplitAggByDay,1) 
            lastRow <- tail(datfSplitAggByDay,1)
    dateIndexSeries <- seq(from= make_date(firstRow[[1,1]], firstRow[[1,2]], firstRow[[1,3]]), to= make_date(lastRow[[1,1]], lastRow[[1,2]], lastRow[[1,3]]), by="day") %>% as.data.frame()
    dateIndexSeries <- filter(dateIndexSeries, format(dateIndexSeries[,1], "%A") %in% c(filteredWeekDay[[1]]))
          emptyDatf <- tibble(Year = year(dateIndexSeries[,1]), Month = month(dateIndexSeries[,1]), Day = day(dateIndexSeries[,1])) # NumOfSalesOrder = 0, SalesQty = 0)
           emptyDatf <- emptyDatf %>% add_column(data = -1, .name_repair = make.unique)
           names(emptyDatf)[4] <- names(datfSplitAggByDay)[4]
      dataForAlgo <- rows_update(emptyDatf, datfSplitAggByDay, by=c("Year", "Month", "Day"), unmatched = "ignore")
      dataForAlgo[[4]] <- dataForAlgo[[4]] %>% na_if(-1) # %>% na_kalman()
      freq <- NROW(filteredWeekDay)
      originDataAndForecast <- prepareForecastData(freq, filteredWeekDay, dataForAlgo, datfSplitAggByDayOU, make_date(lastRow$Year, lastRow$Month, lastRow$Day)) 
      forecastPlot  <- plotForecastFromDailyData(originDataAndForecast[[1]])
      totalForecastStep <- originDataAndForecast[[2]]
      forecastTable <- tail(originDataAndForecast[[1]], totalForecastStep) %>% .[-c(2)] %>% mutate_if(is.numeric, as.integer) %>% mutate(Day = format(Date, "%A")) %>% select(Date, Day, everything())
#      browser()
      datfSplitAggByDay <- NULL
#      tags$h5(tags$b("Black and Blue Line Represent Original and Forecast Data."))
      return(list(renderPlot(forecastPlot), renderDataTable(forecastTable)))
#  return(list(plotDataAndTrend, plotSeason = forecastInfo[[1]], plotForecast = forecastInfo[[2]], forecastTable = forecastInfo[[3]]))
}

dailyForecastAndPlot  <- function(splitedDatf) {
 rplot = list()
 rplot <- lapply(4:NCOL(splitedDatf), function(rowIndex){
   splitedDatfC <- splitedDatf %>% .[c(1,2,3,rowIndex)] %>% na.omit()
   createDailyForecastAndPlot(splitedDatfC)
 })
 rplot
}
