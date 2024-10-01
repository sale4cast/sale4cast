plotDataAndTrendDaily <- function(datfSplitAggByDay, colName, numOfOutlier) {
  datfSplitAggByDay <- datfSplitAggByDay %>% mutate(Date = make_date(Year, Month, Day), WeekDay = format(Date, "%A"))
  datfSplitAggByDay$WeekDay <- factor(datfSplitAggByDay$WeekDay, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))  
  plotDataAndTrend <- createPlotDataAndTrend(datfSplitAggByDay, colName, numOfOutlier)
  return(plotDataAndTrend) 
}

plotSeasonDaily <- function(tSeriesTibble, colName) {
  if(NROW(tSeriesTibble) >= 180) captionText = "(Last 180 observation is taken as sample set.)"
  else  captionText = NULL    
  tSeriesTibble <- tSeriesTibble %>% mutate(WeekDay = format(Date, "%A"))
  tSeriesTibble$WeekDay <- factor(tSeriesTibble$WeekDay, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))  
  
  plotSeason <- ggplot(data = tSeriesTibble, mapping = aes_string(x = colnames(tSeriesTibble)[1], y = colnames(tSeriesTibble)[4])) + 
    geom_line(size = 1.05, alpha = 1/3) +
    geom_point(mapping = aes(color = WeekDay), size = 2.3) +
    scale_x_date(date_labels = "%d-%b-%Y", breaks = breaks_pretty(6))  + 
    #scale_color_discrete(guide = guide_legend(title = "WeekDay", override.aes = list(alpha = 1, size = 2))) +
    labs(x = "Date", y = colName[1], caption = captionText) + 
    theme(
      legend.position = "top",
      plot.title = element_text(size = rel(1.2), face = "bold"), 
      plot.subtitle = element_text(size = rel(1.0), face = "bold"),
      legend.title = element_text(size = rel(1),face = "bold"), 
      #legend.background = element_rect(fill = "lightblue3", colour = NA),
      axis.title.x = element_text(size = rel(1.15), face = "bold"),
      axis.title.y = element_text(size = rel(1), face = "bold"),
      axis.text.x =  element_text(size = rel(1.3), face = "bold"),
      axis.text.y =  element_text(size = rel(1.3), face = "bold")
    ) +
    guides(x = guide_axis(angle = 20))
  
  return(plotSeason)
}

plotForecastDaily <- function(originFittedForecast, colName) {
  
  if(NCOL(originFittedForecast) > 4) geom_rib <- geom_ribbon(aes(ymin = Lo80, ymax = Hi80), fill = "steelblue2", alpha = 1/5)
  else geom_rib <- NULL
  
  if(NROW(originFittedForecast) >= 180) captionText = "(Last 180 observation is taken as sample set.)"
  else  captionText = NULL  

  originFittedForecast <- originFittedForecast %>% mutate(WeekDay = format(Date, "%A"))
  originFittedForecast$WeekDay <- factor(originFittedForecast$WeekDay, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))  
  
  plotForecast <- ggplot(data = originFittedForecast, mapping = aes(x = Date)) + 
    geom_line(mapping = aes(y = NumberOfCustomer), size = 1.05, alpha = 1/3, na.rm = TRUE) + 
    geom_point(mapping = aes(y = NumberOfCustomer, color = WeekDay), size = 2.3, na.rm = TRUE) + 
    geom_line(mapping = aes(y = Fitted), color = "blue", size = 1.05, alpha = 1/3, na.rm = TRUE) +
    geom_rib +
    geom_line(mapping = aes(y = Forecast), color = "blue", size = 1.05, alpha = 1/3, na.rm = TRUE) + 
    geom_point(mapping = aes(y = Forecast, color = WeekDay), size = 2.3, na.rm = TRUE) +
    scale_x_date(date_labels = "%d-%b-%Y", breaks = breaks_pretty(12))  + 
    scale_color_discrete(guide = guide_legend(title = "WeekDay", override.aes = list(alpha = 1, size = 2))) +
    labs(x = "Date", y = colName[1], caption = captionText) + 
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

#    tSeries %>% diff() %>% ur.kpss() -> tval
#    tval@teststat 
createForecastDaily <- function(NumberOfCustomer, freq) {
     dailyForecast <- NULL
         extraStep <- 0
    
  trainTseries <- ts(head(NumberOfCustomer, NROW(NumberOfCustomer) - freq), frequency = freq) %>% tsclean()
  forecastArima <- trainTseries %>% auto.arima() %>% forecast(h = freq)
  forecastETS <- trainTseries %>% ets() %>% forecast(h = freq)
  if(freq > 1) forecastSTLF <- trainTseries %>% stlf(h = freq)
  forecastNNETAR <- trainTseries %>% nnetar() %>% forecast(h = freq)
  
  trainPlusTestTseries <- ts(NumberOfCustomer, frequency = freq) # no clean
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
#https://stackoverflow.com/questions/37691885/error-in-stl-series-has-less-than-two-periods-erroneous
createForecastPlot <- function(freq, filteredWeekDay, dataForAlgo, datfSplitAggByDayOU, lastDate, colName) {
  plotSeason <- forecastedData <- timeSeries <- tsTibble <- NULL
  totalForcastStep <- seasonStrength <- 0
  DateFromDataForAlgo <- make_date(dataForAlgo$Year, dataForAlgo$Month, dataForAlgo$Day)
  nRow <- NROW(dataForAlgo) 
    ### Bug: if freq = 1, stlf() can have a problem.
    if(nRow < 2*freq)
    {
      timeSeries <- ts(data = dataForAlgo$NumberOfCustomer, frequency = freq)    
      #   nday <- ceiling(nRow/freq) * 7 + freq
      #dateCol <- make_date(dataForAlgo[[1,1]],dataForAlgo[[1,2]],dataForAlgo[[1,3]]) + 0:(nday-1)
      #dateCol <- dateCol[format(dateCol, "%A") %in% c(filteredWeekDay[[1]])]
      totalForcastStep <- freq
      forecastedData <- snaive(timeSeries, h = totalForcastStep) %>% fortify() %>% .[-c(7,8)] %>% as_tibble()  # col 7 and 8 contains 95% confidence interval
    }
    else if(nRow >= 2*freq && nRow <= 3*freq + 1)
    {
      if(nRow >= 2*freq) timeSeries <- ts(data = c(dataForAlgo$NumberOfCustomer[freq], dataForAlgo$NumberOfCustomer), frequency = freq)    
      else timeSeries <- ts(data = dataForAlgo$NumberOfCustomer, frequency = freq)    
      tsTibble <- timeSeries %>% mstl() %>% tail(nRow) %>% as_tibble() %>% add_column(Date = DateFromDataForAlgo, .before = "Data")    
      trendStrength <- max(0, 1 - (var(tsTibble[[5]])/(var(tsTibble[[3]] + tsTibble[[5]]))))
      seasonStrength <- max(0, 1 - (var(tsTibble[[5]])/(var(tsTibble[[4]] + tsTibble[[5]])))) 
      plotSeason <- plotSeasonDaily(tsTibble, colName)
      totalForcastStep <- freq
      forecastedData <- stlf(timeSeries, h = totalForcastStep) %>% fortify() %>% tail(nRow + freq) %>% .[-c(7,8)] %>% as_tibble()  # col 7 and 8 contains 95% confidence interval
    }
    else if(nRow > 3*freq + 1)
    {
      tsTibble <- ts(data = dataForAlgo$NumberOfCustomer, frequency = freq) %>% mstl() %>% as_tibble() %>% add_column(Date = DateFromDataForAlgo, .before = "Data")
      trendStrength <- max(0, 1 - (var(tsTibble[[5]])/(var(tsTibble[[3]] + tsTibble[[5]]))))
      seasonStrength <- max(0, 1 - (var(tsTibble[[5]])/(var(tsTibble[[4]] + tsTibble[[5]]))))    
      plotSeason <- plotSeasonDaily(tsTibble, colName)
      dailyForecast  <- createForecastDaily(dataForAlgo$NumberOfCustomer, freq) 
      forecastedData   <- dailyForecast[[1]]
      totalForcastStep <- dailyForecast[[2]]
      forecastedData <- forecastedData %>% fortify() %>% .[-c(7,8)] %>% as_tibble()  # col 7 and 8 contains 95% confidence interval
    }
           
    fittedData <- tibble(Year = dataForAlgo$Year, Month = dataForAlgo$Month, Day = dataForAlgo$Day, Fitted = head(forecastedData$Fitted,NROW(dataForAlgo)))
    originalAndFittedData <- left_join(datfSplitAggByDayOU, fittedData, by = c("Year", "Month", "Day")) %>% ungroup() %>% mutate(Date = make_date(Year, Month, Day)) %>% select(Date, NumberOfCustomer, Fitted) %>% add_column(Forecast = NA)
    if(NCOL(forecastedData) > 4) originalAndFittedData <- originalAndFittedData %>% add_column(Lo80 = NA, Hi80 = NA)
    dateCol <- lastDate + 1:(7 * ceiling(totalForcastStep/freq) + 7)
    dateCol <- head(dateCol[format(dateCol, "%A") %in% c(filteredWeekDay[[1]])], totalForcastStep)
    forecastPart <- tibble(Date = dateCol, NumberOfCustomer = NA, Fitted = NA, Forecast = tail(forecastedData[[4]], totalForcastStep))
    if(NCOL(forecastedData) > 4) forecastPart <- forecastPart %>% add_column(Lo80 = tail(forecastedData[[5]], totalForcastStep), Hi80 = tail(forecastedData[[6]], totalForcastStep))
    originFittedForecast <- rbind(originalAndFittedData, forecastPart)
    plotForecast  <- plotForecastDaily(originFittedForecast, colName)    
    forecastTable <- forecastPart %>% select(-NumberOfCustomer, -Fitted) %>% mutate(WeekDay = format(Date, "%A")) %>% select(Date, WeekDay, Forecast, everything())
    
    if("SalesQty" %in% colnames(dataForAlgo)) {
      dataForAlgo$SalesQty <- ts(dataForAlgo$SalesQty, frequency = freq) %>% tsclean()
      dataForAlgo <- dataForAlgo %>% mutate(WeekDay = format(make_date(Year, Month, Day), "%A"))
      QtySalesRatioByWday <- dataForAlgo %>% select(WeekDay, NumberOfCustomer, SalesQty) %>% mutate(QtySalesRatio = SalesQty/NumberOfCustomer) %>% select(WeekDay, QtySalesRatio)
      QtySalesRatioByWday$QtySalesRatio <- ts(QtySalesRatioByWday$QtySalesRatio, frequency = freq) %>% tsclean()
      QtySalesRatioByWday <- QtySalesRatioByWday %>% group_by(WeekDay) %>% summarise_at(c("QtySalesRatio"), mean, na.rm = TRUE)
      forecastTable <- left_join(forecastTable, QtySalesRatioByWday, by = "WeekDay")
      forecastTable <- forecastTable %>% mutate(SalesQty = QtySalesRatio * Forecast) %>% select(-QtySalesRatio)
    }
    forecastTable <- forecastTable %>% mutate_if(is.numeric, as.integer)   
    forecastTable$Forecast[forecastTable$Forecast < 0] <- 0
    if("Lo80" %in% colnames(forecastTable)) forecastTable$Lo80[forecastTable$Lo80 < 0] <- 0
    if("Hi80" %in% colnames(forecastTable)) forecastTable$Hi80[forecastTable$Hi80 < 0] <- 0
  return(list(plotSeason, plotForecast, forecastTable, seasonStrength))
}

# case1: Imagine a time series data is sequential and has only 10 rows but cover weekNr 51, 52 and 01.
# Example: Mon1 Tue1 Wed1 Thu1 Fri1 Sat1 Sun1 Mon2 Tue2 Wed2
# case2: Imagine a time series data is not sequential and has only 10 rows. 
# Example: Mon1 Tue1 Wed1, Mon2 Tue2 Wed2, Mon3 Tue3 Wed3 Thu3
# case3: Imagine a time series data is not sequential and has only 10 rows and one day in a week. 
# Example: Mon1, Mon2, Mon3, Mon4, Mon5, Mon6, Mon7, Mon8, Mon9, Mon10

hourToDailyForecast <- function(datfSplitAggByDay) {
  colName <- colnames(datfSplitAggByDay) %>% .[-c(1,2,3)]
  if(NCOL(datfSplitAggByDay) == 4) names(datfSplitAggByDay)[4] <- c("NumberOfCustomer")
  else if(NCOL(datfSplitAggByDay) == 5) names(datfSplitAggByDay)[c(4,5)] <- c("NumberOfCustomer","SalesQty")
   freqLimitOfAdayOvernWeeks <- 0.4
   if(NROW(datfSplitAggByDay) > 1) 
     datfSplitAggByDayOU <- outlierRemoveSalesOrder(datfSplitAggByDay) 
   else 
     datfSplitAggByDayOU <- datfSplitAggByDay 
   numOfOutlier <- NROW(datfSplitAggByDay) - NROW(datfSplitAggByDayOU)
   plotDataAndTrend <- plotDataAndTrendDaily(datfSplitAggByDayOU, colName, numOfOutlier)

   datfSplitAggByDay <- tail(datfSplitAggByDay, 180)
   if(NROW(datfSplitAggByDay) > 1) 
     datfSplitAggByDayOU <- outlierRemoveSalesOrder(datfSplitAggByDay) 
   else 
     datfSplitAggByDayOU <- datfSplitAggByDay 
   
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
          emptyDatf <- tibble(Year = year(dateIndexSeries[,1]), Month = month(dateIndexSeries[,1]), Day = day(dateIndexSeries[,1])) # NumberOfCustomer = 0, SalesQty = 0)
         for(colNo in 4:NCOL(datfSplitAggByDay)) 
         {
           emptyDatf <- emptyDatf %>% add_column(data = -1, .name_repair = make.unique)
           names(emptyDatf)[colNo] <- names(datfSplitAggByDay)[colNo]
         }    
        dataForAlgo <- rows_update(emptyDatf, datfSplitAggByDay, by=c("Year", "Month", "Day"), unmatched = "ignore")
        #dataForAlgo <- dataForAlgo %>% na_if(-1) # %>% na_kalman(dataForAlgo) 
        dataForAlgo$NumberOfCustomer <- dataForAlgo$NumberOfCustomer %>% na_if(-1) # %>% na_kalman()
        if("SalesQty" %in% colnames(dataForAlgo)) dataForAlgo$SalesQty <- dataForAlgo$SalesQty %>% na_if(-1) # %>% na_kalman()        
        dataForAlgo <- dataForAlgo %>% mutate(WeekDay = format(make_date(Year, Month, Day), "%A")) %>% filter(WeekDay %in% filteredWeekDay[[1]])
               freq <- NROW(filteredWeekDay)
      forecastInfo <- createForecastPlot(freq, filteredWeekDay, dataForAlgo, datfSplitAggByDayOU, make_date(lastRow$Year, lastRow$Month, lastRow$Day), colName) 
      forecastTable <- forecastInfo[[3]]
      names(forecastTable)[3] <- colName[1]
      if("SalesQty" %in% colnames(forecastTable)) names(forecastTable)[NCOL(forecastTable)] <- colName[2]
      tagList(
        tags$h5(tags$b("Original and Avaerage Data are represented by Grey and Blue Line.")),
        renderPlot(plotDataAndTrend),
        if(freq != NROW(dataForAlgo)) tags$h5( tags$b(
          "Original Data - Average Data = Seasonality,    Seasonal Strength = ", round(forecastInfo[[4]], digits = 4)," out of 1.0"        
        )),
        if(freq != NROW(dataForAlgo)) renderPlot(forecastInfo[[1]]),
        tags$h5(tags$b("Grey and Blue Line Represent Original and Forecast Data.")),
        renderPlot(forecastInfo[[2]]),
        renderDataTable(forecastTable)
      )
#  return(list(plotDataAndTrend, plotSeason = forecastInfo[[1]], plotForecast = forecastInfo[[2]], forecastTable = forecastInfo[[3]]))
}
