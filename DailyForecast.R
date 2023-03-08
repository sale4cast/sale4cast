plotDataAndTrendDaily <- function(datfSplitAggByDay) {
  datfSplitAggByDay <- datfSplitAggByDay %>% mutate(Date = make_date(Year, Month, Day), WeekDay = format(Date, "%A"))
  plotDataAndTrend <- createPlotDataAndTrend(datfSplitAggByDay)
  return(plotDataAndTrend) 
}

plotSeasonDaily <- function(tSeriesTibble, seasonStrength) {
  plotSeason <- ggplot(data = tSeriesTibble, mapping = aes(x = Date, y = Seasonal7)) + geom_line(size = 1.05, alpha = 1/3) +
    geom_point(mapping = aes(color = format(Date, "%A")), size = 2.3) +
    scale_x_date(date_labels = "%d-%b-%Y")  + 
    scale_color_discrete(guide = guide_legend(title = "WeekDay", override.aes = list(alpha = 1, size = 2))) +
    labs(x = "Date", y = "Number of Sales Order") + 
    theme(
      legend.position = "top",
      plot.title = element_text(size = rel(1.2), face = "bold"), 
      plot.subtitle = element_text(size = rel(1.0), face = "bold"),
      legend.title = element_text(size = rel(1),face = "bold"), 
      #      legend.background = element_rect(fill = "lightblue3", colour = NA),
      axis.title.x = element_text(size = rel(1.15), face = "bold"),
      axis.title.y = element_text(size = rel(1), face = "bold"),
      axis.text.x =  element_text(size = rel(1.3), face = "bold"),
      axis.text.y =  element_text(size = rel(1.3), face = "bold")
      #      plot.background = element_rect(fill = "lightblue3", colour = NA),
      #      panel.background = element_rect(fill = "lightblue", colour = NA),
      #      axis.text = element_text(colour = "linen"),
      #      axis.title = element_text(colour = "linen")      
    ) +
    guides(x = guide_axis(angle = 20))
  
  return(plotSeason)
}

#plotForecast <- ggplot(data = forecastedData, mapping = aes(x = Date)) + 
#geom_line(data = datfSplitAggByDayOU, mapping = aes(x = Date, y = NumOfSalesOrder), size = 1.05, alpha = 1/3) + 
  
plotForecastDaily <- function(originFittedForecast) {
  
  if(NCOL(originFittedForecast) > 4) geom_rib <- geom_ribbon(aes(ymin = Lo80, ymax = Hi80), fill = "steelblue2", alpha = 1/5)
  else geom_rib <- NULL
  plotForecast <- ggplot(data = originFittedForecast, mapping = aes(x = Date)) + 
    geom_line(mapping = aes(y = NumOfSalesOrder), size = 1.05, alpha = 1/3) + 
    geom_point(mapping = aes(y = NumOfSalesOrder, color = format(Date, "%A")), size = 2.3) + 
    geom_line(mapping = aes(y = Fitted), color = "blue", size = 1.05, alpha = 1/3) +
    geom_rib +
    geom_line(mapping = aes(y = Forecast), color = "blue", size = 1.05, alpha = 1/3) + 
    geom_point(mapping = aes(y = Forecast, color = format(Date, "%A")), size = 2.3) +
    scale_x_date(date_labels = "%d-%b-%Y")  + 
    scale_color_discrete(guide = guide_legend(title = "WeekDay", override.aes = list(alpha = 1, size = 2))) +
    labs(x = "Date", y = "Number of Sales Order") + 
    theme(
      legend.position = "top",
      plot.title = element_text(size = rel(1.2), face = "bold"), 
      legend.title = element_text(size = rel(1),face = "bold"), 
      axis.title.x = element_text(size = rel(1.15), face = "bold"),
      axis.title.y = element_text(size = rel(1), face = "bold"),
      axis.text.x =  element_text(size = rel(1.3), face = "bold"),
      axis.text.y =  element_text(size = rel(1.3), face = "bold")
    ) +
    guides(x = guide_axis(angle = 20))
  
  return(plotForecast)
}

#    tSeries %>% diff() %>% ur.kpss() -> tval
#    tval@teststat 
createForecastDaily <- function(NumOfSalesOrder, freq) {
    dailyForecast <- NULL
        extraStep <- 0
  NumOfSalesOrder <- tail(NumOfSalesOrder, 120)
   
    trainTseries <- ts(head(NumOfSalesOrder, NROW(NumOfSalesOrder) - freq), frequency = freq) %>% tsclean()
   forecastArima <- trainTseries %>% auto.arima() %>% forecast(h = freq)
     forecastETS <- trainTseries %>% ets() %>% forecast(h = freq)
    forecastSTLF <- trainTseries %>% stlf(h = freq)
  forecastNNETAR <- trainTseries %>% nnetar() %>% forecast(h = freq)
  
  trainPlusTestTseries <- ts(NumOfSalesOrder, frequency = freq) # no clean
  accuArima <- accuracy(forecastArima, trainPlusTestTseries)
    accuETS <- accuracy(forecastETS, trainPlusTestTseries)
   accuSTLF <- accuracy(forecastSTLF, trainPlusTestTseries)
    accuNNR <- accuracy(forecastNNETAR, trainPlusTestTseries)
  algoAccuOnTestData <- rbind(accuArima[2,1:7], accuETS[2,1:7], accuSTLF[2,1:7], accuNNR[2,1:7]) %>% as_tibble() %>% cbind(algoName = c("Arima", "ETS", "STLF", "NNR")) %>% select(algoName, everything())          
  print(algoAccuOnTestData)
  
  testMAPEmin <- algoAccuOnTestData %>% select(algoName, MAPE) %>% filter(MAPE == min(algoAccuOnTestData[6]))
  print(paste("algoName: ", testMAPEmin$algoName))
  trainPlusTestTseries <- trainPlusTestTseries %>% tsclean()

  if(NROW(trainPlusTestTseries) - 30 > 0) extraStep <- floor((NROW(trainPlusTestTseries) - 30) / 7) 
    totalForcastStep <<- freq + extraStep
  if(testMAPEmin$algoName == "Arima")
    dailyForecast <- trainPlusTestTseries %>% auto.arima() %>% forecast(h = totalForcastStep)
  if(testMAPEmin$algoName == "ETS")
    dailyForecast <- trainPlusTestTseries %>% ets() %>% forecast(h = totalForcastStep)    
  if(testMAPEmin$algoName == "STLF")
    dailyForecast <- trainPlusTestTseries %>% stlf(h = totalForcastStep)
  if(testMAPEmin$algoName == "NNR")
    dailyForecast <- trainPlusTestTseries %>% nnetar() %>% forecast(h = totalForcastStep)
  return(dailyForecast)
}
#https://stackoverflow.com/questions/37691885/error-in-stl-series-has-less-than-two-periods-erroneous
createForecastPlot <- function(freq, filteredWeekDay, dataForAlgo, datfSplitAggByDayOU, lastDate) {
  plotSeason <- forecastedData <- timeSeries <- tsTibble <- NULL
    DateFromDataForAlgo <- make_date(dataForAlgo$Year, dataForAlgo$Month, dataForAlgo$Day)
    nRow <- NROW(dataForAlgo) 
    if(nRow < 2*freq)
    {
      timeSeries <- ts(data = dataForAlgo$NumOfSalesOrder, frequency = freq)    
      #   nday <- ceiling(nRow/freq) * 7 + freq
      #dateCol <- make_date(dataForAlgo[[1,1]],dataForAlgo[[1,2]],dataForAlgo[[1,3]]) + 0:(nday-1)
      #dateCol <- dateCol[format(dateCol, "%A") %in% c(filteredWeekDay[[1]])]
      totalForcastStep <<- freq
      forecastedData <- snaive(timeSeries, h = totalForcastStep) %>% fortify() %>% .[-c(7,8)] %>% as_tibble()  # col 7 and 8 contains 95% confidence interval
    }
    else if(nRow >= 2*freq && nRow <= 3*freq + 1)
    {
      if(nRow >= 2*freq) timeSeries <- ts(data = c(dataForAlgo$NumOfSalesOrder[freq], dataForAlgo$NumOfSalesOrder), frequency = freq)    
      else timeSeries <- ts(data = dataForAlgo$NumOfSalesOrder, frequency = freq)    
      tsTibble <- timeSeries %>% mstl() %>% tail(nRow) %>% as_tibble() %>% add_column(Date = DateFromDataForAlgo, .before = "Data")    
      trendStrength <- max(0, 1 - (var(tsTibble[[5]])/(var(tsTibble[[3]] + tsTibble[[5]]))))
      seasonStrength <<- max(0, 1 - (var(tsTibble[[5]])/(var(tsTibble[[4]] + tsTibble[[5]]))))    
      plotSeason <- plotSeasonDaily(tsTibble, seasonStrength)
      totalForcastStep <<- freq
      forecastedData <- stlf(timeSeries, h = totalForcastStep) %>% fortify() %>% tail(nRow + freq) %>% .[-c(7,8)] %>% as_tibble()  # col 7 and 8 contains 95% confidence interval
    }
    else if(nRow > 3*freq + 1)
    {
      tsTibble <- ts(data = dataForAlgo$NumOfSalesOrder, frequency = freq) %>% mstl() %>% as_tibble() %>% add_column(Date = DateFromDataForAlgo, .before = "Data")
      trendStrength <- max(0, 1 - (var(tsTibble[[5]])/(var(tsTibble[[3]] + tsTibble[[5]]))))
      seasonStrength <<- max(0, 1 - (var(tsTibble[[5]])/(var(tsTibble[[4]] + tsTibble[[5]]))))    
      plotSeason <- plotSeasonDaily(tsTibble, seasonStrength)
      forecastedData <- createForecastDaily(dataForAlgo$NumOfSalesOrder, freq) 
      forecastedData <- forecastedData %>% fortify() %>% .[-c(7,8)] %>% as_tibble()  # col 7 and 8 contains 95% confidence interval
    }
           
    fittedData <- tibble(Year = dataForAlgo$Year, Month = dataForAlgo$Month, Day = dataForAlgo$Day, Fitted = head(forecastedData$Fitted,NROW(dataForAlgo)))
    originalAndFittedData <- left_join(datfSplitAggByDayOU, fittedData, by = c("Year", "Month", "Day")) %>% ungroup() %>% mutate(Date = make_date(Year, Month, Day)) %>% select(Date, NumOfSalesOrder, Fitted) %>% add_column(Forecast = NA)
    if(NCOL(forecastedData) > 4) originalAndFittedData <- originalAndFittedData %>% add_column(Lo80 = NA, Hi80 = NA)
    dateCol <- lastDate + 1:(7 * ceiling(totalForcastStep/freq) + 7)
    dateCol <- head(dateCol[format(dateCol, "%A") %in% c(filteredWeekDay[[1]])], totalForcastStep)
    forecastPart <- tibble(Date = dateCol, NumOfSalesOrder = NA, Fitted = NA, Forecast = tail(forecastedData[[4]], totalForcastStep))
    if(NCOL(forecastedData) > 4) forecastPart <- forecastPart %>% add_column(Lo80 = tail(forecastedData[[5]], totalForcastStep), Hi80 = tail(forecastedData[[6]], totalForcastStep))
    originFittedForecast <- rbind(originalAndFittedData, forecastPart)
    plotForecast  <- plotForecastDaily(originFittedForecast)    
    forecastTable <- forecastPart %>% select(-NumOfSalesOrder, -Fitted) %>% mutate(WeekDay = format(Date, "%A")) %>% select(Date, WeekDay, Forecast, everything())
    
    if("SalesQty" %in% colnames(dataForAlgo)) {
      dataForAlgo$SalesQty <- ts_clean_vec(dataForAlgo$SalesQty, period = freq, lambda = NULL)       
      dataForAlgo <- dataForAlgo %>% mutate(WeekDay = format(make_date(Year, Month, Day), "%A"))
      QtySalesRatioByWday <- dataForAlgo %>% select(WeekDay, NumOfSalesOrder, SalesQty) %>% mutate(QtySalesRatio = SalesQty/NumOfSalesOrder) %>% select(WeekDay, QtySalesRatio)
      QtySalesRatioByWday <- QtySalesRatioByWday %>% group_by(WeekDay) %>% summarise_all(list(mean))
      forecastTable <- left_join(forecastTable, QtySalesRatioByWday, by = "WeekDay")
      forecastTable <- forecastTable %>% mutate(QtySalesRatio = QtySalesRatio * Forecast) %>% rename(SalesQty = "QtySalesRatio") 
    }
    forecastTable <- forecastTable %>% mutate_if(is.numeric, as.integer)   
    forecastTable[forecastTable < 0] <- 0
  return(list(plotSeason, plotForecast, forecastTable))
}

# case1: Imagine a time series data is sequential and has only 10 rows but cover weekNr 51, 52 and 01.
# Example: Mon1 Tue1 Wed1 Thu1 Fri1 Sat1 Sun1 Mon2 Tue2 Wed2
# case2: Imagine a time series data is not sequential and has only 10 rows. 
# Example: Mon1 Tue1 Wed1, Mon2 Tue2 Wed2, Mon3 Tue3 Wed3 Thu3
# case3: Imagine a time series data is not sequential and has only 10 rows and one day in a week. 
# Example: Mon1, Mon2, Mon3, Mon4, Mon5, Mon6, Mon7, Mon8, Mon9, Mon10

dailyForecastAndPlot  <- function(datfSplitAggByDay) {
   freqLimitOfAdayOvernWeeks <- 0.4
   if(NROW(datfSplitAggByDay) > 1) 
     datfSplitAggByDayOU <- outlierRemoveSalesOrder(datfSplitAggByDay) 
   else 
     datfSplitAggByDayOU <- datfSplitAggByDay 
   plotDataAndTrend <- plotDataAndTrendDaily(datfSplitAggByDayOU)
               Date <- make_date(datfSplitAggByDay$Year, datfSplitAggByDay$Month, datfSplitAggByDay$Day) 
 totWdayWithinAweek <- tibble(WeekDay = format(Date, "%A"), WeekNr = format(Date, "%V")) %>% group_by(WeekNr) %>% summarise(nDays = n())               
             nWeeks <- NROW(totWdayWithinAweek)
   totWdayByUniqueWdayinAweek <- NROW(date) / max(totWdayWithinAweek$nDays)
   if(totWdayByUniqueWdayinAweek < 2) freqLimitOfAdayOvernWeeks <- 0.3
             
    filteredWeekDay <- tibble(WeekDay = format(Date, "%A")) %>% group_by(WeekDay) %>% summarise(freqRateOfAdayOvernWeeks = n()/nWeeks) 
    filteredWeekDay <- filteredWeekDay %>% filter(freqRateOfAdayOvernWeeks > freqLimitOfAdayOvernWeeks) %>% select(WeekDay)
           firstRow <- head(datfSplitAggByDay,1) 
            lastRow <- tail(datfSplitAggByDay,1)
    dateIndexSeries <- seq(from= make_date(firstRow[[1,1]], firstRow[[1,2]], firstRow[[1,3]]), to= make_date(lastRow[[1,1]], lastRow[[1,2]], lastRow[[1,3]]), by="day") %>% as.data.frame()
    dateIndexSeries <- filter(dateIndexSeries, format(dateIndexSeries[,1], "%A") %in% c(filteredWeekDay[[1]]))
          emptyDatf <- tibble(Year = year(dateIndexSeries[,1]), Month = month(dateIndexSeries[,1]), Day = day(dateIndexSeries[,1])) # NumOfSalesOrder = 0, SalesQty = 0)
         for(colNo in 4:NCOL(datfSplitAggByDay)) 
         {
           emptyDatf <- emptyDatf %>% add_column(data = -1, .name_repair = make.unique)
           setnames(emptyDatf, c(colnames(emptyDatf)[colNo]), c(colnames(datfSplitAggByDay)[colNo]))     
         }    
       dataForAlgo <- rows_update(emptyDatf, datfSplitAggByDay, by=c("Year", "Month", "Day"), unmatched = "ignore")
       dataForAlgo <- dataForAlgo %>% na_if(-1) # %>% na_kalman(dataForAlgo) 
       dataForAlgo <- dataForAlgo %>% mutate(WeekDay = format(Date, "%A")) %>% filter(WeekDay %in% filteredWeekDay[[1]])
              freq <- NROW(filteredWeekDay)
      forecastInfo <- createForecastPlot(freq, filteredWeekDay, dataForAlgo, datfSplitAggByDayOU, make_date(lastRow$Year, lastRow$Month, lastRow$Day)) 
      tagList(
        tags$h5(tags$b("Black and Blue Line Represents Original and Avaerage Data.")),
        renderPlot(plotDataAndTrend),
        if(freq != NROW(dataForAlgo)) tags$h5( tags$b(
          "Original Data - Average Data = Seasonality,    Seasonal Strength = ", round(seasonStrength, digits = 4)," out of 1.0"        
        )),
        if(freq != NROW(dataForAlgo)) renderPlot(forecastInfo[[1]]),
        tags$h5(tags$b("Black and Blue Line Represent Original and Forecast Data.")),
        renderPlot(forecastInfo[[2]]),
        renderDataTable(forecastInfo[[3]])
      )
#  return(list(plotDataAndTrend, plotSeason = forecastInfo[[1]], plotForecast = forecastInfo[[2]], forecastTable = forecastInfo[[3]]))
}
