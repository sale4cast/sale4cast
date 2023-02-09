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
    labs(x = "Date", y = "Number of Sales Order", title = paste("Original Data - Average Data = Seasonality      Seasonal Strength = ", round(seasonStrength, digits = 4)," out of 1.0")) + 
    theme(
      legend.position = "top",
      plot.title = element_text(size = rel(1.2), face = "bold"), 
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
    ) 
  return(plotSeason)
}

plotForecastDaily <- function(forecastedData) {
  if(NCOL(forecastedData) > 4) geom_rib <- geom_ribbon(aes(ymin = Lo80, ymax = Hi80), fill = "steelblue2", alpha = 1/5)
  else geom_rib <- NULL
  plotForecast <- ggplot(data = forecastedData, mapping = aes(x = Date)) + 
    geom_line(mapping = aes(y = Original_Data), size = 1.05, alpha = 1/3) + 
    geom_point(mapping = aes(y = Original_Data, color = format(Date, "%A")), size = 2.3) + 
    geom_line(mapping = aes(y = Model_Data), color = "blue", size = 1.05, alpha = 1/3) +
    geom_rib +
    geom_line(mapping = aes(y = Forecast_Data), color = "blue", size = 1.05, alpha = 1/3) + 
    geom_point(mapping = aes(y = Forecast_Data, color = format(Date, "%A")), size = 2.3) +
    scale_x_date(date_labels = "%d-%b-%Y")  + 
    scale_color_discrete(guide = guide_legend(title = "WeekDay", override.aes = list(alpha = 1, size = 2))) +
    labs(x = "Date", y = "Number of Sales Order", title = "Black and Blue line represent an original and forecast data.") + 
    theme(
      legend.position = "top",
      plot.title = element_text(size = rel(1.2), face = "bold"), 
      legend.title = element_text(size = rel(1),face = "bold"), 
      axis.title.x = element_text(size = rel(1.15), face = "bold"),
      axis.title.y = element_text(size = rel(1), face = "bold"),
      axis.text.x =  element_text(size = rel(1.3), face = "bold"),
      axis.text.y =  element_text(size = rel(1.3), face = "bold")
    )
  return(plotForecast)
}

createForecastDaily <- function(tSeries, freq) {
  #    tSeries %>% diff() %>% ur.kpss() -> tval
  #    tval@teststat   
  trainTseries <- head(tSeries, NROW(tSeries) - freq)
           ETS <- forecast(ets(trainTseries), h=freq)
         ARIMA <- forecast(auto.arima(trainTseries, lambda=0, biasadj=TRUE), h=freq)
           STL <- stlf(trainTseries, lambda=0, h=freq, biasadj=TRUE) # if data is seasonal
          NNAR <- forecast(nnetar(trainTseries), h=freq)

  algoAccu <- c(Algorithm = 1, accuracy(ETS, tSeries)[2,1:7])    
  algoAccu <- algoAccu %>% rbind(c(Algorithm = 2, accuracy(ARIMA, tSeries)[2,1:7])) 
  algoAccu <- algoAccu %>% rbind(c(Algorithm = 3, accuracy(STL, tSeries)[2,1:7]))
  algoAccu <- algoAccu %>% rbind(c(Algorithm = 4, accuracy(NNAR, tSeries)[2,1:7]))
  algoAccu <- algoAccu %>% as_tibble()   
  algoNo   <- algoAccu %>% filter(MAPE == min(algoAccu[["MAPE"]])) %>% select(Algorithm)
  
  if(algoNo == 1)
    dailyForecast <- tSeries %>% ets() %>% forecast(h = freq)
  if(algoNo == 2)
    dailyForecast <- tSeries %>% auto.arima() %>% forecast(h = freq)
  if(algoNo == 3)
    dailyForecast <- tSeries %>% stlf(h = freq)
  if(algoNo == 4)
    dailyForecast <- tSeries %>% nnetar() %>% forecast(h = freq)
  return(dailyForecast)
}

createForecastPlot <- function(dateIndexSeries, freq, filteredWeekDay, dataForAlgo, nRow) {
         tSeries <- ts(data = dataForAlgo$NumOfSalesOrder, frequency = freq)
   tSeriesTibble <- tSeries %>% mstl() %>% as_tibble() %>% add_column(Date = tail(dateIndexSeries[,1], (nRow - (nRow %% freq))), .before = "Data")    
   trendStrength <- max(0, 1 - (var(tSeriesTibble[[5]])/(var(tSeriesTibble[[3]] + tSeriesTibble[[5]]))))
  seasonStrength <- max(0, 1 - (var(tSeriesTibble[[5]])/(var(tSeriesTibble[[4]] + tSeriesTibble[[5]]))))    
            nday <- ceiling(NROW(dataForAlgo)/freq) * 7 + freq
      plotSeason <- plotSeasonDaily(tSeriesTibble, seasonStrength)
         dateCol <- make_date(dataForAlgo[[1,1]],dataForAlgo[[1,2]],dataForAlgo[[1,3]]) + 0:(nday-1)
         dateCol <- dateCol[format(dateCol, "%A") %in% c(filteredWeekDay[[1]])]
  forecastedData <- createForecastDaily(tSeries, freq) %>% fortify() %>% .[-c(7,8)] %>% as_tibble()  # col 7 and 8 contains 95% confidence interval
           cname <- c("Date", "Original_Data", "Model_Data", "Forecast_Data", "Lo80", "Hi80")
           
       forecastedData[[1]] <- dateCol
  colnames(forecastedData) <- cname[1:NCOL(forecastedData)]  
  
   plotForecast  <- plotForecastDaily(forecastedData)    
   forecastTable <- forecastedData %>% tail(freq) %>% as_tibble() %>% select(-c(Original_Data, Model_Data))
   forecastTable <- forecastTable %>% mutate(WeekDay = format(Date, "%A"))
   
   if("SalesQty" %in% colnames(dataForAlgo)) {
     dataForAlgo <- dataForAlgo %>% mutate(WeekDay = format(make_date(Year, Month, Day), "%A"))
     QtySalesRatioByWday <- dataForAlgo %>% select(WeekDay, NumOfSalesOrder, SalesQty) %>% mutate(QtySalesRatio = SalesQty/NumOfSalesOrder) %>% select(WeekDay, QtySalesRatio)
     QtySalesRatioByWday <- QtySalesRatioByWday %>% group_by(WeekDay) %>% summarise_all(list(mean))
     forecastTable <- left_join(forecastTable, QtySalesRatioByWday, by = "WeekDay")
     forecastTable <- forecastTable %>% mutate(QtySalesRatio = QtySalesRatio * Forecast_Data) %>% rename(SalesQty = "QtySalesRatio") %>% select(Date, WeekDay, everything())   
   }
   forecastTable <- forecastTable %>% mutate_if(is.numeric, as.integer)   
  
   forecastTable[forecastTable < 0] <- 0
  return(list(plotSeason, plotForecast, forecastTable))
}

dailyForecastAndPlot  <- function(datfSplitAggByDay) {
  plotDataAndTrend <- plotDataAndTrendDaily(outlierRemoveSalesOrder(datfSplitAggByDay))
              Date <- make_date(datfSplitAggByDay$Year, datfSplitAggByDay$Month, datfSplitAggByDay$Day) 
            WeekNr <- format(Date, "%V")
            nWeeks <- NROW(unique(WeekNr))
   filteredWeekDay <- tibble(WeekDay = format(Date, "%A")) %>% group_by(WeekDay) %>% summarise(FreqRateOfAdayOvernWeeks = 100*n()/nWeeks) %>% filter(FreqRateOfAdayOvernWeeks > 40) %>% select(WeekDay)
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
       dataForAlgo <- rows_update(emptyDatf, datfSplitAggByDay, by=c("Year", "Month", "Day"))
       dataForAlgo <- dataForAlgo %>% na_if(-1) # %>% na_kalman(dataForAlgo)    
              freq <- NROW(filteredWeekDay)
              nRow <- NROW(dataForAlgo)
       dataForAlgo <- tail(dataForAlgo, (nRow - (nRow %% freq)))
       dataForAlgo$NumOfSalesOrder <- ts_clean_vec(dataForAlgo$NumOfSalesOrder, period = freq, lambda = NULL)
       if("SalesQty" %in% colnames(dataForAlgo)) dataForAlgo$SalesQty <- ts_clean_vec(dataForAlgo$SalesQty, period = freq, lambda = NULL)       
      forecastInfo <- createForecastPlot(dateIndexSeries, freq, filteredWeekDay, dataForAlgo, nRow) 
  return(list(plotDataAndTrend, plotSeason = forecastInfo[[1]], plotForecast = forecastInfo[[2]], forecastTable = forecastInfo[[3]]))
}
