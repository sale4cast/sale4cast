createDailyForecast <- function(tSeries, freq){
  #    tSeries %>% diff() %>% ur.kpss() -> tval
  #    tval@teststat   
  trainTseries <- head(tSeries, NROW(tSeries) - freq)
  ETS <- forecast(ets(trainTseries), h=freq)
  ARIMA <- forecast(auto.arima(trainTseries, lambda=0, biasadj=TRUE), h=freq)
  STL <- stlf(trainTseries, lambda=0, h=freq, biasadj=TRUE) # if data is seasonal
  NNAR <- forecast(nnetar(trainTseries), h=freq)
  a <- 10
  algoAccu <- c(Algorithm = 1, accuracy(ETS, tSeries)[2,1:7])    
  algoAccu %>% rbind(c(Algorithm = 2, accuracy(ARIMA, tSeries)[2,1:7])) -> algoAccu
  algoAccu %>% rbind(c(Algorithm = 3, accuracy(STL, tSeries)[2,1:7])) -> algoAccu
  algoAccu %>% rbind(c(Algorithm = 4, accuracy(NNAR, tSeries)[2,1:7])) -> algoAccu
  algoAccu %>% as_tibble() -> algoAccu   
  algoAccu %>% filter(MAPE == min(algoAccu[["MAPE"]])) %>% select(Algorithm) -> algoNo
  
  if(algoNo == 1)
    tSeries %>% ets() %>% forecast(h = freq) -> dailyForecast
  if(algoNo == 2)
    tSeries %>% auto.arima() %>% forecast(h = freq) -> dailyForecast
  if(algoNo == 3)
    tSeries %>% stlf(h = freq) -> dailyForecast
  if(algoNo == 4)
    tSeries %>% nnetar() %>% forecast(h = freq) -> dailyForecast
  return(dailyForecast)
}

plotDailyForecast <- function(dateIndexSeries, freq, filteredWeekDay, dataForAlgo, nRow){
  tSeries <- ts(data <- dataForAlgo[["NumberOfCustomer"]], frequency = freq)
  tSeries %>% mstl() %>% as_tibble() %>% add_column(Date = tail(dateIndexSeries[,1], (nRow - (nRow %% freq))), .before = "Data") -> tSeriesTibble    
  trendStrength <- max(0, 1 - (var(tSeriesTibble[[5]])/(var(tSeriesTibble[[3]] + tSeriesTibble[[5]]))))
  seasonStrength <- max(0, 1 - (var(tSeriesTibble[[5]])/(var(tSeriesTibble[[4]] + tSeriesTibble[[5]]))))    
  nday <- ceiling(NROW(dataForAlgo)/freq) * 7 + freq
  
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
      axis.text.y =  element_text(size = rel(1.3), face = "bold"),
#      plot.background = element_rect(fill = "lightblue3", colour = NA),
#      panel.background = element_rect(fill = "lightblue", colour = NA),
#      axis.text = element_text(colour = "linen"),
#      axis.title = element_text(colour = "linen")      
    ) 
  
  dateCol <- make_date(dataForAlgo[[1,1]],dataForAlgo[[1,2]],dataForAlgo[[1,3]]) + 0:(nday-1)
  dateCol <- dateCol[format(dateCol, "%A") %in% c(filteredWeekDay[[1]])]
  createDailyForecast(tSeries, freq) %>% fortify() %>% as_tibble() -> tSeriesTibble # tb should be replaced by tSeriesTibble
  tSeriesTibble <- tSeriesTibble[-c(7,8)]
  tSeriesTibble[[1]] <- dateCol
  forecastedData <- rename(tSeriesTibble, Date = "Index", Original_Data = Data, Model_Data = Fitted, Forecast = "Point Forecast", Lo80 = "Lo 80", Hi80 = "Hi 80")
  
  plotForecast <- ggplot(data = forecastedData, mapping = aes(x = Date)) + 
    geom_line(mapping = aes(y = Original_Data), size = 1.05, alpha = 1/3) + 
    geom_point(mapping = aes(y = Original_Data, color = format(Date, "%A")), size = 2.3) + 
    geom_line(mapping = aes(y = Model_Data), color = "blue", size = 1.05, alpha = 1/3) +
    geom_ribbon(aes(ymin = Lo80, ymax = Hi80), fill = "steelblue2", alpha = 1/5) +
    geom_line(mapping = aes(y = Forecast), color = "blue", size = 1.05, alpha = 1/3) + 
    geom_point(mapping = aes(y = Forecast, color = format(Date, "%A")), size = 2.3) +
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
  
  forecastTable <- forecastedData %>% tail(freq) %>% as_tibble() %>% select(Date, Forecast, Lo80, Hi80) %>% mutate(Date = format(Date, "%Y-%b-%d, %A"))   
  return(list(plotSeason, plotForecast, forecastTable))
}

createDailyAnalysis <- function(datfSplitAggByDay){
  plotDataAndTrend <- ggplot(data = datfSplitAggByDay, mapping = aes(x = make_date(Year, Month, Day), y = NumberOfCustomer)) + 
    geom_line(size = 1.05, alpha = 1/3) + 
    geom_point(mapping = aes(color = WeekDay), size = 2) + 
    scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2))) + 
    geom_smooth(span = 0.2, se = FALSE) +
    scale_x_date(date_labels = "%d-%b-%Y")  + 
    labs(x = "Date", y = "Number of Sales Order", title = "Sales Order is Represented Over Date. Black and Blue line represents an original and avaerage data.") + 
    theme( legend.position = "top",
           plot.title = element_text(size = rel(1.2), face = "bold"), 
           legend.title = element_text(size = rel(1),face = "bold"), 
           axis.title.x = element_text(size = rel(1.15), face = "bold"),
           axis.title.y = element_text(size = rel(1), face = "bold"),
           axis.text.x =  element_text(size = rel(1.3), face = "bold"),
           axis.text.y =  element_text(size = rel(1.3), face = "bold")
    )
  
  nWeeks <- NROW(unique(parse_number(datfSplitAggByDay[["WeekNr"]])))
  datfSplitAggByDay %>% group_by(WeekDay) %>% summarise(FreqRateOfAdayOvernWeeks = 100*n()/nWeeks) %>% filter(FreqRateOfAdayOvernWeeks > 40) %>% select(WeekDay) -> filteredWeekDay
  firstDay <- make_date(datfSplitAggByDay[[1,1]], datfSplitAggByDay[[1,2]], datfSplitAggByDay[[1,3]])
  lastRow <- tail(datfSplitAggByDay,1)    
  lastDay <- make_date(lastRow[[1,1]], lastRow[[1,2]], lastRow[[1,3]])
  Date <- seq(from=firstDay, to=lastDay, by="day")
  dateIndexSeries <- as.data.frame(Date)
  dateIndexSeries <- filter(dateIndexSeries, format(dateIndexSeries[,1], "%A") %in% c(filteredWeekDay[[1]]))
  emptyDatf <- tibble(Year = year(dateIndexSeries[,1]), Month = month(dateIndexSeries[,1]), Day = day(dateIndexSeries[,1]), WeekDay = "a", WeekNr = "a", NumberOfCustomer = 0, salesQty = 0)
  rows_update(emptyDatf, datfSplitAggByDay, by=c("Year", "Month", "Day")) -> dataForAlgo
  freq <- NROW(filteredWeekDay)
  nRow <- NROW(dataForAlgo)
  dataForAlgo <- tail(dataForAlgo, (nRow - (nRow %% freq)))
  plotSeason <- plotDailyForecast(dateIndexSeries, freq, filteredWeekDay, dataForAlgo, nRow)[[1]]
  plotForecast <- plotDailyForecast(dateIndexSeries, freq, filteredWeekDay, dataForAlgo, nRow)[[2]]
  forecastTable <- plotDailyForecast(dateIndexSeries, freq, filteredWeekDay, dataForAlgo, nRow)[[3]]
  return(list(plotDataAndTrend, plotSeason, plotForecast, forecastTable))
}
