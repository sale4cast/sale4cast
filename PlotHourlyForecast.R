toForecast <- function(dataForAlgo, colIndicator) {
  dataForAlgo %>% group_by(Hour) %>% summarise(count = n()) -> hourList  
  if("SalesQty" %in% colnames(dataForAlgo)) {
    QtySalesRatioByHour <- dataForAlgo %>% select(Hour, NumOfSalesOrder, SalesQty) %>% mutate(QtySalesRatio = SalesQty/NumOfSalesOrder) %>% select(Hour, QtySalesRatio)
    QtySalesRatioByHour <- QtySalesRatioByHour %>% group_by(Hour) %>% summarise_all(list(mean))
  } 
  freq <- NROW(hourList)

  if(NROW(dataForAlgo) == freq)
  {
    tSeries <- ts(data = dataForAlgo$NumOfSalesOrder, frequency = freq)    
    forecastedData <- snaive(tSeries, h = freq) %>% fortify() %>% tail(freq)
  }
  else if(NROW(dataForAlgo) == 2*freq)
  {
    tSeries <- ts(data = c(dataForAlgo$NumOfSalesOrder[freq], dataForAlgo$NumOfSalesOrder), frequency = freq)      
    forecastedData <- stlf(tSeries, h = freq) %>% fortify() %>% tail(freq)
  }
  else if(NROW(dataForAlgo) > 2*freq)
  {
    tSeries <- ts(dataForAlgo$NumOfSalesOrder, frequency = freq)
    #  createDailyForecast(tSeries, freq) %>% fortify() %>% tail(freq) -> forecastedData  
    forecastedData <- tSeries %>% stlf(h = freq) %>% fortify() %>% tail(freq)
  }
  
  forecastedData[-c(1,2,3,7,8)] -> forecastedData # colnames(forecastedData) = "Index", "Data", "Fitted", "Point Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95" 
  forecastedData %>% add_column(Hour = hourList$Hour, .before = "Point Forecast") -> forecastedData
  forecastedData <- rename(forecastedData, NumberOfSalesOrder = "Point Forecast", Lo80 = "Lo 80", Hi80 = "Hi 80")
  if("SalesQty" %in% colnames(dataForAlgo)) 
    left_join(forecastedData, QtySalesRatioByHour, by = "Hour") %>% mutate(SalesQty = NumberOfSalesOrder * QtySalesRatio) %>% select(-QtySalesRatio) -> forecastedData  
  forecastedData %>% mutate_if(is.double, as.integer) -> forecastedData   
  forecastedData[forecastedData < 0] <- 0
  return(forecastedData)
}

forecastPlot <- function(forecastedData, colIndicator, dayName) {
  
  if("SalesQty" %in% colnames(forecastedData)) geom_p <- geom_point(aes(size = SalesQty), alpha = 1, color = "#F49F05")
  else geom_p <- geom_point(size = 3, color = "red")
  
  pl <- ggplot(data = forecastedData, mapping = aes(x = Hour, y = NumberOfSalesOrder)) + 
    geom_ribbon(aes(ymin = Lo80, ymax = Hi80), fill = "darkgrey", alpha = 1/5) + geom_p + geom_point(color = "red") + geom_line(color = "orange", linetype = 2, alpha = 2/3) +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22), 
                       label = c("0.00H", "2.00H", "4.00H", "6.00H", "8.00H", "10.00H", "12.00H", "14.00H", "16.00H", "18.00H", "20.00H", "22.00H")) +
    labs(x = "Hour", y = "Number of Sales Order", title = paste("Sales Forecast - Coming", dayName)) + 
    theme(
      plot.title = element_text(size = rel(1.2), face = "bold"), 
      legend.title = element_text(size = rel(1),face = "bold"), 
      legend.position = "top",
      axis.title.x = element_text(size = rel(1.15), face = "bold"),
      axis.title.y = element_text(size = rel(1), face = "bold"),
      axis.text.x =  element_text(size = rel(1.3), face = "bold"),
      axis.text.y =  element_text(size = rel(1.3), face = "bold")
    ) +
    guides(x = guide_axis(angle = 30)) 
  return(pl)
}

plotHourlyForecast <- function(datfSplitAggByHour, colIndicator) {
  datfSplitAggByHour %>% group_by(WeekDay) %>% count() -> WeekDay 
  datfSplitAggByHour %>% ungroup() -> datfSplitAggByHour
  ForecastMonday <- ForecastTuesday <- ForecastWednesday <- ForecastThursday <- ForecastFriday <- ForecastSaturday <- ForecastSunday <- NULL
  forecastedDataMon <- forecastedDataTue <- forecastedDataWed <- forecastedDataThu <- forecastedDataFri <- forecastedDataSat <- forecastedDataSun <- NULL
  WeekDay %>% filter(WeekDay %in% c("Thursday", "Monday", "Tuesday", "Wednesday", "Friday", "Sunday", "Saturday")) -> WeekDay
  a <- 10
  if("Monday" %in% WeekDay[[1]]) {
    datfSplitAggByHour %>% filter(WeekDay == "Monday") %>% select(-WeekDay) -> datfMonAggByHourM
    datfMonAggByHourM <- dataImputation(datfMonAggByHourM)
    forecastedDataMon <- toForecast(datfMonAggByHourM, colIndicator)
       ForecastMonday <- forecastPlot(forecastedDataMon, colIndicator, "Monday")   
  } 
  if("Tuesday" %in% WeekDay[[1]]) {
    datfSplitAggByHour %>% filter(WeekDay == "Tuesday") %>% select(-WeekDay) %>% dataImputation() -> datfTueAggByHourM
    datfTueAggByHourM <- dataImputation(datfTueAggByHourM)
    forecastedDataTue <- toForecast(datfTueAggByHourM, colIndicator)
      ForecastTuesday <- forecastPlot(forecastedDataTue, colIndicator, "Tuesday")   
  } 
  if("Wednesday" %in% WeekDay[[1]]) {
    datfSplitAggByHour %>% filter(WeekDay == "Wednesday") %>% select(-WeekDay) %>% dataImputation() -> datfWedAggByHourM
    datfWedAggByHourM <- dataImputation(datfWedAggByHourM)
    forecastedDataWed <- toForecast(datfWedAggByHourM, colIndicator)
    ForecastWednesday <- forecastPlot(forecastedDataWed, colIndicator, "Wednesday")   
  } 
  if("Thursday" %in% WeekDay[[1]]) {
    datfSplitAggByHour %>% filter(WeekDay == "Thursday") %>% select(-WeekDay) %>% dataImputation() -> datfThuAggByHourM
    datfThuAggByHourM <- dataImputation(datfThuAggByHourM)
    forecastedDataThu <- toForecast(datfThuAggByHourM, colIndicator)
     ForecastThursday <- forecastPlot(forecastedDataThu, colIndicator, "Thursday")   
  } 
  if("Friday" %in% WeekDay[[1]]) {
    datfSplitAggByHour %>% filter(WeekDay == "Friday") %>% select(-WeekDay) %>% dataImputation() -> datfFriAggByHourM
    datfFriAggByHourM <- dataImputation(datfFriAggByHourM)
    forecastedDataFri <- toForecast(datfFriAggByHourM, colIndicator)
       ForecastFriday <- forecastPlot(forecastedDataFri, colIndicator, "Friday")   
  } 
  if("Saturday" %in% WeekDay[[1]]) {
    datfSplitAggByHour %>% filter(WeekDay == "Saturday") %>% select(-WeekDay) %>% dataImputation() -> datfSatAggByHourM
    datfSatAggByHourM <- dataImputation(datfSatAggByHourM)
    forecastedDataSat <- toForecast(datfSatAggByHourM, colIndicator)
     ForecastSaturday <- forecastPlot(forecastedDataSat, colIndicator, "Saturday")   
  } 
  if("Sunday" %in% WeekDay[[1]]) {
    datfSplitAggByHour %>% filter(WeekDay == "Sunday") %>% select(-WeekDay) %>% dataImputation() -> datfSunAggByHourM
    datfSunAggByHourM <- dataImputation(datfSunAggByHourM)
    forecastedDataSun <- toForecast(datfSunAggByHourM, colIndicator)
    ForecastSunday <- forecastPlot(forecastedDataSun, colIndicator, "Sunday")   
  }    
  
  plotVec <- list(ForecastMonday, ForecastTuesday, ForecastWednesday, ForecastThursday, ForecastFriday, ForecastSaturday, ForecastSunday)    
  ForecastedDataVec <- list(forecastedDataMon, forecastedDataTue, forecastedDataWed, forecastedDataThu, forecastedDataFri, forecastedDataSat, forecastedDataSun)        
  lastRow <- tail(datfSplitAggByHour,1)
  firstDay <- make_date(datfSplitAggByHour[[1,1]], datfSplitAggByHour[[1,2]], datfSplitAggByHour[[1,3]])
  lastDay <- make_date(lastRow[[1,1]], lastRow[[1,2]], lastRow[[1,3]])
  forecastFirstDay   = lastDay + 1
  forecastSecondDay  = lastDay + 2
  forecastThirdDay   = lastDay + 3
  forecastFourthDay  = lastDay + 4
  forecastFifthDay   = lastDay + 5
  forecastSixthDay   = lastDay + 6
  forecastSeventhDay = lastDay + 7
  dateRange <- paste("Historical Data From ", firstDay,  "To", lastDay,".")
  pictureTable <- tagList(
    tags$h5( tags$b(
      dateRange, "Last Day of Observation - ", tags$span(style="color:red", format(lastDay, "%A"))        
    )),
    if (format(forecastFirstDay, "%A") %in% WeekDay[[1]]) renderPlot(plotVec[[as.numeric(format(forecastFirstDay, "%u"))]]),
    if (format(forecastFirstDay, "%A") %in% WeekDay[[1]]) renderDataTable(ForecastedDataVec[[as.numeric(format(forecastFirstDay, "%u"))]], options = list(pageLength = 6, info = FALSE)),
    
    if (format(forecastSecondDay, "%A") %in% WeekDay[[1]]) renderPlot(plotVec[[as.numeric(format(forecastSecondDay, "%u"))]]),
    if (format(forecastSecondDay, "%A") %in% WeekDay[[1]]) renderDataTable(ForecastedDataVec[[as.numeric(format(forecastSecondDay, "%u"))]], options = list(pageLength = 6, info = FALSE)),
    
    if (format(forecastThirdDay, "%A") %in% WeekDay[[1]]) renderPlot(plotVec[[as.numeric(format(forecastThirdDay, "%u"))]]),
    if (format(forecastThirdDay, "%A") %in% WeekDay[[1]]) renderDataTable(ForecastedDataVec[[as.numeric(format(forecastThirdDay, "%u"))]], options = list(pageLength = 6, info = FALSE)),
    
    if (format(forecastFourthDay, "%A") %in% WeekDay[[1]]) renderPlot(plotVec[[as.numeric(format(forecastFourthDay, "%u"))]]),
    if (format(forecastFourthDay, "%A") %in% WeekDay[[1]]) renderDataTable(ForecastedDataVec[[as.numeric(format(forecastFourthDay, "%u"))]], options = list(pageLength = 6, info = FALSE)),
    
    if (format(forecastFifthDay, "%A") %in% WeekDay[[1]]) renderPlot(plotVec[[as.numeric(format(forecastFifthDay, "%u"))]]),
    if (format(forecastFifthDay, "%A") %in% WeekDay[[1]]) renderDataTable(ForecastedDataVec[[as.numeric(format(forecastFifthDay, "%u"))]], options = list(pageLength = 6, info = FALSE)),
    
    if (format(forecastSixthDay, "%A") %in% WeekDay[[1]]) renderPlot(plotVec[[as.numeric(format(forecastSixthDay, "%u"))]]),
    if (format(forecastSixthDay, "%A") %in% WeekDay[[1]]) renderDataTable(ForecastedDataVec[[as.numeric(format(forecastSixthDay, "%u"))]], options = list(pageLength = 6, info = FALSE)),
    
    if (format(forecastSeventhDay, "%A") %in% WeekDay[[1]]) renderPlot(plotVec[[as.numeric(format(forecastSeventhDay, "%u"))]]),             
    if (format(forecastSeventhDay, "%A") %in% WeekDay[[1]]) renderDataTable(ForecastedDataVec[[as.numeric(format(forecastSeventhDay, "%u"))]], options = list(pageLength = 6, info = FALSE)),      
  )
  return(pictureTable)
}  

