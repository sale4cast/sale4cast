createNumericForecastPlot <- function(NumberOfCustomer, colName)
{
  freq <- findfrequency(NumberOfCustomer)

  #  freq <- 1
  #  NumberOfCustomer <- 5
  
  #freq <- 2
  #NumberOfCustomer <- c(5,3)
  #NumberOfCustomer <- c(7,4,5,2,6,3,7,6,9,8)
  
  #freq <- 3
  #NumberOfCustomer <- c(7,4,5,2,6)
  #NumberOfCustomer <- c(7,4,5,2,6,3,7,6,9,8)
  
  dailyForecast <- NULL
  extraStep <- 0
  NumberOfCustomer <- tail(NumberOfCustomer, 120)
  datfSplitAggByDay <- tibble(TimeIndex = 1:NROW(NumberOfCustomer), NumberOfCustomer = NumberOfCustomer)
  datfSplitAggByDayOU <- outlierRemoveSalesOrder(datfSplitAggByDay) 
  plotDataAndTrend <- ggplot(data = datfSplitAggByDayOU, mapping = aes(x = TimeIndex, y = NumberOfCustomer)) + 
    geom_line(size = 1.05, color = "black") +
    geom_smooth(span = 0.3) +
    labs(x = "TimeIndex", y = colName)
  
  if(NROW(NumberOfCustomer) <= 5 || NROW(NumberOfCustomer) <= 2*freq) 
  {
    forecastedData <- snaive(ts(NumberOfCustomer, frequency = freq), h = freq)
    forecastedData <- forecastedData %>% fortify()
    forecastedData <- forecastedData %>% mutate(Index = 1:NROW(forecastedData))
    forecastedDataDetails <- forecastedData %>% tail(NROW(forecastedData) - NROW(NumberOfCustomer))    
  }
  else 
  {
    tsTibble <- ts(NumberOfCustomer, frequency = freq) %>% mstl() %>% as_tibble() %>% add_column(TimeIndex = 1:NROW(NumberOfCustomer) , .before = "Data")
    plotElse <- ggplot(data = tsTibble, mapping = aes_string(x = colnames(tsTibble)[1], y = colnames(tsTibble)[4])) + 
      geom_line(size = 1.05, alpha = 1/3, color = "black") +
      labs(x = "TimeIndex", y = colName) + 
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
      )
        
    forecastedData <- createForecastDaily(NumberOfCustomer, freq)
    forecastedData <- forecastedData[[1]]
    totalForcastStep <- forecastedData[[2]]
    forecastedData <- forecastedData %>% fortify()    
    forecastedData <- forecastedData %>% mutate(Index = 1:NROW(forecastedData))
    forecastedDataDetails <- forecastedData %>% tail(NROW(forecastedData) - NROW(NumberOfCustomer))
  }
  
  if(NCOL(forecastedData) > 4) geom_rib <- geom_ribbon(aes(ymin = `Lo 80`, ymax = `Hi 80`), fill = "steelblue2", alpha = 1/5) 
  else geom_rib <- NULL
  
  forecastPlot <- ggplot(data = forecastedData, mapping = aes(x = Index)) + 
    geom_rib +
    geom_line(mapping = aes(y = Data), size = 1, color = "black") + 
    geom_point(mapping = aes(y = Data), size = 1, color = "grey") + 
    geom_line(mapping = aes(y = Fitted), color = "blue") +
    geom_line(mapping = aes(y = `Point Forecast`), color = "blue", size = 1) + 
    labs(x = "TimeIndex", y = colName)
  
  # tagList(
  #   tags$h5(tags$b("Black and Blue Line represents Original and Avaerage Data.")),
  #   renderPlot(plotDataAndTrend),
  #   if(NROW(NumberOfCustomer) >= (2*freq + 1)) tags$h5( tags$b("Original Data - Average Data = Seasonality ")),
  #   if(NROW(NumberOfCustomer) >= (2*freq + 1)) renderPlot(plotElse),
  #   tags$h5(tags$b("Black and Blue Line represent Original and Forecast Data.")),
  #   renderPlot(forecastPlot),
  #   renderDataTable(forecastedDataDetails %>% select(-Data, -Fitted) %>% mutate_if(is.numeric, as.integer))  
  # )
  forecastTable <- forecastedDataDetails %>% select(-Data, -Fitted) %>% mutate_if(is.numeric, as.integer)
  return(list(renderPlot(forecastPlot), renderDataTable(forecastTable)))
}

numericInsightAndForecastPlot  <- function(splitedDatf) {
  rplot = list()
  rplot <- lapply(1:NCOL(splitedDatf), function(rowIndex){
    splitedDatfC <- splitedDatf %>% .[rowIndex] %>% na.omit()
    createNumericForecastPlot(splitedDatfC[[1]], colnames(splitedDatfC)[[1]])
  })
  rplot
}