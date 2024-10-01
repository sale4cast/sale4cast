hourlyInsight <- function(datfSplitAggByHour)
{
  oriData <- oriDataByHour <- oriDataByHourAndWeekDay <- customerVsQty <- 0
  colName <- colnames(datfSplitAggByHour) %>% .[-c(1,2,3,4)]
  if(NCOL(datfSplitAggByHour) == 5) names(datfSplitAggByHour)[5] <- c("NumberOfCustomer")
  else if(NCOL(datfSplitAggByHour) == 6) names(datfSplitAggByHour)[c(5,6)] <- c("NumberOfCustomer","SalesQty")
  datfSplitAggByHour <- datfSplitAggByHour %>% mutate(DateTime = make_datetime(Year, Month, Day, Hour), WeekDay = format(make_date(Year, Month, Day), "%A"))
  nRowBefore <- NROW(datfSplitAggByHour)  
  
  if(NROW(datfSplitAggByHour) > 1 && ("NumberOfCustomer" %in% colnames(datfSplitAggByHour))) 
    datfSplitAggByHour <- outlierRemoveSalesOrder(datfSplitAggByHour)
  numOfOutlier <- nRowBefore - NROW(datfSplitAggByHour)

  if(numOfOutlier > 0) captionText = paste("( ", numOfOutlier, " outliers omited )")
  else  captionText = NULL
  cols <- c("Sunday" = "red", "Monday" = "#197AEB", "Tuesday" = "#49be25", "Wednesday" = "#FAAF08", "Thursday" = "#F606C9", "Friday" = "#7C7F82", "Saturday" = "#473436")  
  oriData <- ggplot(data = datfSplitAggByHour) + 
    geom_point(mapping = aes(x = DateTime, y = NumberOfCustomer, color = WeekDay), na.rm = TRUE) + 
    scale_x_datetime(labels = date_format("%d-%m-%y %H:%M:%S"))  + 
    labs(x = "DateTime", y = colName[1], title = "Data over Date and Time", caption = captionText) + 
    scale_colour_manual(values = cols) + 
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
  
  oriDataByHour <- ggplot(data = datfSplitAggByHour, mapping = aes(x = Hour, y = NumberOfCustomer)) + 
    geom_point(mapping = aes(color = WeekDay), na.rm = TRUE) +
    stat_summary(fun.min = min, fun.max = max, geom = "linerange", color = "grey") +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22), 
                       label = c("0.00H", "2.00H", "4.00H", "6.00H", "8.00H", "10.00H", "12.00H", "14.00H", "16.00H", "18.00H", "20.00H", "22.00H")) +
    labs(x = "Hour", y = paste(colName[1], " [Max-Min Range]"), title = "Data is Arranged over Hour" ) + 
    scale_colour_manual(values = cols) + 
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
  
  oriDataByHourAndWeekDay <- ggplot(data = datfSplitAggByHour, mapping = aes(x = Hour, y = NumberOfCustomer)) + 
    geom_point(mapping = aes(color = WeekDay), show.legend = FALSE, na.rm = TRUE) + 
    #stat_summary(fun = mean, alpha = 1/2, color = "#A2C523")  + 
    geom_smooth(formula = y ~ x, method = "loess", span = 0.2, color = "grey", na.rm = TRUE) +
    facet_wrap(~WeekDay) +   
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22), 
                       label = c("0.00H", "2.00H", "4.00H", "6.00H", "8.00H", "10.00H", "12.00H", "14.00H", "16.00H", "18.00H", "20.00H", "22.00H")) +
    labs(x = "Hour", y =  paste(colName[1], " [Max-Min Range]"), title = "Separated WeekDay. Shadow is an Average Value" ) + 
    scale_colour_manual(values = cols) + 
    theme(
      plot.title = element_text(size = rel(1.1), face = "bold"), 
      legend.title = element_text(size = rel(1),face = "bold"), legend.position = "top",
      axis.title.x = element_text(size = rel(1.15), face = "bold"),
      axis.title.y = element_text(size = rel(1), face = "bold"),
      axis.text.x =  element_text(size = rel(1.3), face = "bold"),
      axis.text.y =  element_text(size = rel(1.3), face = "bold"),
      strip.text = element_text(face = "bold")
    ) +
    guides(x = guide_axis(angle = 30))
  
  if("SalesQty" %in% colnames(datfSplitAggByHour)) {
    if(NROW(datfSplitAggByHour) > 1) datfSplitAggByHour <- outlierRemoveSalesQty(datfSplitAggByHour)
    datfSplitAggByHour %>% group_by(Hour) %>% summarise(SalesQtyPerCustomer = sum(SalesQty)/sum(NumberOfCustomer)) -> SalesQtyPerHour
    datfSplitAggByHourAvgValue <- datfSplitAggByHour %>% ungroup() %>% select(Hour, WeekDay, NumberOfCustomer, SalesQty) %>% group_by(Hour, WeekDay) %>% summarise_all(list(mean))    
    geomQty <- geom_point(aes(color = WeekDay, size = SalesQty), alpha = 1/2, na.rm = TRUE) 
  }
  else {
    datfSplitAggByHourAvgValue <- datfSplitAggByHour %>% ungroup() %>% select(Hour, WeekDay, NumberOfCustomer) %>% group_by(Hour, WeekDay) %>% summarise_all(list(mean))
    geomQty <- NULL  
  }
  
  oriDataByHourAndWeekDayQty <- ggplot(data = datfSplitAggByHourAvgValue, mapping = aes(x = Hour, y = NumberOfCustomer)) + 
    geomQty + 
    geom_point(aes(color = WeekDay), size = 1.5, na.rm = TRUE) + 
    geom_line(aes(linetype = WeekDay, color = WeekDay), show.legend = FALSE, na.rm = TRUE) +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22), 
                       label = c("0.00H", "2.00H", "4.00H", "6.00H", "8.00H", "10.00H", "12.00H", "14.00H", "16.00H", "18.00H", "20.00H", "22.00H")) +
    labs(x = "Hour", y =  colName[1], title = "Average Value from Separated Weekday." ) + 
    scale_colour_manual(values = cols, guide = guide_legend(
      direction = "horizontal",
      title.position = "bottom",
      label.position = "top", nrow = 1, title.hjust = 0.5, label.vjust = 1, label.hjust = 0, 
      label.theme = element_text(angle = 90, size = 11)
    )) + 
    scale_size(guide = guide_legend(
      title = colName[2],
      direction = "horizontal", 
      title.position = "bottom", 
      label.position = "bottom", 
      nrow = 2, 
      #label.theme = element_text(angle = 90, size = 11)
    )) +     
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
  
  if("SalesQty" %in% colnames(datfSplitAggByHour)){
    customerVsQty <- ggplot(data = datfSplitAggByHour, mapping = aes(x = NumberOfCustomer, y = SalesQty)) + 
      geom_smooth(mapping = aes(color = WeekDay, linetype = WeekDay), formula = y ~ x, method = "loess", se = FALSE, na.rm = TRUE) +
      scale_linetype(guide = "none") +
      labs(x =  colName[1], y =  colName[2], title = "Cor-relation" ) + 
      scale_colour_manual(values = cols) + 
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
  }
  
  tagList(
    renderPlot(oriData),
    renderPlot(oriDataByHour),      
    renderPlot(oriDataByHourAndWeekDay),
    renderPlot(oriDataByHourAndWeekDayQty),      
    if("SalesQty" %in% colnames(datfSplitAggByHour)) renderPlot(customerVsQty)     
  )
}