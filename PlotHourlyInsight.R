plotHourlyInsight <- function(datfSplitAggByHour, colIndicator)
{
  oriData <- oriDataByHour <- oriDataByHourAndWeekDay <- customerVsQty <- 0  
  datfSplitAggByHour <- datfSplitAggByHour %>% mutate(DateTime = make_datetime(Year, Month, Day, Hour))
  nRowBefore <- NROW(datfSplitAggByHour)  
  
  if("NumOfSalesOrder" %in% colnames(datfSplitAggByHour)) datfSplitAggByHour <- outlierRemoveSalesOrder(datfSplitAggByHour)
  numOfOutlier <- nRowBefore - NROW(datfSplitAggByHour)
  print(numOfOutlier)
  
  cols <- c("Sunday" = "red", "Monday" = "#B38867", "Tuesday" = "#EBDF00", "Wednesday" = "#FAAF08", "Thursday" = "#4D648D", "Friday" = "darkgrey", "Saturday" = "#EE693F")  
  oriData <- ggplot(data = datfSplitAggByHour) + 
    geom_point(mapping = aes(x = DateTime, y = NumOfSalesOrder, color = WeekDay)) + 
    scale_x_datetime(labels = date_format("%d-%m-%y %H:%M:%S"))  + 
    labs(x = "DateTime", y = "Number of Sales Order", title = "Data over Date and Time" ) + 
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
  
  oriDataByHour <- ggplot(data = datfSplitAggByHour) + geom_point(mapping = aes(x = Hour, y = NumOfSalesOrder, color = WeekDay)) +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22), 
                       label = c("0.00H", "2.00H", "4.00H", "6.00H", "8.00H", "10.00H", "12.00H", "14.00H", "16.00H", "18.00H", "20.00H", "22.00H")) +
    labs(x = "Hour", y = "Number of Sales Order", title = "Data is Arranged over Hour" ) + 
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
  
  oriDataByHourAndWeekDay <- ggplot(data = datfSplitAggByHour, mapping = aes(x = Hour, y = NumOfSalesOrder)) + geom_point(mapping = aes(color = WeekDay), show.legend = FALSE) + stat_summary(fun = mean, alpha = 1/2, color = "#A2C523")  + geom_smooth(span = 0.2, color = "grey") +
    facet_wrap(~WeekDay) +   
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22), 
                       label = c("0.00H", "2.00H", "4.00H", "6.00H", "8.00H", "10.00H", "12.00H", "14.00H", "16.00H", "18.00H", "20.00H", "22.00H")) +
    labs(x = "Hour", y = "Number of Sales Order", title = "Separated WeekDay. Shadow is an Average" ) + 
    scale_colour_manual(values = cols) + 
    theme(
      plot.title = element_text(size = rel(1.1), face = "bold"), 
      legend.title = element_text(size = rel(1),face = "bold"), legend.position = "top",
      axis.title.x = element_text(size = rel(1.15), face = "bold"),
      axis.title.y = element_text(size = rel(1), face = "bold"),
      axis.text.x =  element_text(size = rel(1.3), face = "bold"),
      axis.text.y =  element_text(size = rel(1.3), face = "bold")
    ) +
    guides(x = guide_axis(angle = 30))
  
  if("SalesQty" %in% colnames(datfSplitAggByHour)) {
    datfSplitAggByHour <- outlierRemoveSalesQty(datfSplitAggByHour)
    datfSplitAggByHour %>% group_by(Hour) %>% summarise(SalesQtyPerCustomer = sum(SalesQty)/sum(NumOfSalesOrder)) -> SalesQtyPerHour
    datfSplitAggByHourAvgValue <- datfSplitAggByHour %>% ungroup() %>% select(Hour, WeekDay, NumOfSalesOrder, SalesQty) %>% group_by(Hour, WeekDay) %>% summarise_all(list(mean))    
    geomQty <- geom_point(aes(color = WeekDay, size = SalesQty), alpha = 1/2) 
  }
  else {
    datfSplitAggByHourAvgValue <- datfSplitAggByHour %>% ungroup() %>% select(Hour, WeekDay, NumOfSalesOrder) %>% group_by(Hour, WeekDay) %>% summarise_all(list(mean))
    geomQty <- NULL  
  }
  
  oriDataByHourAndWeekDayQty <- ggplot(data = datfSplitAggByHourAvgValue, mapping = aes(x = Hour, y = NumOfSalesOrder)) + 
    geomQty + 
    geom_point(aes(color = WeekDay), size = 1.5) + 
    geom_line(aes(linetype = WeekDay, color = WeekDay), show.legend = FALSE) +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22), 
                       label = c("0.00H", "2.00H", "4.00H", "6.00H", "8.00H", "10.00H", "12.00H", "14.00H", "16.00H", "18.00H", "20.00H", "22.00H")) +
    labs(x = "Hour", y = "Number of Sales Order", title = "Average Value from Separated Weekday." ) + 
    scale_colour_manual(values = cols, guide = guide_legend(
      direction = "horizontal",
      title.position = "bottom",
      label.position = "top", nrow = 1, title.hjust = 0.5, label.vjust = 1, label.hjust = 0, 
      label.theme = element_text(angle = 90, size = 11)
    )) + 
    scale_size(guide = guide_legend( 
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
    customerVsQty <- ggplot(data = datfSplitAggByHour, mapping = aes(x = NumOfSalesOrder, y = SalesQty)) + 
      geom_smooth(mapping = aes(color = WeekDay, linetype = WeekDay), se = FALSE) +
      #      geom_smooth(mapping = aes(linetype = WeekDay), se = FALSE) +
      scale_colour_manual(values = cols) + scale_linetype(guide = "none") +
      labs(x = "Number of Sales Order", y = "Sales Qty", title = "Cor-relation" ) + 
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