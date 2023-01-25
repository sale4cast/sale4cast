plotHourlyInsight <- function(datfSplitAggByHour, colIndicator)
{
  oriData <- oriDataByHour <- oriDataByHourAndWeekDay <- customerVsQty <- 0
  cols <- c("Sunday" = "red", "Monday" = "#B38867", "Tuesday" = "#EBDF00", "Wednesday" = "#FAAF08", "Thursday" = "#4D648D", "Friday" = "darkgrey", "Saturday" = "#EE693F")
  oriData <- ggplot(data = datfSplitAggByHour) + 
    geom_point(mapping = aes(x = make_datetime(Year, Month, Day, Hour), y = NumberOfCustomer, color = WeekDay)) + 
    ylim(minMaxLim(datfSplitAggByHour[["NumberOfCustomer"]])) +
    scale_x_datetime(labels = date_format("%d-%m-%y %H:%M:%S"))  + 
    labs(x = "DateTime", y = "Number of Sales Order", title = "Sales Order is Represented over Date and Hour" ) + 
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
  
  a <- 10
  
  oriDataByHour <- ggplot(data = datfSplitAggByHour) + geom_point(mapping = aes(x = Hour, y = NumberOfCustomer, color = WeekDay)) +
    ylim(minMaxLim(datfSplitAggByHour[["NumberOfCustomer"]])) +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22), 
                       label = c("0.00H", "2.00H", "4.00H", "6.00H", "8.00H", "10.00H", "12.00H", "14.00H", "16.00H", "18.00H", "20.00H", "22.00H")) +
    labs(x = "Hour", y = "Number of Sales Order", title = "Sales Order is Represented over an Hour" ) + 
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
  
  oriDataByHourAndWeekDay <- ggplot(data = datfSplitAggByHour, mapping = aes(x = Hour, y = NumberOfCustomer)) + geom_point(mapping = aes(color = WeekDay), show.legend = FALSE) + stat_summary(fun = mean, alpha = 1/2, color = "#A2C523")  + geom_smooth(span = 0.2, color = "grey") +
    facet_wrap(~WeekDay) +   
    ylim(minMaxLim(datfSplitAggByHour[["NumberOfCustomer"]])) +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22), 
                       label = c("0.00H", "2.00H", "4.00H", "6.00H", "8.00H", "10.00H", "12.00H", "14.00H", "16.00H", "18.00H", "20.00H", "22.00H")) +
    labs(x = "Hour", y = "Number of Sales Order", title = "Sales Order is Represented over an Hour. Thick shadow represents average Sales" ) + 
    scale_colour_manual(values = cols) + 
    theme(
      plot.title = element_text(size = rel(1.2), face = "bold"), 
      legend.title = element_text(size = rel(1),face = "bold"), legend.position = "top",
      axis.title.x = element_text(size = rel(1.15), face = "bold"),
      axis.title.y = element_text(size = rel(1), face = "bold"),
      axis.text.x =  element_text(size = rel(1.3), face = "bold"),
      axis.text.y =  element_text(size = rel(1.3), face = "bold")
    ) +
    guides(x = guide_axis(angle = 30))
  
  datfSplitAggByHourAvgValue <- datfSplitAggByHour %>% ungroup() %>% select(Hour, WeekDay, NumberOfCustomer, salesQty) %>% group_by(Hour, WeekDay) %>% summarise_all(list(mean))
  oriDataByHourAndWeekDayQty <- ggplot(data = datfSplitAggByHourAvgValue, mapping = aes(x = Hour, y = NumberOfCustomer)) + 
    geom_point(mapping = aes(color = WeekDay, size = salesQty), alpha = 1/3) + 
    geom_point(aes(color = WeekDay), size = 1.5) + 
    geom_line(aes(linetype = WeekDay, color = WeekDay), show.legend = FALSE) +
    ylim(minMaxLim(datfSplitAggByHour[["NumberOfCustomer"]])) +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22), 
                       label = c("0.00H", "2.00H", "4.00H", "6.00H", "8.00H", "10.00H", "12.00H", "14.00H", "16.00H", "18.00H", "20.00H", "22.00H")) +
    labs(x = "Hour", y = "Number of Sales Order", title = "Sales Order [average value] and Quantity are represented over an Hour and Day." ) + 
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
  
  if(colIndicator[[4]] != 0){
    customerVsQty <- ggplot(data = datfSplitAggByHour, mapping = aes(x = NumberOfCustomer, y = salesQty)) + 
      geom_smooth(mapping = aes(color = WeekDay, linetype = WeekDay), se = FALSE) +
#      geom_smooth(mapping = aes(linetype = WeekDay), se = FALSE) +
      scale_colour_manual(values = cols) + scale_linetype(guide = "none") +
      labs(x = "Number of Sales Order", y = "Sales Qty", title = "Co-relation between Sales Qty and a Number of Sales Order" ) + 
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
  return(list(oriData, oriDataByHour, oriDataByHourAndWeekDay, oriDataByHourAndWeekDayQty, customerVsQty))
}