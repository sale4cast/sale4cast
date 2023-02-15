plotDailyInsight <- function(datfSplitAggByDay){
  
  datfSplitAggByDay <- datfSplitAggByDay %>% mutate(Date = make_date(Year, Month, Day), WeekDay = format(Date, "%A"), MonthName = format(Date, "%B"))
  nRowBefore <- NROW(datfSplitAggByDay)  
  if("NumOfSalesOrder" %in% colnames(datfSplitAggByDay)) datfSplitAggByDay <- outlierRemoveSalesOrder(datfSplitAggByDay)
  numOfOutlier <- nRowBefore - NROW(datfSplitAggByDay)
  print(numOfOutlier)
  plotDataAndTrend <- createPlotDataAndTrend(datfSplitAggByDay)
  
  plotOverWeekDayAvg <- ggplot(data = datfSplitAggByDay, mapping = aes(x = WeekDay, y = NumOfSalesOrder)) + 
    geom_point(aes(group = Month, color = Month), size = 2.3) + 
    geom_line(alpha = 1/3) +
    stat_summary(fun = mean, size = 1, alpha = 1/2, color = "red")  + 
    labs(x = "Date", y = "Number of Sales Order") + 
    scale_colour_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), guide = guide_legend()) + 
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

  DayCompare <- tibble(Day = c(1:31), WeekNr = c(rep(1,7), rep(2,7), rep(3,7), rep(4,7), 5,5,5))
  left_join(datfSplitAggByDay, DayCompare, by = "Day") -> datfSplitAggByDay
  
  plotOverMonthAndWday <- ggplot(data = datfSplitAggByDay, mapping = aes(x = WeekDay, y = NumOfSalesOrder)) + 
    geom_point(aes(color = WeekDay, size = WeekNr), alpha = 1/2) + 
    facet_wrap(~MonthName, ncol = 2) +
    labs(x = "WeekDay", y = "Number of Sales Order") + 
    theme(
      plot.title = element_text(size = rel(1.2), face = "bold"), 
      legend.title = element_text(size = rel(1),face = "bold"), 
      legend.position = "top",
      axis.title.x = element_text(size = rel(1.15), face = "bold"),
      axis.title.y = element_text(size = rel(1), face = "bold"),
      axis.text.x =  element_text(size = rel(1.3), face = "bold"),
      axis.text.y =  element_text(size = rel(1.3), face = "bold")
    ) +
    guides(x = guide_axis(angle = 30), color = "none")
  
  datfSplitAggByDayM <- datfSplitAggByDay %>% mutate(WeekNr = format(Date, "Week-%V-%b"))  
  
  if("SalesQty" %in% colnames(datfSplitAggByDayM)) geomQty <- geom_point(aes(color = WeekDay, size = SalesQty), alpha = 1/2)
  else geomQty <- geom_point(aes(color = WeekDay), size = 2)
  plotOverWeekDayAndNr1 <- ggplot(data = datfSplitAggByDayM, mapping = aes(x = WeekNr, y = NumOfSalesOrder)) + 
    geom_point(size = 1.2) +
    geomQty + 
    geom_line(mapping = aes(group = WeekDay, color = WeekDay, linetype = WeekDay), alpha = 1/3, show.legend = FALSE) +    
    scale_color_discrete(guide = guide_legend(
      override.aes = list(alpha = 1, size = 2),
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
    labs(x = "WeekNr", y = "Number of Sales Order") + 
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

  tagList(
    tags$h5(tags$b("Black and Blue Line Represents Original and Avaerage Data.")),
    renderPlot(plotDataAndTrend),
    tags$h5(tags$b("Red Dot Represents an Average Value.")),
    renderPlot(plotOverWeekDayAvg),
    tags$h5( tags$b("Day 1 to 7 Represents WeekNr1. Day 8 to 14 Represents WeekNr2 and So On.")),    
    renderPlot(plotOverMonthAndWday),  
    tags$h5( tags$b("Data is Represented Over WeekDay, WeekNr and Month.")),    
    renderPlot(plotOverWeekDayAndNr1)
  )
}

