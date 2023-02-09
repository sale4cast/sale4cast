plotDailyInsight <- function(datfSplitAggByDay){
  
  datfSplitAggByDay <- datfSplitAggByDay %>% mutate(Date = make_date(Year, Month, Day), WeekDay = format(Date, "%A"), WeekNr = format(Date, "%W-%V-%b"))
  nRowBefore <- NROW(datfSplitAggByDay)  
  if("NumOfSalesOrder" %in% colnames(datfSplitAggByDay)) datfSplitAggByDay <- outlierRemoveSalesOrder(datfSplitAggByDay)
  numOfOutlier <- nRowBefore - NROW(datfSplitAggByDay)
  print(numOfOutlier)
  plotDataAndTrend <- createPlotDataAndTrend(datfSplitAggByDay)
  plotOverWeekDay <- ggplot(data = datfSplitAggByDay, mapping = aes(x = WeekDay, y = NumOfSalesOrder)) + 
    geom_point(aes(group = Month, color = Month), size = 2.3) + 
    geom_line(alpha = 1/3) + 
    labs(x = "Date", y = "Number of Sales Order", title = "Sales Order is Represented Over WeekDay.") + 
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
  
  plotOverWeekDayAvg <- ggplot(data = datfSplitAggByDay, mapping = aes(x = WeekDay, y = NumOfSalesOrder)) + 
    geom_point(aes(group = Month, color = Month), size = 2.3) + 
    geom_line(alpha = 1/3) +
    stat_summary(fun = mean, size = 1, alpha = 1/2, color = "red")  + 
    labs(x = "Date", y = "Number of Sales Order", title = "Red Dot represents an average value.") + 
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
  
  datfSplitAggByDay %>% mutate(MonthName = format(make_date(Year, Month, Day), "%B")) -> datfSplitAggByDayM 
  
  if("SalesQty" %in% colnames(datfSplitAggByDayM)) geomQty <- geom_point(aes(color = WeekDay, size = SalesQty), alpha = 1/2)
  else geomQty <- geom_point(aes(color = WeekDay), size = 2)
  
  plotOverWeekDayAndNr1 <- ggplot(data = datfSplitAggByDayM, mapping = aes(x = WeekNr, y = NumOfSalesOrder)) + 
    geom_point(size = 1.2) +
    geomQty + 
    geom_line(mapping = aes(group = WeekDay, color = WeekDay, linetype = WeekDay), alpha = 1/3, show.legend = FALSE) +    
    scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2) )) + 
    labs(x = "WeekNr", y = "Number of Sales Order", title = "Sales Order is Represented Over WeekDay, WeekNr and Month.") + 
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

#  if("SalesQty" %in% colnames(datfSplitAggByDayM)) geomQty <- geom_point(aes(color = WeekDay, size = SalesQty), alpha = 1/2)
#  else geomQty <- geom_point(aes(color = WeekDay), size = 2)
  
  plotOverWeekDayAndNr2 <- ggplot(data = datfSplitAggByDayM, mapping = aes(x = WeekDay, y = NumOfSalesOrder)) + 
    geomQty + 
    scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2))) +
    geom_line(mapping = aes(group = WeekNr, color = WeekNr, linetype = WeekNr), alpha = 1/3, show.legend = FALSE) +
    facet_wrap(~MonthName, ncol = 2) +
    labs(x = "WeekDay", y = "Number of Sales Order", title = "Sales Order is Represented Over WeekDay, WeekNr and Month.") + 
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
  return(list(plotDataAndTrend, plotOverWeekDay, plotOverWeekDayAvg, plotOverWeekDayAndNr1, plotOverWeekDayAndNr2))
}

