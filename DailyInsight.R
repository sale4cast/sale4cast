plotDailyInsight <- function(datfSplitAggByDay){
  tempColName <- colnames(datfSplitAggByDay)[4]
  colnames(datfSplitAggByDay)[4] <- "SalesQty"  
  #browser()
  datfSplitAggByDay <- datfSplitAggByDay %>% mutate(Date = make_date(Year, Month, Day), WeekDay = format(Date, "%a"))
  nRowBefore <- NROW(datfSplitAggByDay)  
  if(NROW(datfSplitAggByDay) > 1 && ("NumOfSalesOrder" %in% colnames(datfSplitAggByDay))) 
    datfSplitAggByDay <- outlierRemoveSalesOrder(datfSplitAggByDay)
  numOfOutlier <- nRowBefore - NROW(datfSplitAggByDay)
  
  datfSplitAggByDay$WeekDay <- factor(datfSplitAggByDay$WeekDay,
                                      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))  
  filteredMonth <- datfSplitAggByDay %>% select(Year, Month) %>% group_by(Year, Month) %>% summarise(dayCount = n()) %>% tail(4)
  
  sdate <- make_date(head(filteredMonth,1)$Year, head(filteredMonth,1)$Month, 1)
  last4Months <- datfSplitAggByDay %>% filter(Date >= sdate)
  datfSplitAggByDay <- datfSplitAggByDay %>% select(c(5,4,6))  
  #datfSplitAggByDayM <- datfSplitAggByDay %>% mutate(WeekNr = format(Date, "Week-%V-%b"))  
  #if("SalesQty" %in% colnames(datfSplitAggByDay)) geomQty <- geom_point(aes(color = WeekDay, size = SalesQty), alpha = 1/2)
  geomQty <- geom_point(aes(color = WeekDay), size = 2)
  #browser()
  dateIndex = which(colnames(datfSplitAggByDay) == "Date")
  eachDayLinePlot <- ggplot(data = datfSplitAggByDay, mapping = aes(x = Date, y = SalesQty)) + 
    geom_point(size = 1.2) +
    geomQty + 
    geom_line(color = "lightblue") +
    #geom_line(mapping = aes(color = WeekDay), alpha = 1/3, show.legend = FALSE) +    
    #scale_x_date(date_labels = "Week-%V-%b-%y", breaks = breaks_pretty(12)) +
    scale_x_date(date_labels = "%d-%b-%Y", breaks = breaks_pretty(12)) +
    # scale_color_discrete(guide = guide_legend(
    #   override.aes = list(alpha = 1, size = 2),
    #   direction = "horizontal",
    #   title.position = "bottom",
    #   label.position = "top", nrow = 1, title.hjust = 0.5, label.vjust = 1, label.hjust = 0, 
    #   label.theme = element_text(angle = 90, size = 11)      
    # )) +    
    theme(
      plot.title = element_text(size = rel(1.2), face = "bold"), 
      legend.title = element_text(size = rel(1),face = "bold"), 
      legend.position = "top",
      axis.title.x = element_text(size = rel(1.15), face = "bold"),
      axis.title.y = element_text(size = rel(1), face = "bold"),
      axis.text.x =  element_text(size = rel(1.3), face = "bold"),
      axis.text.y =  element_text(size = rel(1.3), face = "bold")
    ) +
    guides(x = guide_axis(angle = 30)) +
    ggtitle(paste("Each Day Line for", tempColName)) + ylab(tempColName)
  
  DayCompare <- tibble(Day = c(1:31), DayOrder = c(rep(1,7), rep(2,7), rep(3,7), rep(4,7), 5,5,5))
  last4Months <- left_join(last4Months, DayCompare, by = "Day")
  last4Months <- last4Months %>% select(-c(Year, Month, Day)) %>% mutate(Month = format(Date, "%b"))
  if(NROW(filteredMonth) >= 4) captionText = "(Last 4 Months .. )"
  else  captionText = NULL
  last4Months$WeekDay <- as.factor(last4Months$WeekDay)
  weekDayCustomerForEachMonth <- ggplot(data = last4Months, mapping = aes(x = WeekDay, y = SalesQty)) +
    geom_point(size = 0.75) +
    geom_line(color = "darkgrey") +
    geom_point(aes(color = WeekDay, size = DayOrder), alpha = 1/2) +
    facet_wrap(~format(Date, "%Y-%m [%b]"), ncol = 2) +
    labs(y = paste(colnames(last4Months)[1], " [Max-Min Range]"), caption = captionText) +
    #scale_x_discrete("WeekDay", labels = c("Friday" = "Fri","Saturday" = "Sat", "Sunday" = "Sun", "Monday" = "Mon", "Tuesday"="Tue", "Wednesday" = "Wed", "Thursday" = "Thu")) +
    scale_size(breaks = 1:5, labels = \(x)c("1st","2nd","3rd","4th","5th")[x]) +
    theme(
      plot.title = element_text(size = rel(1.2), face = "bold"),
      legend.title = element_text(size = rel(1),face = "bold"),
      legend.position = "top",
      axis.title.x = element_text(size = rel(1.15), face = "bold"),
      axis.title.y = element_text(size = rel(1), face = "bold"),
      axis.text.x =  element_text(size = rel(1.3), face = "bold"),
      axis.text.y =  element_text(size = rel(1.3), face = "bold"),
      strip.text = element_text(face = "bold")
      #plot.caption = element_text(face = "italic")
    ) +
    guides(x = guide_axis(angle = 30), color = "none") +
    ggtitle("Small Dot Represents 1st Day of Month") + ylab(tempColName)
  
  allMonthCustomerForEachWeekDay <- ggplot(data = last4Months, mapping = aes(x = Month, y = SalesQty)) +
    geom_point(size = 1) + 
    geom_point(aes(size= DayOrder, color = WeekDay), alpha=1/3) +
    #scale_size_identity() +
    facet_wrap(~WeekDay, nrow = 2) + 
    scale_x_discrete(limit = unique(last4Months$Month)) +      
    scale_size(breaks = 1:5, labels = \(x)c("1st","2nd","3rd","4th","5th")[x]) +
    labs(caption = captionText) +
    theme(
      plot.title = element_text(size = rel(1.2), face = "bold"), 
      legend.title = element_text(size = rel(1), face = "bold"), 
      legend.position = "top",
      axis.title.x = element_text(size = rel(1.15), face = "bold"),
      axis.title.y = element_text(size = rel(1), face = "bold"),
      axis.text.x =  element_text(size = rel(1.3), face = "bold"),
      axis.text.y =  element_text(size = rel(1.3), face = "bold"),
      plot.margin = margin(r=0.5, t=0.5, l=0.5, b=0.5, "cm"),  # Adjust the plot margins
      strip.text = element_text(face = "bold")
    ) +
    guides(x = guide_axis(angle = 60), color = "none") +
    coord_cartesian(clip = "off") +
    ggtitle("Small Dot Represents 1st Day of Month") + ylab(tempColName)
  return(list(renderPlot(eachDayLinePlot), renderPlot(weekDayCustomerForEachMonth), renderPlot(allMonthCustomerForEachWeekDay)))
}

dailyInsigtAndPlot  <- function(splitedDatf) {
  rplot = list()
  rplot <- lapply(4:NCOL(splitedDatf), function(rowIndex){
    splitedDatfC <- splitedDatf %>% .[c(1,2,3,rowIndex)] %>% na.omit()
    plotDailyInsight(splitedDatfC)
  })
  rplot
}
