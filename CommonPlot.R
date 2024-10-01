
createPlotDataAndTrend <- function(datfSplitAggByDay, colName, numOfOutlier) {
  if(numOfOutlier > 0) captionText = paste("( ", numOfOutlier, " outliers omited )")
  else  captionText = NULL
  dataAndTrendPlot <- ggplot(data = datfSplitAggByDay, mapping = aes(x = Date, y = NumberOfCustomer)) + 
    geom_line(size = 1.05, alpha = 1/3) + 
    geom_point(mapping = aes(color = WeekDay), alpha = 1, size = 2) + 
    scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2))) + 
    geom_smooth(formula = y ~ x, method = "loess", span = 0.15, se = TRUE, level = 0.40) +
    scale_x_date(date_labels = "%d-%b-%Y")  + 
    labs(x = "Date", y = colName[1], caption = captionText) + 
    theme( legend.position = "top",
           plot.title = element_text(size = rel(1.2), face = "bold"), 
           legend.title = element_text(size = rel(1),face = "bold"), 
           axis.title.x = element_text(size = rel(1.15), face = "bold"),
           axis.title.y = element_text(size = rel(1), face = "bold"),
           axis.text.x =  element_text(size = rel(1.3), face = "bold"),
           axis.text.y =  element_text(size = rel(1.3), face = "bold")
    ) +
    guides(x = guide_axis(angle = 20)) +
    ggtitle("Grey and Blue Line Represent Original and Average Number")
  
  return(dataAndTrendPlot)
} 