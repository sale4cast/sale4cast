options(shiny.maxRequestSize=30*1024^2)
source("CommonPlot.R")
source("DateFormat.R")
source("FindDateTimeColumnIndex.R")
source("DataImputation.R")
source("PlotHourlyForecast.R")
source("ParseNumber.R")
source("DailyInsight.R")
source("DailyForecast.R")
source("PlotHourlyInsight.R")
source("ReadFile.R")
source("SharedFunc.R")
library(data.table)
library(gdata)
library(ggfortify)
library(gridExtra)
library(fpp2)
library(lubridate) # For DateTime
library(memoise)
library(openxlsx) # For the webaccess of .xlsx file
library(patchwork)
library(timetk)
library(scales) # For scaling of X lab and Y lab
library(shiny)
library(tidyverse)

shinyServer(function(input,output,session){
  
  getDataFrame <- reactive({
    
    validate(
      need(input$file != "" || input$link != "", "Please select a data set")
    )
    
    file_in <- input$file
    dPath <- file_in$datapath 
    
    upLink <- input$link
    print(upLink)
    
    if(upLink != ""){
      
      if(grepl("dl=0",upLink)){
        upLink <- gsub("dl=0","dl=1",upLink)
      }
      
      print(upLink)
      datf <- readingFiles(upLink)
      validate(
        need(datf != "", "Uploaded file/Link is not working or Permission is denied.")
      )
      return(datf)
    }
    else{
      datf <- readingFiles(dPath)
      return(datf)
    }
  })
# Story: If the dateTime format is like ymd_h and a total NumOfSalesOrder is almost equal to a total number of Row. Column has a unique hour.   
# Story: It can also happen that hourly time stamp came uniquely once but it is in the format of ymd_hms, ymd_hm, ymd_h, ymd_hm or ymd_hms.   
# Step1: check the dateTimeFormat string to ensure whether the format looks like ymd_h, dmy_h, mdy_h, ymd_hm or ymd_hms. 
# Step2: Sum of NumOfSalesOrder which should be close to a total number of Row i.e, sum(NumOfSalesOrder)/NROW(datf) < 1.6. However the sum(salesQty)/NROW(datf) should be high enough.
# Step3: Even if a format looks like ymd_hms ymd_hm, sum(NumOfSalesOrder)/NROW(datf) < 1.6 needs to be checked.  

  splitedDatfYMDHMS <- reactive({
    datf <- getDataFrame()
    datf <- na.omit(datf)
    colIndicator <- findDateTimeColumnIndex(datf)
    print(paste('DateTimeColumnIndex: ', colIndicator[[1]],'    DateColumnIndex: ', colIndicator[[2]],'    TimeColumnIndex: ', colIndicator[[3]]))    
    datfSplitAggByHour <- splitDatfForSelectedCol(datf, colIndicator[[1]], colIndicator[[2]], colIndicator[[3]])
    datfSplitAggByHour <- datfSplitAggByHour %>% mutate(WeekDay = format(make_date(Year, Month, Day), "%A"))
    return(list(datfSplitAggByHour, colIndicator))
  })
  
  output$plotHourlyInsightUI <- renderUI({
    splitedDatfAggByHour <- splitedDatfYMDHMS()
      datfSplitAggByHour <- splitedDatfAggByHour[[1]]
            colIndicator <- splitedDatfAggByHour[[2]]    
    plotList <- plotHourlyInsight(datfSplitAggByHour, colIndicator)    
    tagList(
      renderPlot(plotList[[1]]+plotList[[2]]),
      renderPlot(plotList[[3]]),
      if("SalesQty" %in% colnames(datfSplitAggByHour)) renderPlot(plotList[[4]] + plotList[[5]])      
      else renderPlot(plotList[[4]])
    )
  })
  
  output$plotHourlyForecastUI <- renderUI({
    splitedDatfAggByHour <- splitedDatfYMDHMS()
      datfSplitAggByHour <- splitedDatfAggByHour[[1]]
            colIndicator <- splitedDatfAggByHour[[2]]    
    plotHourlyForecast(datfSplitAggByHour, colIndicator)
  })
  
  hourToDay <- function(datfSplitAggByHour) {
    datfSplitAggByDay <- datfSplitAggByHour %>% ungroup() %>% select(-Hour, -WeekDay) %>% group_by(Year, Month, Day) %>% summarise_all(sum)        
    return(datfSplitAggByDay)
  } %>% memoise()
  
  output$plotDailyInsightUI <- renderUI({
    splitedDatfAggByHour <- splitedDatfYMDHMS()
      datfSplitAggByHour <- splitedDatfAggByHour[[1]]
            colIndicator <- splitedDatfAggByHour[[2]]
       datfSplitAggByDay <- hourToDay(datfSplitAggByHour)
             plotInsight <- plotDailyInsight(datfSplitAggByDay)    
    tagList(
      renderPlot(plotInsight[[1]]),
      renderPlot(plotInsight[[2]] + plotInsight[[3]]),
      renderPlot(plotInsight[[4]]),
      renderPlot(plotInsight[[5]])      
    )
  })  
  
  output$plotDailyForecastUI <- renderUI({
     splitedDatfAggByHour <- splitedDatfYMDHMS()
       datfSplitAggByHour <- splitedDatfAggByHour[[1]]
             colIndicator <- splitedDatfAggByHour[[2]]   
        datfSplitAggByDay <- hourToDay(datfSplitAggByHour)             
    plotListDailyForecast <- dailyForecastAndPlot(datfSplitAggByDay)
    tagList(
      renderPlot(plotListDailyForecast[[1]]),
      renderPlot(plotListDailyForecast[[2]]),
      renderPlot(plotListDailyForecast[[3]]),
      renderDataTable(plotListDailyForecast[[4]]),
      downloadButton('exportForecast',"Download Report")
    )
  })
  
  pdfTitle<- as.character(format(Sys.Date(),"%A"))  
  
  output$exportForecast <- downloadHandler(
    #filename is a default argument of downloadHandler function. A string of the filename, including extension or a function that returns such a string..
    filename <- function() {
      paste("report-", Sys.Date(), ".pdf")
    },
    #https://community.rstudio.com/t/download-shiny-report-from-renderdatatables-and-plots-to-pdf/23804    
    #content is a also default argument of downloadHandler function. A function that takes a single argument "file" that is a file path (string) of a nonexistent temp file, and writes the content to that file path. 
    content <- function(file) {
      datfSplitAggByDay <- splitedDatfYMDHMS()[[1]]
      colIndicator <- splitedDatfYMDHMS()[[3]]    
      plotListDailyForecast <- createDailyAnalysis(datfSplitAggByDay)
      pdf(file,title = pdfTitle,paper = "a4") # open the pdf device
      grid.arrange(plotListDailyForecast[[1]], plotListDailyForecast[[2]], plotListDailyForecast[[3]], clip = TRUE)
      grid.arrange(tableGrob(plotListDailyForecast[[4]]), clip = TRUE) 
      dev.off()  # turn the device off
    }
  )
})

