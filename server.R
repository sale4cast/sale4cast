options(shiny.maxRequestSize=30*1024^2)
source("DateFormat.R")
source("FindDateTimeColumnIndex.R")
source("DataImputation.R")
source("PlotHourlyForecast.R")
source("ParseNumber.R")
source("PlotDailyInsight.R")
source("PlotDailyForecast.R")
source("PlotHourlyInsight.R")
source("ReadFile.R")
library(data.table)
library(gdata)
library(ggfortify)
library(gridExtra)
library(fpp2)
library(imputeTS) # For na_kalman()
library(lubridate) # For DateTime
library(openxlsx) # For the webaccess of .xlsx file
library(patchwork)
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
  
  splitedDatfYMDHMS <- reactive({
    datf <- getDataFrame()
    datf <- na.omit(datf)
    colIndicator <- findDateTimeColumnIndex(datf)
    print(paste('DateTimeColumnIndex: ', colIndicator[[1]],'    DateColumnIndex: ', colIndicator[[2]],'    TimeColumnIndex: ', colIndicator[[3]],'    valueColumnIndex: ', colIndicator[[4]]))    
    datfSplitAggByHour <- datfWithSelectedCol(datf, colIndicator)
    datfSplitAggByHour <- na.omit(datfSplitAggByHour)
    datfSplitAggByHour %>% ungroup() %>% select(-Hour)  %>% group_by(Year, Month, Day, WeekDay, WeekNr) %>% summarise_all(sum) -> datfSplitAggByDay    
    return(list(datfSplitAggByDay, datfSplitAggByHour, colIndicator))
  })
  
  output$plotHourlyInsight <- renderUI({
    datfSplitAggByHour <- splitedDatfYMDHMS()[[2]]
    colIndicator <- splitedDatfYMDHMS()[[3]]    
    plotList <- plotHourlyInsight(datfSplitAggByHour, colIndicator)    
    tagList(
      renderPlot(plotList[[1]]+plotList[[2]]),
      renderPlot(plotList[[3]]),
      if(colIndicator[[4]] > 0) renderPlot(plotList[[4]] + plotList[[5]])      
    )
  })

  output$plotHourlyForecast <- renderUI({
    datfSplitAggByHour <- splitedDatfYMDHMS()[[2]]
    colIndicator <- splitedDatfYMDHMS()[[3]]
    downloadButton('exportHourlyForecast',"Download Report")
    testVar <- plotHourlyForecast(datfSplitAggByHour, colIndicator)
  })
  
  output$plotDailyForecast <- renderUI({
    datfSplitAggByDay <- splitedDatfYMDHMS()[[1]]
    colIndicator <- splitedDatfYMDHMS()[[3]]    
    plotListDailyForecast <- createDailyAnalysis(datfSplitAggByDay)
    
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
  
  output$plotDailyInsight <- renderUI({
    datfSplitAggByDay <- splitedDatfYMDHMS()[[1]]
    colIndicator <- splitedDatfYMDHMS()[[3]]    
    plotInsight <- plotDailyInsight(datfSplitAggByDay)    
    tagList(
      renderPlot(plotInsight[[1]]),
      renderPlot(plotInsight[[2]] + plotInsight[[3]]),
      renderPlot(plotInsight[[4]]),
      renderPlot(plotInsight[[5]])      
    )
  })  
})

