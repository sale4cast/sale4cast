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
  
  hourToDay <- function(datfSplitAggByHour) {
    datfSplitAggByDay <- datfSplitAggByHour %>% ungroup() %>% select(-Hour, -WeekDay) %>% group_by(Year, Month, Day) %>% summarise_all(sum)        
    return(datfSplitAggByDay)
  } %>% memoise()  
  
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
    print(paste('DateTimeColumnIndex: ', colIndicator[[1]],'    DateColumnIndex: ', colIndicator[[2]],'    TimeColumnIndex: ', colIndicator[[3]]))    
    datfSplitAggByHour <- splitDatfForSelectedCol(datf, colIndicator[[1]], colIndicator[[2]], colIndicator[[3]])
    datfSplitAggByHour <- datfSplitAggByHour %>% mutate(WeekDay = format(make_date(Year, Month, Day), "%A"))
    return(list(datfSplitAggByHour, colIndicator))
  })
  
  
  output$createSlider <- renderUI({
    datfSplitAggByDay <- NULL
    splitedDatfAggByHour <- splitedDatfYMDHMS()    
    datfSplitAggByHour <- splitedDatfAggByHour[[1]]
    datfSplitAggByDay <- hourToDay(datfSplitAggByHour)
    firstRow <- head(datfSplitAggByDay,1) 
    lastRow <- tail(datfSplitAggByDay,1)
    from  <- make_date(firstRow[[1,1]], firstRow[[1,2]], firstRow[[1,3]])
    to    <- make_date(lastRow[[1,1]], lastRow[[1,2]], lastRow[[1,3]])
    toMin <- as.Date(to - dmonths(1))
    
    from <- as.Date("2013-11-01")
    to <- as.Date("2013-12-31")
    
    if(toMin > from) 
      toMin = toMin
    else
      toMin = from
    
    toMin <- from
    sliderInput("dateInp", "DateRange", value = c(toMin,to), min = from, max = to, timeFormat="%d-%b-%Y")
    
  })  

  splitedDatfYMDHMS2 <- eventReactive(input$dateInp, {
    splitedDatfAggByHour <- splitedDatfYMDHMS()    
    datfSplitAggByHour <- splitedDatfAggByHour[[1]]
    datfSplitAggByHour <- datfSplitAggByHour %>% mutate(Date = make_date(Year, Month, Day)) %>% filter(Date >= input$dateInp[1], Date <= input$dateInp[2]) %>% select(-Date) #as.Date("2013-12-10")
    return(list(datfSplitAggByHour, splitedDatfAggByHour[[2]]))
  })
  
  output$plotHourlyInsightUI <- renderUI({
    splitedDatfAggByHour <- splitedDatfYMDHMS2()
    datfSplitAggByHour <- splitedDatfAggByHour[[1]]
    colIndicator <- splitedDatfAggByHour[[2]]    
    plotHourlyInsight(datfSplitAggByHour, colIndicator)    
  })

  output$plotHourlyForecastUI <- renderUI({
    splitedDatfAggByHour <- splitedDatfYMDHMS2()
    datfSplitAggByHour <- splitedDatfAggByHour[[1]]
    colIndicator <- splitedDatfAggByHour[[2]]    
    plotHourlyForecast(datfSplitAggByHour, colIndicator)
  })
  
  output$plotDailyInsightUI <- renderUI({
    splitedDatfAggByHour <- splitedDatfYMDHMS2()
    datfSplitAggByHour <- splitedDatfAggByHour[[1]]
    colIndicator <- splitedDatfAggByHour[[2]]
    datfSplitAggByDay <- hourToDay(datfSplitAggByHour)
    plotDailyInsight(datfSplitAggByDay)    
  })  
  
  output$plotDailyForecastUI <- renderUI({
    splitedDatfAggByHour <- splitedDatfYMDHMS2()
    datfSplitAggByHour <- splitedDatfAggByHour[[1]]
    colIndicator <- splitedDatfAggByHour[[2]]   
    datfSplitAggByDay <- hourToDay(datfSplitAggByHour)             
    dailyForecastAndPlot(datfSplitAggByDay)
  })
  
})

