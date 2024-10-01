options(shiny.maxRequestSize=30*1024^2)
source("CommonPlot.R")
source("DailyInsight.R")
source("DailyForecast.R")
source("DateFormat.R")
source("DataImputation.R")
source("FindDateTimeColumnIndex.R")
source("HourToDailyInsight.R")
source("HourToDailyForecast.R")
source("HourlyForecast.R")
source("HourlyInsight.R")
source("NumericColumnAnalysis.R")
source("ParseNumber.R")
source("ReadFile.R")
source("SharedFunc.R")
library(googlesheets4)
library(dplyr)
library(fpp2)
library(ggfortify)
library(lubridate)
library(memoise)
library(openxlsx)
library(scales)
library(shiny)
library(stringr)
library(tibble)
library(readr)
options(dplyr.summarise.inform = FALSE)
options(dplyr.warn.conflicts = FALSE)

ui <- fluidPage(
  titlePanel("File Input"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file (CSV or Excel)"),
      textInput("link", "Upload link"),
      uiOutput("createSlider")
    ),
    mainPanel(
      uiOutput("createTab")
    )
  )
)

server <- function(input,output,session){
  hourToDay <- function(datfSplitAggByHour) {
    datfSplitAggByDay <- datfSplitAggByHour %>% ungroup() %>% select(-Hour) %>% group_by(Year, Month, Day) %>% summarise_all(sum)        
    return(datfSplitAggByDay)
  } %>% memoise()  
  
  getDataFrame <- reactive({
    validate(
      need(is.null(input$file) == FALSE || input$link != "", "Please select a data set")
    )
    file_in <- input$file
    dPath <- file_in$datapath 
    upLink <- input$link
    print(upLink)
    datf <- NULL
    if(upLink != "") {
      if(grepl("docs.google.com/spreadsheets", upLink) == TRUE) {
        options(gargle_oauth_cache = ".secrets")
        gs4_auth()
        list.files(".secrets/")
        gs4_deauth()
        gs4_auth(cache = ".secrets", email = "shafiul0304034@gmail.com")
        datf <- read_sheet(upLink, col_types = "c")
      } else {
        if(grepl("dl=0",upLink)) upLink <- gsub("dl=0","dl=1",upLink)
        datf <- readingFiles(upLink)
      }
      validate(
        need(datf != "", "Uploaded file/Link is not working or Permission is denied.")
      )
      return(datf)
    }
    else{
      id <- showNotification("Reading data...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)      
      datf <- readingFiles(dPath)
      return(datf)
    }
  })
  
  identifyColumn <- reactive({
    datf <- getDataFrame()
    datf <- na.omit(head(datf, 60))
    colIndicator <- findDateTimeColumnIndex(datf)
    return(colIndicator)
  })
  
  splitedDatfYMDHMS <- reactive({
    colIndicator <- identifyColumn()
    splitedDatf = NULL
    if(colIndicator[[1]] != 0 || (colIndicator[[2]] != 0 && colIndicator[[3]] != 0))
    {
      datf <- getDataFrame() %>% na.omit()
      splitedDatf <- splitDatfForDateTimeCol(datf, colIndicator[[1]], colIndicator[[2]], colIndicator[[3]])      
    }
    else if(colIndicator[[1]] == 0 && (colIndicator[[2]] != 0 && colIndicator[[3]] == 0))
    {
      datf <- getDataFrame()
      splitedDatf <- splitDatfForDateCol(datf, colIndicator[[1]], colIndicator[[2]], colIndicator[[3]])      
    }
    return(splitedDatf)
  })
  
  splitedDatfYMDHMS2 <- eventReactive(c(input$dateInp, input$file, input$link), {
      returnVar <- splitedDatfYMDHMS()
      splitedDatf <- returnVar %>% mutate(Date = make_date(Year, Month, Day)) %>% filter(Date >= input$dateInp[1], Date <= input$dateInp[2]) %>% select(-Date)
      return(splitedDatf)
  })

  output$createSlider <- renderUI({
    datfSplitAggByDay <- NULL
    colIndicator <- identifyColumn()
    if(colIndicator[[1]] != 0 || colIndicator[[2]] != 0)
    {
      to <- from <- 0
      splitedDatf <- splitedDatfYMDHMS() %>% na.omit()
      if(colIndicator[[1]] != 0 || (colIndicator[[2]] != 0 && colIndicator[[3]] != 0)) 
      splitedDatf <- hourToDay(splitedDatf)
         firstRow <- head(splitedDatf,1)
          lastRow <- tail(splitedDatf,1)
            from  <- make_date(firstRow[[1,1]], firstRow[[1,2]], firstRow[[1,3]])
            to    <- make_date(lastRow[[1,1]], lastRow[[1,2]], lastRow[[1,3]])
            toMin <- as.Date(to - dmonths(1))
      if(toMin > from)
        toMin = toMin
      else
        toMin = from
      
      toMin <- from
      sliderInput("dateInp", "DateRange", value = c(toMin,to), min = from, max = to, timeFormat="%d-%b-%Y")
    }
  })  
  
  output$createTab <- renderUI({
    colIndicator <- identifyColumn()
    if(colIndicator[[1]] != 0 || (colIndicator[[2]] != 0 && colIndicator[[3]] != 0))
      tabsetPanel(
        tabPanel("Daily Forecast", uiOutput("plotHourToDailyForecastUI")),
        tabPanel("Daily Insight", uiOutput("plotHourToDailyInsightUI")),        
        tabPanel("Hourly Insight", uiOutput("plotHourlyInsightUI")),        
        tabPanel("Hourly Forecast", uiOutput("plotHourlyForecastUI"))
      )    
    else if(colIndicator[[2]] != 0 && (colIndicator[[1]] == 0 && colIndicator[[3]] == 0))
      tabsetPanel(
        tabPanel("Daily Insight", uiOutput("plotDailyInsightUI")),
        tabPanel("Daily Forecast", uiOutput("plotDailyForecastUI"))
      )    
    else if(colIndicator[[4]] != 0 && (colIndicator[[1]] == 0 && colIndicator[[2]] == 0))
      tabsetPanel(
        tabPanel("Forecast", uiOutput("otherForecast"))
      )    
  })

  output$plotHourlyInsightUI <- renderUI({
    colIndicator <- identifyColumn()
    if(colIndicator[[1]] != 0 || (colIndicator[[2]] != 0 && colIndicator[[3]] != 0))
      hourlyInsight(splitedDatfYMDHMS2())      
  })

  output$plotHourlyForecastUI <- renderUI({
    colIndicator <- identifyColumn()
    if(colIndicator[[1]] != 0 || (colIndicator[[2]] != 0 && colIndicator[[3]] != 0))
      hourlyForecast(splitedDatfYMDHMS2())      
  })
  
  output$plotHourToDailyInsightUI <- renderUI({
    colIndicator <- identifyColumn()
    if(colIndicator[[1]] != 0 || (colIndicator[[2]] != 0 && colIndicator[[3]] != 0))
    {
      splitedDatf <- splitedDatfYMDHMS2()     
      splitedDatf <- hourToDay(splitedDatf)
      hourToDailyInsight(splitedDatf)    
    }
  })  
  
  output$plotHourToDailyForecastUI <- renderUI({
    colIndicator <- identifyColumn()
    if(colIndicator[[1]] != 0 || (colIndicator[[2]] != 0 && colIndicator[[3]] != 0))
    {
      datfSplitCombByDay <- splitedDatfYMDHMS2()
      datfSplitCombByDay <- datfSplitCombByDay %>% ungroup() %>% select(-Hour) %>% group_by(Year, Month, Day) %>% summarise_all(sum)
      hourToDailyForecast(datfSplitCombByDay)
    }
  })
  
  output$plotDailyInsightUI <- renderUI({
    colIndicator <- identifyColumn()
    if(colIndicator[[1]] == 0 || (colIndicator[[2]] != 0 && colIndicator[[3]] == 0))
      dailyInsigtAndPlot(splitedDatfYMDHMS2())
  })

  output$plotDailyForecastUI <- renderUI({
    colIndicator <- identifyColumn()
    if(colIndicator[[1]] == 0 || (colIndicator[[2]] != 0 && colIndicator[[3]] == 0))
      dailyForecastAndPlot(splitedDatfYMDHMS2())      
  })  
  
  output$otherForecast <- renderUI({
    colIndicator <- identifyColumn()
    if(colIndicator[[1]] == 0 && colIndicator[[2]] == 0 && colIndicator[[3]] == 0 && colIndicator[[4]] != 0)
    {
      datf <- getDataFrame()
      splitedDatf <- splitDatfForNumericCol(datf)      
      numericInsightAndForecastPlot(splitedDatf)      
    }
  })  
  
  session$onSessionEnded(function() {
    stopApp()
  })  
}

shinyApp(ui = ui, server = server)
