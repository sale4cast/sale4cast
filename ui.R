library(shiny)
shinyUI(fluidPage(
  titlePanel("File Input"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file (CSV or Excel)"),
      textInput("link", "Upload link"),
      uiOutput("createSlider")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Hourly Insight", uiOutput("plotHourlyInsightUI")),
        tabPanel("Hourly Forecast", uiOutput("plotHourlyForecastUI")),        
        tabPanel("Daily Insight", uiOutput("plotDailyInsightUI")),
        tabPanel("Daily Forecast", uiOutput("plotDailyForecastUI"))
      )      
    )
  )
))
