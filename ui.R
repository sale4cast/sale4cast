library(shiny)
shinyUI(fluidPage(
  titlePanel("File Input"),
  sidebarLayout(
    sidebarPanel(
      tags$a(href="//shortstaysolution.com/intro/", "Instruction"),
      fileInput("file","Upload the file (CSV or Excel)"),
      textInput("link", "Upload link")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Hourly Insight", uiOutput("plotHourlyInsight")),
        tabPanel("Hourly Forcast", uiOutput("plotHourlyForecast")),
        tabPanel("Daily Insight", uiOutput("plotDailyInsight")),        
        tabPanel("Daily Forcast", uiOutput("plotDailyForecast"))
      )      
    )
  )
))
