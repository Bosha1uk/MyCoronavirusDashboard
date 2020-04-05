#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(plotly)
library(rsconnect)

source("data.R")

#rsconnect::deployApp('path/to/your/app')

# Define UI
ui <- fluidPage(HTML('<meta name="viewport" content="width=1024">'), 
                
                dashboardPage(
                  dashboardHeader(title = "CoronavirusData", titleWidth = 300),
                  dashboardSidebar(
                    selectInput("Country", "Country: ", c(total_by_country$Country_Region)), width = 300),

                  dashboardBody(uiOutput("Overview"))
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$Overview <- renderUI({
     tabsetPanel(
       tabPanel("Overview",
                tagList(
                  valueBox("Confirmed Cases", value = tags$p(paste(running_total$TotalConfirmed), style = "font-size: 50%;"), width = 3),
                  valueBox("Deaths", value = tags$p(paste(running_total$TotalDeaths), style = "font-size: 50%;"), width = 3),
                  valueBox("Death Percentage", value = tags$p(paste(running_total$DeathPercentage), style = "font-size: 50%;"), width = 3)
                ),
          fluidRow(column(width=12, box(plotlyOutput("Confirmed"), title = "Overall Confirmed Cases", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px")),
                   column(width=12, box(plotlyOutput("deaths"), title = "Overall Deaths", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px")),
                   column(width=12, box(plotlyOutput("deathsP"), title = "Deaths Percentage per day", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px")),
                   column(width=12, box(plotlyOutput("increaseConfirmed"), title = "Confirmed Increase per day", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px")),
                   column(width=12, box(plotlyOutput("increaseDeaths"), title = "Deaths Increase per day", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px")))
       ),
       tabPanel("By Country",
                uiOutput("DatabyCountry"),
                fluidRow(column(width=12, box(plotlyOutput("ConfirmedCountry"), title = "Overall Confirmed Cases", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px")),
                column(width=12, box(plotlyOutput("deathsCountry"), title = "Overall Deaths", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px")),
                column(width=12, box(plotlyOutput("deathsPCountry"), title = "Deaths Percentage per day", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px")),
                column(width=12, box(plotlyOutput("increaseCCountry"), title = "Confirmed Increase per day", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px")),
                column(width=12, box(plotlyOutput("increaseDCountry"), title = "Deaths Increase per day", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px")))
       ))
   })
   
   reactivecountry <- reactive({input$Country})
   
   output$DatabyCountry <- renderUI({
     
   reactivecountry()
   
   total_by_country1 <- total_by_country %>%
       dplyr::filter(Country_Region == input$Country)
     
   tagList(
     valueBox("Confirmed Cases", value = tags$p(paste(total_by_country1$TotalConfirmed), style = "font-size: 50%;"), width = 3),
     valueBox("Deaths", value = tags$p(paste(total_by_country1$TotalDeaths), style = "font-size: 50%;"), width = 3),
     valueBox("Death Percentage", value = tags$p(paste(total_by_country1$DeathPercentage), style = "font-size: 50%;"), width = 3)
   )
   })
   
   output$Confirmed <- renderPlotly({

       plot_ly(overall_date, x = ~Date, 
             y = ~TotalConfirmed, type = 'scatter', mode = 'lines')

   })
   
   output$deaths <- renderPlotly({
     
     plot_ly(overall_date, x = ~Date, 
             y = ~TotalDeaths, type = 'scatter', mode = 'lines')
     
   })
   
   output$deathsP <- renderPlotly({
     
     plot_ly(overall_date, x = ~Date, 
             y = ~DeathPercentage, type = 'scatter', mode = 'lines')
     
   })
   
   output$increaseConfirmed <- renderPlotly({
     
     plot_ly(overall_date, x = ~Date, 
             y = ~increaseConfirmed, type = 'scatter', mode = 'lines')
     
   })
   
   output$increaseDeaths <- renderPlotly({
     
     plot_ly(overall_date, x = ~Date, 
             y = ~increaseDeaths, type = 'scatter', mode = 'lines')
     
   })
   
   output$ConfirmedCountry <- renderPlotly({
     
     reactivecountry()
     
     country_date1 <- country_date %>%
       dplyr::filter(Country_Region == input$Country)
     
     plot_ly(country_date1, x = ~Date, 
             y = ~TotalConfirmed, type = 'scatter', mode = 'lines')
     
   })
   
   output$deathsCountry <- renderPlotly({
     
     reactivecountry()
     
     country_date1 <- country_date %>%
       dplyr::filter(Country_Region == input$Country)
     
     plot_ly(country_date1, x = ~Date, 
             y = ~TotalDeaths, type = 'scatter', mode = 'lines')
     
   })
   
   output$deathsPCountry <- renderPlotly({
     
     reactivecountry()
     
     country_date1 <- country_date %>%
       dplyr::filter(Country_Region == input$Country)
     
     plot_ly(country_date1, x = ~Date, 
             y = ~DeathPercentage, type = 'scatter', mode = 'lines')
     
   })
   
   output$increaseCCountry <- renderPlotly({
     
     reactivecountry()
     
     country_date1 <- country_date %>%
       dplyr::filter(Country_Region == input$Country)
     
     plot_ly(country_date1, x = ~Date, 
             y = ~increaseConfirmed, type = 'scatter', mode = 'lines')
     
   })
   
   output$increaseDCountry <- renderPlotly({
     
     reactivecountry()
     
     country_date1 <- country_date %>%
       dplyr::filter(Country_Region == input$Country)
     
     plot_ly(country_date1, x = ~Date, 
             y = ~increaseDeaths, type = 'scatter', mode = 'lines')
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

