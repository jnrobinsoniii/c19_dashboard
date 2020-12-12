library(shiny)
library(flexdashboard)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(xts)
library(tbl2xts)
library(USAboundaries)

# Get an alphabetical list of names and abbreviations of states from the USAboundaries library to use for our State input and URLs
stateList = state_codes[c(1:52,54,57,60,63),] 
alphaStates = select(stateList[order(stateList$state_name),], state_name, state_abbr) 

ui <- dashboardPage( # Create the user interface and dashboard page using shinydashboard library
    dashboardHeader(title = "COVID-19 Timeline"),
    dashboardSidebar( 
        sidebarMenu( 
            menuItem("Plot", tabName = "Plot", icon = icon("th")), # Sidebar menu item called Plot containing 2 input sections
            selectizeInput("state", 
                           h3("State:"),
                           c("United States",
                             (alphaStates$state_name))),
            selectInput("category",
                        h3("Category:"),
                        c("Positive Cases Total", 
                          "Deaths Total", 
                          "Recovered Total", 
                          "Hospitalized Total", 
                          "Hospitalized Currently", 
                          "Negative Test Results Total", 
                          "Total Test Results", 
                          "Increase in Positive Cases", 
                          "Increase in Deaths", 
                          "Increase in Hospitalized"))
        )
    ),
    dashboardBody( 
        
        # First row in dashboard body shows current data in Value boxes from the flexdashboard library
        fluidRow(
            column(2,flexdashboard::valueBoxOutput("vb1")),
            column(2,flexdashboard::valueBoxOutput("vb2")),
            column(2,flexdashboard::valueBoxOutput("vb3")),
            column(2,flexdashboard::valueBoxOutput("vb4")),
            column(2,flexdashboard::valueBoxOutput("vb5")),
            column(2,flexdashboard::valueBoxOutput("vb6"))
        ),
        
        # Second row in dashboard body shows time series data in a plot from the highcharter library
        fluidRow(
            column(12, highchartOutput("hcontainer"))
        ),
        
        # Third row in dashboard body shows rates in gauges from the flexdashboard library
        fluidRow(
            column(2,flexdashboard::gaugeOutput("gauge1")),
            column(2,flexdashboard::gaugeOutput("gauge2")),
            column(2,flexdashboard::gaugeOutput("gauge3")),
            column(2,flexdashboard::gaugeOutput("gauge4")),
            column(2,flexdashboard::gaugeOutput("gauge5")),
            column(2,flexdashboard::gaugeOutput("gauge6"))
        )
    )
)
