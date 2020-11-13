library(shiny)
library(flexdashboard)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(xts)
library(tbl2xts)
library(USAboundaries)

stateList = state_codes[c(1:52,54,57,60,63),] # Get a list of names and abbreviations of states
alphaStates = select(stateList[order(stateList$state_name),], state_name, state_abbr) # Sort alphabetically, Select name and abbreviations

ui <- dashboardPage(
    dashboardHeader(title = "COVID-19 Timeline"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Plot", tabName = "Plot", icon = icon("th")),
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
        fluidRow(
            column(2,flexdashboard::valueBoxOutput("vb1")),
            column(2,flexdashboard::valueBoxOutput("vb2")),
            column(2,flexdashboard::valueBoxOutput("vb3")),
            column(2,flexdashboard::valueBoxOutput("vb4")),
            column(2,flexdashboard::valueBoxOutput("vb5")),
            column(2,flexdashboard::valueBoxOutput("vb6"))
        ),
        fluidRow(
            column(12, highchartOutput("hcontainer"))
        ),
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