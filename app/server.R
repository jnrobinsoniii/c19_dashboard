

library(shiny)
library(flexdashboard)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(xts)
library(tbl2xts)
library(USAboundaries)

server <- shinyServer(function(input, output, session) {
    
    stits <- reactive({
        dita <- data.frame()
        dita <- (filter(alphaStates, state_name == input$state))
        dita$state_abbr <- tolower(dita$state_abbr)
        dita
    })
    
    # Create reactive element called "category" that filters the plot shown
    
    category <- reactive({
        outcome = switch(input$category,
                         "Positive Cases Total" = "positive",
                         "Deaths Total" = "death", 
                         "Recovered Total" = "recovered",
                         "Hospitalized Total" = "hospitalizedCumulative", 
                         "Hospitalized Currently" = "hospitalizedCurrently", 
                         "Negative Test Results Total" = "negative", 
                         "Total Test Results" = "totalTestResults", 
                         "Increase in Positive Cases" = "positiveIncrease", 
                         "Increase in Deaths" = "deathIncrease", 
                         "Increase in Hospitalized" = "hospitalizedIncrease")
        outcome
    })
    
    # Create reactive element that imports the data from COVID Tracking Project API based on value of State input.
    
    frameDaily <- reactive({
        if (input$state == "United States"){
            df <- data_frame(read.csv(paste0("https://api.covidtracking.com/v1/us/daily.csv"))) # Create dataframe based on data imported 
        } else {
            df <- data_frame(read.csv(paste0("https://api.covidtracking.com/v1/states/", stits()$state_abbr, "/daily.csv")))
        }
        df$date <- as.Date(as.character(df$date), format = "%Y%m%d")  # format the date column of dataframe
        df <- select(df, one_of(c("date", toString(category())))) 
        df <- 
            df %>% 
            tbl_xts(., cols_to_xts = toString(category()))
        df
    })
    
    frameCurrent <- reactive({
        if (input$state == "United States"){
            df <- data_frame(read.csv(paste0("https://api.covidtracking.com/v1/us/current.csv"))) # Create dataframe based on current data for the Value Boxes
        } else {
            df <- data_frame(read.csv(paste0("https://api.covidtracking.com/v1/states/", stits()$state_abbr, "/current.csv")))
        }
        df
    })
    
    frameRates <- reactive({
        if (input$state == "United States"){
            df <- data_frame(read.csv(paste0("https://api.covidtracking.com/v1/us/daily.csv"))) # Create dataframe based on daily data for the Rates
        } else {
            df <- data_frame(read.csv(paste0("https://api.covidtracking.com/v1/states/", stits()$state_abbr, "/daily.csv")))
        }
        df$date <- as.Date(as.character(df$date), format = "%Y%m%d")  # format the date column of the dataframe
        print(df)
        df
    })
    
    pop <- reactive({
        df <- data_frame(read.csv("https://raw.githubusercontent.com/jnrobinsoniii/c19_dashboard/master/state_pop.csv", header = TRUE))
        df <- filter(df, state == input$state)
        print(df)
        df
    })
    
    ratePositive <- reactive({
        df <- slice(frameRates(), 1:7)
        sum1 = sum(df$positive)
        sum2 = sum(df$negative)
        rate = signif((sum1 / (sum1 + sum2)) * 100, digits = 2)
        print(rate)
        rate
    })
    
    rateTest <- reactive({
        df <- slice(frameRates(), 1:7)
        sum1 = sum(df$positiveIncrease)
        sum2 = sum(df$negativeIncrease)
        rate = signif(((sum1 + sum2)/(pop()$pop)) * 100, digits = 2)
        print(rate)
        rate
    })
    
    rateDeath <- reactive({
        df <- slice(frameRates(), 1:7)
        sum1 = sum(df$deathIncrease)
        sum2 = sum(df$positiveIncrease)
        rate = signif((sum1/sum2) * 100, digits = 2)
        print(rate)
        rate
    })
    
    # Value Boxes for totals
    
    output$vb1 <- renderValueBox({
        valueBox(format(frameCurrent()$positive, big.mark = ","), subtitle = "Positive Cases", color = "light-blue")
        
    })
    output$vb2 <- renderValueBox({
        valueBox(format(frameCurrent()$death, big.mark = ","), subtitle = "Deaths", color = "light-blue")
    })
    
    output$vb3 <- renderValueBox({
        valueBox(format(frameCurrent()$hospitalizedCurrently, big.mark = ","), subtitle = "Hospitalized Currently", color = "light-blue")
    })
    
    output$vb4 <- renderValueBox({
        valueBox(format(frameCurrent()$hospitalizedCumulative, big.mark = ","), subtitle = "Hospitalized Total", color = "light-blue")
    })
    
    output$vb5 <- renderValueBox({
        valueBox(format(frameCurrent()$recovered, big.mark = ","), subtitle = "Recovered", color = "light-blue")
    })
    
    output$vb6 <- renderValueBox({
        valueBox(format(frameCurrent()$positiveIncrease, big.mark = ","), subtitle = "New Cases", color = "light-blue")
    })
    
    # Gauges for Rates
    
    ratePositive <- reactive({
        df <- slice(frameRates(), 1:7)
        sum1 = sum(df$positive)
        sum2 = sum(df$negative)
        rate = signif((sum1 / (sum1 + sum2)) * 100, digits = 2)
        print(rate)
        rate
    })
    
    output$gauge1 <- renderGauge({
        gauge(ratePositive(), 
              min = 0,
              max = 100,
              symbol = '%',
              label = "Positivity Rate")
    })
    
    output$gauge2 <- renderGauge({
        gauge(signif((frameCurrent()$positive / (frameCurrent()$negative + frameCurrent()$positive))*100, digits = 2), 
              min = 0,
              max = 100,
              symbol = '%',
              label = "Total Positive Rate")
    })
    
    output$gauge3 <- renderGauge({
        gauge(rateDeath, 
              min = 0,
              max = 100,
              symbol = '%',
              label = "Fatality Rate")
    })
    
    output$gauge4 <- renderGauge({
        gauge(signif((frameCurrent()$death/(frameCurrent()$positive))*100, digits = 2), 
              min = 0,
              max = 100,
              symbol = '%',
              label = "Total Fatality Rate")
    })
    
    output$gauge5 <- renderGauge({
        gauge(rateTest(), 
              min = 0,
              max = 100,
              symbol = '%',
              label = "Recent Test Rate")
    })
    
    output$gauge6 <- renderGauge({
        gauge(signif((frameCurrent()$hospitalizedCumulative/(frameCurrent()$positive))*100, digits = 2), 
              min = 0,
              max = 100,
              symbol = '%',
              label = "Hospitalization Rate")
    })
    
    # Highchart plot
    
    output$hcontainer <- renderHighchart({
        lang <- getOption("highcharter.lang")
        lang$decimalPoint <- "."
        options(highcharter.lang = lang)
        lang
        
        if (input$state == "United States"){
            hcopts <- getOption("highcharter.options")
            hcopts
            hcopts$lang$thousandsSep <- ","
            options(highcharter.options = hcopts)
            hc <- highchart(type = "stock") %>%
                hc_plotOptions(series = list(
                    allowPointSelect = TRUE,
                    cursor = "pointer",
                    point = list(
                        events = list(
                            click = JS("function(event) {window.open('https://covidtracking.com/data')}")
                        )
                    )
                )) %>%
                hc_rangeSelector(enabled = TRUE) %>%
                hc_add_series(name = ("United States"), data = frameDaily())
            hc
        } else {
            hc <- highchart(type = "stock") %>%
                hc_plotOptions(series = list(
                    allowPointSelect = TRUE,
                    cursor = "pointer",
                    point = list(
                        events = list(
                            click = JS("function(event) {window.open('https://covidtracking.com/data/state/' + this.series.name + '#historical')}")
                        )
                    )
                )) %>%
                hc_rangeSelector(enabled = TRUE) %>%
                hc_add_series(name = (stits()$url), data = frameDaily())
            hc
        }
    })
})
