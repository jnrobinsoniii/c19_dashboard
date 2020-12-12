

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

server <- shinyServer(function(input, output, session) {
    
    # Create reactive data frame that responds to State input value to obtain the proper state abbreviation for the URL
    state_abbr <- reactive({
        dita <- data.frame()
        dita <- (filter(alphaStates, state_name == input$state))
        dita$state_abbr <- tolower(dita$state_abbr)
        dita
    })
    
    
    # Create reactive element that responds to Category input to filter our data to the category shown
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
    
    # Create reactive elements that imports the current and historical data from COVID Tracking Project API based on value of State input for the plot. 
    
    # Plot data
    frameDaily <- reactive({
        # Create dataframe
        if (input$state == "United States"){
            df <- data_frame(read.csv(paste0("https://api.covidtracking.com/v1/us/daily.csv"))) 
        } else {
            df <- data_frame(read.csv(paste0("https://api.covidtracking.com/v1/states/", state_abbr()$state_abbr, "/daily.csv")))
        }
        # Format the date column of dataframe
        df$date <- as.Date(as.character(df$date), format = "%Y%m%d")
        df <- select(df, one_of(c("date", toString(category())))) 
        df <- 
            df %>% 
            tbl_xts(., cols_to_xts = toString(category()))
        df
    })
    
    # Value box data
    frameCurrent <- reactive({
        # Create dataframe
        if (input$state == "United States"){
            df <- data_frame(read.csv(paste0("https://api.covidtracking.com/v1/us/current.csv")))
        } else {
            df <- data_frame(read.csv(paste0("https://api.covidtracking.com/v1/states/", state_abbr()$state_abbr, "/current.csv")))
        }
        df
    })
    
    # Rate data
    frameRates <- reactive({
        #Create dataframe
        if (input$state == "United States"){
            df <- data_frame(read.csv(paste0("https://api.covidtracking.com/v1/us/daily.csv")))
        } else {
            df <- data_frame(read.csv(paste0("https://api.covidtracking.com/v1/states/", state_abbr()$state_abbr, "/daily.csv")))
        }
        # Format the date column of the dataframe
        df$date <- as.Date(as.character(df$date), format = "%Y%m%d")
        df
    })
    
    # Create a reactive element to get the populations of the nation, states, and territories
    pop <- reactive({
        df <- data_frame(read.csv("https://raw.githubusercontent.com/jnrobinsoniii/c19_dashboard/master/state_pop.csv", header = TRUE))
        df <- filter(df, state == input$state)
        df
    })
    
    # Create reactive elements that uses frame rate data to generate the values for the gauges
    
    # Create function to generate positivity rate based on last week.
    ratePositive <- reactive({
        df <- slice(frameRates(), 1:7)
        sum1 = sum(df$positive)
        sum2 = sum(df$negative)
        rate = signif((sum1 / (sum1 + sum2)) * 100, digits = 2)
        rate
    })
    
    # Create function to generate testing rate based on last week.
    rateTest <- reactive({
        df <- slice(frameRates(), 1:7)
        sum1 = sum(df$positiveIncrease)
        sum2 = sum(df$negativeIncrease)
        rate = signif(((sum1 + sum2)/(pop()$pop)) * 100, digits = 2)
        rate
    })
    
    # Create function to generate death rate based on last week.
    rateDeath <- reactive({
        df <- slice(frameRates(), 1:7) 
        sum1 = sum(df$deathIncrease)
        sum2 = sum(df$positiveIncrease)
        rate = signif((sum1/sum2) * 100, digits = 2)
        rate
    })
    
    # Value boxes output for current data and totals
    
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
    
    output$gauge1 <- renderGauge({
        gauge(ratePositive(), 
              min = 0,
              max = 100,
              symbol = '%',
              label = "Past Positivity Rate")
    })
    
    output$gauge2 <- renderGauge({
        gauge(signif((frameCurrent()$positive / (frameCurrent()$negative + frameCurrent()$positive))*100, digits = 2), 
              min = 0,
              max = 100,
              symbol = '%',
              label = "Current Positive Rate")
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
            hcopts <- getOption("highcharter.lang")
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
                hc_add_series(name = (state_abbr()$url), data = frameDaily())
            hc
        }
    })
})
