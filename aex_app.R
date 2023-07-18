
library(shiny)
library(jsonlite)

# Define UI
ui <- fluidPage(
  titlePanel("AEX Stock Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("company", "Select Company", choices = c("ADYEN.AS", "ASML.AS", "UNA.AS", "HEIA.AS", "INGA.AS", "RDSA.AS", "PHIA.AS", "DSM.AS", "ABN.AS", "KPN.AS")),
      dateRangeInput("dates", "Select Date Range", start = Sys.Date() - 365, end = Sys.Date())
    ),
    mainPanel(
      plotOutput("stock_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Retrieve stock data
  stockData <- reactive({
    
    ticker <- input$company
    dates <- input$dates
    
    getStockData <- function(symbol, start_date, end_date) {
      url <- paste0(
        "https://query1.finance.yahoo.com/v8/finance/chart/",
        symbol,
        "?period1=",
        as.numeric(as.POSIXct(start_date)),
        "&period2=",
        as.numeric(as.POSIXct(end_date)),
        "&interval=1d"
      )
      
      json_data <- jsonlite::fromJSON(url)
      
      prices <- json_data$chart$result$indicators$quote[[1]]$close[[1]]
      dates <- as.Date(as.POSIXct(json_data$chart$result$timestamp[[1]], origin = "1970-01-01"))
      
      data.frame(Date = dates, Close = prices, stringsAsFactors = FALSE)
    }
    
    getStockData(ticker, dates[1], dates[2])
    
  })
  
  # Plot stock data
  output$stock_plot <- renderPlot({
    
    req(stockData)
    
    thisStock <- stockData()
    
    plot(x = thisStock$Date, 
         y = thisStock$Close, 
         type = "l", 
         col = "steelblue",
         xlab = "Date", 
         ylab = "Closing Price",
         main = paste("Stock Data for", thisStock$ticker))
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
