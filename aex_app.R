
library(shiny)
library(httr)
library(jsonlite)

# Function to retrieve stock data
getStockData <- function(symbol, start_date, end_date) {
  
  url <- paste0("https://query1.finance.yahoo.com/v8/finance/chart/", symbol, "?period1=", 
                as.numeric(as.POSIXct(start_date)), "&period2=", as.numeric(as.POSIXct(end_date)), 
                "&interval=1d")
  
  response <- GET(url)
  json_data <- fromJSON(content(response, as = "text"))
  prices <- json_data$chart$result$indicators$quote[[1]]$close[[1]]
  dates <- as.Date(as.POSIXct(json_data$chart$result$timestamp[[1]], origin = "1970-01-01"))
  
  data.frame(Date = dates, Close = prices, stringsAsFactors = FALSE)
  
}

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
  
  print("running Shiny app server")
  
  # Retrieve stock data
  stockData <- reactive({
    ticker <- input$company
    dates <- input$dates
    getStockData(ticker, dates[1], dates[2])
  })
  
  # Plot stock data
  output$stock_plot <- renderPlot({
    
    stock <- stockData()
    
    plot(x = stock$Date, 
         y = stock$Close, 
         type = "l", 
         col = "steelblue",
         xlab = "Date", 
         ylab = "Closing Price",
         main = paste("Stock Data for", stock$ticker))
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
