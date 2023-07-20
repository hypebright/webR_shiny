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
  
  # Plot stock data
  output$stock_plot <- renderPlot({
    
    ticker <- input$company
    dates <- input$dates
    
    url <- paste0(
      "https://query1.finance.yahoo.com/v8/finance/chart/",
      ticker,
      "?period1=",
      as.numeric(as.POSIXct(dates[1])),
      "&period2=",
      as.numeric(as.POSIXct(dates[2])),
      "&interval=1d"
    )
    
    # Generate a unique callback function name
    callback <- paste0("callback", as.integer(Sys.time()))
    
    # Append the callback function to the URL as a query parameter
    jsonp_url <- paste0(url, "&format=json&callback=", callback)
    
    print(jsonp_url)
    
    # Download data
    download.file(jsonp_url, "stockdata.json")
    
    print(list.files())
    
    # Parse JSON data
    parsed_data <- fromJSON("stockdata.json")
    
    # Extract the necessary data from the parsed JSON
    prices <- parsed_data$chart$result$indicators$quote[[1]]$close[[1]]
    dates <- as.Date(as.POSIXct(parsed_data$chart$result$timestamp[[1]], origin = "1970-01-01"))
    
    stock_data <- data.frame(Date = dates, Close = prices, stringsAsFactors = FALSE)
    
    print(stock_data)
    
    plot(x = stock_data$Date, 
         y = stock_data$Close, 
         type = "l", 
         col = "steelblue",
         xlab = "Date", 
         ylab = "Closing Price",
         main = paste("Stock Data for", ticker))
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
