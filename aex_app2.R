library(shiny)
library(jsonlite)

# Define UI
ui <- fluidPage(
  titlePanel("AEX Stock Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("company", "Select Company", choices = c("ADYEN.AS", "ASML.AS", "UNA.AS", "HEIA.AS", "INGA.AS", "RDSA.AS", "PHIA.AS", "DSM.AS", "ABN.AS", "KPN.AS")),
      dateRangeInput("dates", "Select Date Range", start = Sys.Date() - 365, end = Sys.Date()),
      actionButton("fetch_data", "Fetch Data")
    ),
    mainPanel(
      plotOutput("stock_plot")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Perform the JSONP request and fetch data
  fetch_stock_data <- function() {
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
    
    # get JSONP data
    json_data <- fromJSON(jsonp_url)
    
    # Extract the necessary data from the parsed JSON
    prices <- json_data$chart$result$indicators$quote[[1]]$close[[1]]
    dates <- as.Date(as.POSIXct(json_data$chart$result$timestamp[[1]], origin = "1970-01-01"))
    
    stock_data <- data.frame(Date = dates, Close = prices, stringsAsFactors = FALSE)
    
    # Update the plot with the fetched stock data
    output$stock_plot <- renderPlot({
      plot(
        x = stock_data$Date,
        y = stock_data$Close,
        type = "l",
        col = "steelblue",
        xlab = "Date",
        ylab = "Closing Price",
        main = paste("Stock Data for", input$company)
      )
    })
  }
  
  # Register the fetch_data button click event
  observeEvent(input$fetch_data, {
    fetch_stock_data()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
