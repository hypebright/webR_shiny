library(shiny)
library(httr)
library(jsonlite)

# Define UI
ui <- fluidPage(
  titlePanel("AEX Stock Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("company", "Select Company", choices = c("ADYEN.AS", "ASML.AS", "UNA.AS", "HEIA.AS", "INGA.AS", "RDSA.AS", "PHIA.AS", "DSM.AS", "ABN.AS", "KPN.AS")),
      dateRangeInput("dates", "Select Date Range", start = Sys.Date() - 365, end = Sys.Date()),
      actionButton("fetchData", "Fetch Data")
    ),
    mainPanel(
      plotOutput("stock_plot")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Perform the JSONP request and fetch data
  fetchStockData <- function() {
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
    
    # Make the JSONP request using httr
    jsonp_response <- GET(jsonp_url)
    
    # Extract the JSON data by removing the callback function wrapper
    json_data <- content(jsonp_response, "text", encoding = "UTF-8")
    json_data <- gsub(paste0("^", callback, "\\("), "", json_data)
    json_data <- gsub("\\)$", "", json_data)
    
    # Parse the JSON data
    parsed_data <- fromJSON(json_data)
    
    # Extract the necessary data from the parsed JSON
    prices <- parsed_data$chart$result$indicators$quote[[1]]$close[[1]]
    dates <- as.Date(as.POSIXct(parsed_data$chart$result$timestamp[[1]], origin = "1970-01-01"))
    
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
  
  # Register the fetchData button click event
  observeEvent(input$fetchData, {
    fetchStockData()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
