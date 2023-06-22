# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(rvest)
library(glue)
library(fs)
library(quantmod)

# 1.0 GET STOCK LIST ----
get_stock_list <- function(stock_index = "DAX") {
  
  # Control for upper and lower case
  index_lower <- str_to_lower(stock_index)
  # Control if user input is valid
  index_valid <- c("dax", "sp500", "dow", "nasdaq")
  if (!index_lower %in% index_valid) {stop(paste0("x must be a character string in the form of a valid exchange.",
                                                  " The following are valid options:\n",
                                                  stringr::str_c(str_to_upper(index_valid), collapse = ", ")))
  }
  
  # Control for different currencies and different column namings in wiki
  vars <- switch(index_lower,
                 dax    = list(wiki     = "DAX", 
                               columns  = c("Ticker", "Company")),  # changed here "Ticker symbol" to "Ticker"
                 sp500  = list(wiki     = "List_of_S%26P_500_companies", 
                               columns  = c("Symbol", "Security")),
                 dow    = list(wiki     = "Dow_Jones_Industrial_Average",
                               columns  = c("Symbol", "Company")),
                 nasdaq = list(wiki     = "NASDAQ-100",
                               columns  = c("Ticker", "Company"))
  )
  
  # Extract stock list depending on user input
  read_html(glue("https://en.wikipedia.org/wiki/{vars$wiki}")) %>% 
    
    # Extract table from wiki
    html_nodes(css = "#constituents") %>% 
    html_table() %>% 
    dplyr::first() %>% 
    as_tibble(.name_repair = "minimal") %>% 
    # Select desired columns (different for each article)
    dplyr::select(vars$columns) %>% 
    # Make naming identical
    set_names(c("symbol", "company")) %>% 
    
    # Clean (just relevant for DOW)
    mutate(symbol = str_remove(symbol, "NYSE\\:[[:space:]]")) %>% 
    
    # Sort
    arrange(symbol) %>%
    # Create the label for the dropdown list (Symbol + company name)
    mutate(label = str_c(symbol, company, sep = ", ")) %>%
    dplyr::select(label)
  
}

# example function calls:
stock_list_tbl <- get_stock_list("sp500")
stock_list_tbl <- get_stock_list("dow")
stock_list_tbl <- get_stock_list("nasdaq")
stock_list_tbl <- get_stock_list()
stock_list_tbl



# 2.0 EXTRACT SYMBOL BASED ON USER INPUT ----
user_input <- "AAPL, Apple Inc."
test <- user_input %>% stringr::str_split(pattern=", ") %>% purrr::pluck(1, 1)
test

#function definition:
get_symbol_from_user_input <- function(user_input){
  user_input %>% stringr::str_split(pattern=", ") %>% purrr::pluck(1, 1)
}
#test function calls:
"ADS.DE, Adidas" %>% get_symbol_from_user_input()
"AAPL, Apple Inc." %>% get_symbol_from_user_input()


# 3.0 GET STOCK DATA ----

# Retrieve market data
get_stock_data <- function(stock_symbol, 
                           from = today() - days(180), 
                           to   = today(), 
                           mavg_short = 20, mavg_long = 50) {
  stock_symbol %>% quantmod::getSymbols(
    src         = "yahoo", 
    from        = from, 
    to          = to, 
    auto.assign = FALSE) %>% 
    
    # Convert to tibble
    timetk::tk_tbl(preserve_index = T, 
                   silent         = T) %>% 
    
    # Add currency column (based on symbol)
    mutate(currency = case_when(
      str_detect(names(.) %>% last(), ".DE") ~ "EUR",
      TRUE                                   ~ "USD")) %>% 
    
    # Modify tibble 
    set_names(c("date", "open", "high", "low", "close", "volume", "adjusted", "currency")) %>% 
    drop_na() %>%
    
    # Convert the date column to a date object (I suggest a lubridate function)
    dplyr::mutate(date = lubridate::ymd(date)) %>% 
    
    # Add the moving averages
    # name the columns mavg_short and mavg_long
    dplyr::mutate(mavg_short = rollmean(adjusted, mavg_short,  fill = NA, align = "right")) %>% 
    dplyr::mutate(mavg_long = rollmean(adjusted, mavg_long,  fill = NA, align = "right")) %>% 
    
    # Select the date and the adjusted column
    dplyr::select(date, adjusted, mavg_short, mavg_long, currency)
}
stock_data_tbl <- get_stock_data("AAPL", from = "2020-06-01", to = "2021-01-12", mavg_short = 5, mavg_long = 8)

# 4.0 PLOT STOCK DATA ----
currency_format <- function(currency) {
  if (currency == "USD") 
  { x <- scales::dollar_format(largest_with_cents = 10) }
  if (currency == "EUR")   
  { x <- scales::dollar_format(prefix = "", suffix = " €",
                               big.mark = ".", decimal.mark = ",",
                               largest_with_cents = 10)}
  return(x)
}
plot_stock_data <- function(stock_data){
  g <- stock_data %>% 
    
    # convert to long format
    pivot_longer(cols = c(adjusted, mavg_short, mavg_long), 
                 names_to    = "legend", 
                 values_to    = "value", 
                 names_ptypes = list(legend = factor())) %>% 
    
    # ggplot
    ggplot(aes(x=date, y = value, group = legend)) +
    geom_line(aes(linetype = legend, color=legend)) +
    scale_y_continuous(labels = stock_data %>% pull(currency) %>% first() %>% currency_format()) +
    labs(y = "Adjusted Share Price", x = "")
  ggplotly(g)
}

#example function calls:
"ADS.DE" %>% 
  get_stock_data() %>%
  plot_stock_data()

# 5.0 GENERATE COMMENTARY ----
generate_commentary <- function(data, user_input) {
  warning_signal <- data %>%
    tail(1) %>% # Get last value
    mutate(mavg_warning_flag = mavg_short < mavg_long) %>% # insert the logical expression
    pull(mavg_warning_flag)
  
  n_short <- data %>% pull(mavg_short) %>% is.na() %>% sum() + 1
  n_long  <- data %>% pull(mavg_long) %>% is.na() %>% sum() + 1
  
  if (warning_signal) {
    str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day moving average, indicating negative trends")
  } else {
    str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day moving average, indicating positive trends")
  }
}

generate_commentary(stock_data_tbl, user_input = user_input)



# UI ----
ui <- fluidPage(title = "Stock Analyzer",
                
                # 1.0 HEADER ----
                div(
                  h1("Stock Analyzer"),
                ),
                # 2.0 APPLICATION UI -----
                div(
                  column(
                    width = 4,
                    wellPanel(
                      
                      pickerInput(inputId = "index_selection",
                                  label = "Stock Index", 
                                  choices = c("DAX", "SP500", "DOW", "NASDAQ"),
                                  select = "DAX"
                      ),
                      uiOutput("indices"),            
                      pickerInput(inputId = "stock_selection",
                                  label="Stocks",
                                  choices = stock_list_tbl$label,
                                  options = list(
                                    multiple=F,
                                    actionsBox = FALSE,
                                    liveSearch = TRUE,
                                    size = 10
                                  )
                      ),
                      dateRangeInput(inputId = "date_range", 
                                     label   = h4("Date Range"), 
                                     start   = today() - 180, 
                                     end     = today(),        
                                     #min     = today() - 300, 
                                     #max     = today(), 
                                     startview = "year"),
                      
                      actionButton(inputId = "analyze",
                                   label="Analyze",
                                   icon = icon("download")),
                      hr(),
                      
                      sliderInput(inputId = "mavg_short", 
                                  label   = "Short Moving Average", 
                                  min     = 5,
                                  max     = 40, 
                                  value   = c(20), 
                                  step    = 1, 
                                  round   = TRUE),
                      
                      sliderInput(inputId = "mavg_long", 
                                  label   = "Long Moving Average", 
                                  min     = 50,
                                  max     = 120, 
                                  value   = c(50), 
                                  step    = 1, 
                                  round   = TRUE)
                    )
                  ), 
                  column(
                    width = 8,
                    div(h4(textOutput(outputId = "plot_header"))),
                    div(plotlyOutput(outputId = "plotly_plot"))
                  )
                ),
                
                
                # 3.0 ANALYST COMMENTARY ----
                div(
                  column(
                    width = 12,
                    div(h3("Analyst Commentary")),
                    div(textOutput(outputId = "commentary"))
                  )
                )
)

# SERVER ----
server <- function(input, output, session) {
  # Stock Symbol ----
  
  stock_list_tbl <- reactive({
    get_stock_list(input$index_selection)
    
  })
  
  output$indices <- renderUI({
    pickerInput(inputId = "stock_selection",
                label="Stocks",
                choices = stock_list_tbl() %>% purrr::pluck("label"),
                select = stock_list_tbl() %>% purrr::pluck("label") %>% purrr::pluck(1),
                options = list(
                  multiple=F,
                  actionsBox = FALSE,
                  liveSearch = TRUE,
                  size = 10
                )
    )
  })
  
  stock_symbol <- eventReactive(ignoreNULL = FALSE, input$analyze, {
    input$stock_selection
  })
  
  stock_data_tbl <- reactive({
    
    stock_symbol() %>%
      get_symbol_from_user_input %>%
      get_stock_data(from = input$date_range[1],
                     to   = input$date_range[2],
                     mavg_short = input$mavg_short,
                     mavg_long  = input$mavg_long)
    
  })
  
  output$plot_header <- renderText({stock_symbol()})
  
  output$stock_data <- renderPrint({stock_data_tbl()})
  
  output$plotly_plot <- renderPlotly({plot_stock_data(stock_data_tbl())})
  
  output$commentary <- renderPrint({generate_commentary(stock_data_tbl(), user_input = stock_symbol())})
}

# RUN APP ----

shinyApp(ui = ui, server = server)