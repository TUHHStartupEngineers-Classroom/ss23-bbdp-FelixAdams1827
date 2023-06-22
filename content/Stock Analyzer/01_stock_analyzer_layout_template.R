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
source(file = "stock_analysis_functions.R")

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
                                  max     = 50, 
                                  value   = c(20), 
                                  step    = 1, 
                                  round   = TRUE),
                      
                      sliderInput(inputId = "mavg_long", 
                                  label   = "Long Moving Average", 
                                  min     = 50,
                                  max     = 150, 
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
                    div(h3("Analyst Comment")),
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
