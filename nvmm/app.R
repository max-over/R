library(shiny)

# Initialize reactive values
strx <- reactiveVal("Period, Demand, Shipment, Inventory, Costs, SL,\n")
period <- reactiveVal(0)
demand <- reactiveVal(0)
inventory_ret <- reactiveVal(0)
holdingrate_ret <- reactiveVal(4)
lostsalesrate_ret <- reactiveVal(25)
sl_ret <- reactiveVal(1.0)
lostsales_ret <- reactiveVal(0)
lostsalescount_ret <- reactiveVal(0)
costs_ret <- reactiveVal(0)
inventorycosts_ret <- reactiveVal(0)
lostsalescosts_ret <- reactiveVal(0)

# UI definition
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .card-header-customer {
        color: white !important;
        background-color: #797979 !important;
        padding: 10px;
        font-weight: bold;
        border-radius: 5px 5px 0 0;
      }
      .card-header-manufacturer {
        color: white !important;
        background-color: #004225 !important;
        padding: 10px;
        font-weight: bold;
        border-radius: 5px 5px 0 0;
      }
      .card {
        border: 1px solid #ddd;
        border-radius: 5px;
        margin-bottom: 15px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .card-body {
        padding: 15px;
      }
      .period-btn {
        color: white !important;
        background-color: #797979 !important;
      }
      .export-btn {
        color: black !important;
        background-color: #FFFFFF !important;
        border: 1px solid #ddd;
      }
    "))
  ),
  
  h1("Multiperiod inventory model", style = "font-size: 300%;"),
  
  # Customer card
  div(class = "card",
      div(class = "card-header-customer", "Customer"),
      div(class = "card-body",
          fluidRow(
            column(6, h5(textOutput("txt_demand"))),
            column(6, h5(textOutput("txt_period")))
          )
      )
  ),
  
  # Manufacturer card
  div(class = "card",
      div(class = "card-header-manufacturer", "Manufacturer"),
      div(class = "card-body",
          textOutput("out_ret_rates"),
          br(),
          fluidRow(
            column(6, 
                   div(class = "card",
                       div(class = "card-body",
                           numericInput("shipment_distr", "Shipment size", value = 100, min = 0)
                       )
                   )
            ),
            column(6,
                   div(class = "card",
                       div(class = "card-body",
                           textOutput("txt_costs_ret"),
                           br(),
                           textOutput("txt_sl_ret"),
                           br(),
                           textOutput("txt_inv_ret")
                       )
                   )
            )
          )
      )
  ),
  
  # Buttons row
  fluidRow(
    column(6,
           div(class = "card",
               div(class = "card-header", h5("Next period")),
               div(class = "card-body",
                   actionButton("period_button", "Next period", class = "period-btn")
               )
           )
    ),
    column(6,
           div(class = "card",
               div(class = "card-header", h5("Export statistics")),
               div(class = "card-body",
                   downloadButton("stats_button", "Statistics", class = "btn-default export-btn")
               )
           )
    )
  ),
  
  br(),
  
  # Footer
  div(
    style = "margin-top: 30px; text-align: center;",
    a(
      href = "https://github.com/max-over",
      target = "_blank",
      "https://github.com/max-over"
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Text outputs
  output$txt_demand <- renderText({
    paste("Demand:", demand())
  })
  
  output$txt_period <- renderText({
    paste("Period:", period() + 1)
  })
  
  output$out_ret_rates <- renderText({
    paste("Inventory holding rate:", holdingrate_ret(), ", Lost sales rate:", lostsalesrate_ret())
  })
  
  output$txt_costs_ret <- renderText({
    paste("Manufacturer costs:", costs_ret())
  })
  
  output$txt_sl_ret <- renderText({
    paste("Manufacturer SL:", round(sl_ret(), 2))
  })
  
  output$txt_inv_ret <- renderText({
    paste("Manufacturer inventory:", round(inventory_ret(), 2))
  })
  
  # Download handler for statistics
  output$stats_button <- downloadHandler(
    filename = function() {
      paste0("Inv_stat-", Sys.Date(), "-", sample(100:999, 1), ".csv")
    },
    content = function(file) {
      # Small delay to simulate async behavior
      Sys.sleep(0.25)
      writeLines(strx(), file)
    }
  )
  
  # Period button action
  observeEvent(input$period_button, {
    # Update inventory by adding shipment
    inventory_ret(inventory_ret() + input$shipment_distr)
    
    # Increment period
    period(period() + 1)
    
    # Generate random demand (normal distribution with mean 100, sd 30)
    rnd_num <- max(round(rnorm(1, mean = 100, sd = 30)), 0)
    demand(rnd_num)
    
    # Calculate costs based on inventory vs demand
    if (inventory_ret() >= demand()) {
      # Excess inventory costs
      inventorycosts_ret(
        inventorycosts_ret() + (inventory_ret() - demand()) * holdingrate_ret()
      )
      # Reduce inventory by demand
      inventory_ret(inventory_ret() - demand())
    } else {
      # Lost sales costs
      lostsalescosts_ret(
        lostsalescosts_ret() + (demand() - inventory_ret()) * lostsalesrate_ret()
      )
      lostsalescount_ret(lostsalescount_ret() + 1)
      # Inventory becomes 0 when demand exceeds inventory
      inventory_ret(0)
    }
    
    # Update total costs and service level
    costs_ret(round(inventorycosts_ret() + lostsalescosts_ret(), 0))
    sl_ret(round(1 - lostsalescount_ret() / period(), 2))
    
    # Update CSV string with inventory data
    new_str <- paste0(
      strx(),
      period(), ",", 
      as.integer(demand()), ",",
      as.integer(input$shipment_distr), ",",
      as.integer(inventory_ret()), ",",
      as.integer(costs_ret()), ",",
      sl_ret(), ",\n"
    )
    strx(new_str)
  })
}

# Run the app
shinyApp(ui = ui, server = server)