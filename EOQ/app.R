
library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style("#eoq {font-size:48px }"),

    titlePanel("EOQ model"),

    sidebarLayout(
        sidebarPanel("Model input",
          numericInput('holding_costs', h3("Holding costs"), value = 2),
          numericInput('transportation_costs', h3("Transportation costs"), value = 5),
          numericInput('demand', h3("Demand"), value = 1000),
        ),


        mainPanel("Model output",
                  h1("Economic Order Size:"),
                  textOutput("eoq"),
                  
        )
    )
)


server <- function(input, output) {

  output$eoq <- renderText(
    
     sqrt(input$demand*input$transportation_costs/input$holding_costs))


}
# Run the application 
shinyApp(ui = ui, server = server)
