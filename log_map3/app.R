
library(shiny)
library(ggplot2)


ui <- fluidPage(

    titlePanel("Logistic Map Model"),
    mainPanel(
      sliderInput('r_param',h3("R parameter value"), min = 1, max = 4, step = 0.02, width = '100%', value = 3.6),
      plotOutput("plot2")
    )
)

server <- function(input, output) {
observe({
lamda <- input$r_param  
vals <- c(0.5)
iter <- seq(1,100,1)

for (i in iter){

  vals[(i+1)] <- lamda * vals[i] * (1 - vals[i])
  
}
  
vals <- vals[-length(vals)]

x <- tibble::tibble(vals, iter)

output$plot2 <- renderPlot(ggplot(x)+
                             aes(x = iter, y = vals) +
                             geom_line(colour = "#0C4C8A") +
                             labs(x = "iteration", y = "Value", title = "Logistic Map") +
                             ylim(0,1) +
                             theme_minimal()
                    
                           )
  
})
}

shinyApp(ui = ui, server = server)
