library(shiny)
library(leaflet)
library(readxl)
library(writexl)
library(dplyr)
library(tidygeocoder)
library(osrm)
library(mapview)
library(sf)
library(stplanr)


ui <- fluidPage(
  
  titlePanel(title = "Routing example"),
  
  sidebarLayout(sidebarPanel(
    fileInput('file1', 'Choose xlsx file with locations coordinates', accept = c(".xlsx")),
    selectInput("mode", "Choose a delivery mode:", list("car", "bike", "foot")),
    h4("Route Building"),
    actionButton('button_route', "Build routes"),
    h4(" "),
    downloadButton('dl', "Download trip"),
    h4("Reverse Geocoding"),
    actionButton('button_rev', "Reverse Geocode"),
    h4(" "),
    downloadButton('dl2', "Download Rev.Geocode"),
    
  ),
  mainPanel(
    h4(p("Route Distance: ", textOutput(outputId = "distance", inline = T)," kilometers")),
    h4(p("Route Duration: ", textOutput(outputId = "duration", inline = T)," minutes")),
    h4(p("Locations: ", textOutput(outputId = "rows", inline = T))),
    leafletOutput("mymap"),
    fluidRow(
      splitLayout(cellWidths = c("45%","5%","50%"), tags$b("Locations lists"), " ", tags$b("Route"))
    ),
    fluidRow(
      splitLayout(cellWidths = c("45%","5%","50%"), tableOutput('file1'), " ", tableOutput('file3'))
    )
  )
  )
   
)


server <- function(input, output, session) {
  
  output$file1 <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read_excel(inFile$datapath)
  })
  
  observe({
    req(c(input$file1))
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    exceldata <- read_excel(inFile$datapath)
    df = data.frame(exceldata)
    last_row <-nrow(df)
    output$rows <- renderText(last_row)
    
    mymap_new2 <- st_as_sf(df, coords = c("long","lat"), crs = 4326)
    m2 <- mapview(mymap_new2, legend = FALSE)
    output$mymap <- renderLeaflet({ m2@map})
    
    observeEvent(input$button_rev,{
      df3 <<- df %>%
        reverse_geocode(lat=lat, long = long, method = 'osm', address = address_found, full_results = FALSE) %>%
        select(client, lat, long, address_found)
      output$file1 <- renderTable(as.data.frame(df3))
    })
    
    observeEvent(input$button_route,{
      stops_hse <- st_as_sf(df, coords = c("long","lat"), crs = "WGS84")
      osrmTable_hse <- osrmTable(stops_hse[1:last_row,], stops_hse[1:last_row,], osrm.profile = input$mode )
      trips_hse <- osrmTrip(loc = stops_hse[1:last_row,], osrm.profile = input$mode)
      mytrip2 <<- trips_hse[[1]]$trip
      mytrip_duration <- round(trips_hse[[1]]$summary$duration, digits = 2)
      mytrip_distance <- round(trips_hse[[1]]$summary$distance, digits = 2)
      output$duration <- renderText(mytrip_duration)
      output$distance <- renderText(mytrip_distance)
      trip_tibble <- tibble(mytrip2) %>% select(-geometry)
      output$file3 <- renderTable(as.data.frame(trip_tibble))
      
      mymap_new <- st_as_sf(df, coords = c("long","lat"), crs = 4326)
      m <- mapview(mytrip2) + mapview(mymap_new, legend = FALSE)
      output$mymap <- renderLeaflet({ m@map})
      })
    
  })

  output$dl <- downloadHandler(
    filename = function() {"mytrip_test.xlsx"},
    content = function(file) {write_xlsx(mytrip2,path = file)}
  )
  output$dl2 <- downloadHandler(
    filename = function() {"rev_geocode_test.xlsx"},
    content = function(file) {write_xlsx(df3,path = file)}
  )
}


shinyApp(ui = ui, server = server)
