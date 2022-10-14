library(leaflet)
library(shiny)
library(dplyr)

# initial map position
eu_lat = 55
eu_lon = 10
eu_zoom = 3

# load preprocessed data
max_cases = readRDS(file = "data/max_cases.rds")
avail_dates = readRDS(file = "data/avail_dates.rds")
data_countries_shapes = readRDS(file = "data/data_countries_shapes.rds")  # contains monkeypox data
nodata_countries_shapes = readRDS(file = "data/nodata_countries_shapes.rds")

# shiny user interface
ui = shiny::fluidPage(
  # title
  shiny::titlePanel("Monkeypox Cases"),
  # sidebar with controls
  # TODO: add color legend
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::sliderInput(
        inputId = "date",
        label = "Date (year 2022)",
        min = as.Date(min(avail_dates)),
        max = as.Date(max(avail_dates)),
        value = as.Date("2022-04-22"),
        timeFormat = "%m-%d"
      )
    ),
    # map is main element
    shiny::mainPanel(
      leaflet::leafletOutput("map"),
    )
  )
)

# palette function
cases_palette = colorNumeric(
  palette = 'Reds',
  domain = c(0, max_cases), 
  na.color = '#a0a0a0',
)

# shiny server
server = function(input, output, session) {
  
  # function for the color to be called
  color = shiny::reactive({
    index_cases_date = as.character(input$date)
    return(cases_palette(data_countries_shapes@data[, index_cases_date]))
  })
  
  # change main panel map
  output$map = leaflet::renderLeaflet({
    # leaflet map
    leaflet::leaflet(data = data_countries_shapes) %>%
      
      leaflet::setView(
        lat = eu_lat,
        lng = eu_lon,
        zoom = eu_zoom,
      ) %>%
      
      # countries without data
      leaflet::addPolygons(
        data = nodata_countries_shapes,
        color = "#a0a0a0",
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 0.5,
        stroke = FALSE,
      ) %>%
    
      leaflet::addLegend(
        position = "bottomright",
        pal = cases_palette,
        values = c(0, 200),
      )
  })
  
  # fixed layer_ids lead to replacement of polygon
  layer_ids = as.character(1:length(data_countries_shapes))
  
  # dynamic part
  shiny::observe({
    proxy = leaflet::leafletProxy("map", data = data_countries_shapes)
    proxy %>%
      # add polygons with new color
      leaflet::addPolygons(
        layerId = layer_ids,
        color = color(),
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        stroke = TRUE,
      )
  })
}

# show shiny app
shiny::shinyApp(ui, server)