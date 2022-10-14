library(leaflet)
library(shiny)
library(dplyr)

# initial map position
eu_lat = 55
eu_lon = 10
eu_zoom = 3

# load preprocessed data
max_cases_per_week = readRDS(file = "data/max_cases_per_week.rds")
avail_weeks = readRDS(file = "data/avail_weeks.rds")
avail_weeks_fridays = readRDS(file = "data/avail_weeks_fridays.rds")
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
        label = "Select a date",
        min = min(avail_weeks_fridays),
        max = max(avail_weeks_fridays),
        value = as.Date("2022-04-22"),
        timeFormat = "%d %b %Y",
        step = 7,
      ),
      shiny::p(shiny::textOutput("time_description")),
      shiny::p(
        "Generated with ",
        shiny::a(
          "publicly available data",
          href = "https://www.ecdc.europa.eu/en/publications-data/data-monkeypox-cases-eueea"
        ),
        " from the ECDC.",
      )
    ),
    # map is main element
    shiny::mainPanel(
      leaflet::leafletOutput(
        outputId = "map",
        height = 600,
      ),
    )
  )
)

# palette functions
#palette_colors = 'Reds'
#palette_colors = c('#e8e8e8', '#c0c040', '#d02020', '#c04080')
palette_colors = 'Reds'
cases_palette = leaflet::colorNumeric(
  palette = palette_colors,
  domain = c(0, max_cases_per_week), 
  na.color = '#a0a0a0',
)
cases_palette_rev = leaflet::colorNumeric(
  palette = palette_colors,
  domain = c(0, max_cases_per_week), 
  na.color = '#a0a0a0',
  reverse = TRUE,
)

# shiny server
server = function(input, output, session) {
  
  # reactive function for the color
  color = shiny::reactive({
    input_week = strftime(input$date, "%Y-%U")
    if (input_week %in% names(data_countries_shapes)) {
      case_numbers = data_countries_shapes@data[, input_week]
    } else {
      # this should never happen, but anyway
      case_numbers = rep(NaN, length(data_countries_shapes))
    }
    return(cases_palette(case_numbers))
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
        smoothFactor = 0.6,
        opacity = 1.0,
        fillOpacity = 0.5,
        stroke = FALSE,
      ) %>%
      
      # add legend
      leaflet::addLegend(
        position = "bottomright",
        pal = cases_palette_rev,
        values = c(0, max_cases_per_week),
        title = 'Cases per week',
        labFormat = leaflet::labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
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
        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        stroke = TRUE,
      )
  })
  
  # generate time info below slider
  output$time_description = renderText(
    expr = {
      from = input$date + as.difftime("07", format = "%d")
      to = input$date
      week = format(input$date, "%U")
      time_info = sprintf("Showing the cases reported from %s to %s (week number %s)", from, to, week)
      return(time_info)
    }
  )
}

# show shiny app
shiny::shinyApp(ui, server)