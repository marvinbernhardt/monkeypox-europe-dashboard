library(leaflet)
library(shiny)

# initial map position
eu_lat <- 55
eu_lon <- 10
eu_zoom <- 3

# Reading polygons from json is very slow. Save as RDS once
#world_countries_shapes <- rgdal::readOGR(dsn = "data/countries.geo.json")
#saveRDS(world_countries_shapes, file = "data/countries.rds")

# read map data from RDS
world_countries_shapes = readRDS(file = "data/countries.rds")

# read smallpox data
smallpox_df <- read.csv("data/monkeypox-eu.csv")

# list available of countries
avail_countries = unique(smallpox_df$CountryExp)

# shiny user interface
ui <- fluidPage(
  # title
  titlePanel("Monkeypox Cases"),
  # sidebar with controls
  # TODO: make it a date selector
  # TODO: add color legend
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "date",
        label = "Date",
        min = 1,
        max = 50,
        value = 30,
      )
    ),
    # map is main element
    mainPanel(
      leafletOutput("map"),
    )
  )
)

# shiny server
server <- function(input, output, session) {
  # change main panel map
  output$map <- renderLeaflet({
    # leaflet map
    leaflet(world_countries_shapes) %>%
      leaflet::setView(lat = eu_lat, lng = eu_lon, zoom = eu_zoom) %>%
      leaflet::addPolygons(
        # TODO: make color depend on number of cases
        color = "#444444",
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        stroke = world_countries_shapes$name %in% avail_countries,
      )
  })
}

# show shiny app
shinyApp(ui, server)