library(leaflet)
library(shiny)
library(dplyr)

# initial map position
eu_lat <- 55
eu_lon <- 10
eu_zoom <- 3

# read map data from RDS
world_countries_shapes = readRDS(file = "data/countries.rds")

# read monkeypox data
monkeypox_df <- read.csv("data/monkeypox-eu.csv")
max_cases = max(monkeypox_df$ConfCases)

# available countries
avail_countries = unique(monkeypox_df$CountryExp)

# avaliable dates
avail_dates = sort(unique(monkeypox_df$DateRep), decreasing=FALSE)

# TODO: merge data
for (date_temp in avail_dates) {
  #date_temp = "2022-07-13"
  conf_cases_temp <- left_join(
    x = world_countries_shapes@data,
    y = monkeypox_df[monkeypox_df$DateRep == date_temp,],
    by = c('name' = "CountryExp"),
  )$ConfCases
  index_cases_date = paste("ConfCases", date_temp, sep='_')
  world_countries_shapes@data[index_cases_date] <- conf_cases_temp
}

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

# palette function
palette = colorNumeric(
  palette = 'Reds',
  domain = c(0, max_cases), 
  na.color = '#a0a0a0',
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
        color = palette(world_countries_shapes@data[,"ConfCases_2022-07-10"]),
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