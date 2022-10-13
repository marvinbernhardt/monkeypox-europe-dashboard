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

# load data for all dates
for (date_temp in avail_dates) {
  #date_temp = "2022-07-13"
  conf_cases_temp <- left_join(
    x = world_countries_shapes@data,
    y = monkeypox_df[monkeypox_df$DateRep == date_temp,],
    by = c('name' = "CountryExp"),
  )$ConfCases
  index_cases_date = as.character(date_temp)
  world_countries_shapes@data[index_cases_date] <- conf_cases_temp
  rm(conf_cases_temp)
}

# shiny user interface
ui <- fluidPage(
  # title
  titlePanel("Monkeypox Cases"),
  # sidebar with controls
  # TODO: add color legend
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "date",
        label = "Date (year 2022)",
        min = as.Date(min(avail_dates)),
        max = as.Date(max(avail_dates)),
        value = as.Date("2022-04-22"),
        timeFormat = "%m-%d"
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
  
  # function for the color to be called
  color <- eventReactive(input$date, {
    index_cases_date = as.character(input$date)
    return(palette(world_countries_shapes@data[, index_cases_date]))
  }, ignoreNULL = FALSE)
  
  # change main panel map
  output$map <- renderLeaflet({
    # leaflet map
    leaflet(data = world_countries_shapes) %>%
      
      leaflet::setView(lat = eu_lat, lng = eu_lon, zoom = eu_zoom) %>%
      
      # countries without data
      leaflet::addPolygons(
        color = "#a0a0a0",
        weight = 1,
        smoothFactor = 0.5,
        opacity = ifelse(world_countries_shapes$name %in% avail_countries, 0.0, 1.0),
        fillOpacity = 0.5,
        stroke = FALSE,
      ) %>%
      
      leaflet::addPolygons(
        # TODO, only plot those with data
        color = color(),
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