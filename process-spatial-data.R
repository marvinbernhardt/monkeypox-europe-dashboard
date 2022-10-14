library(dplyr)

# read map data from RDS
world_countries_shapes = readRDS(file = "data/countries.rds")

# read monkeypox data
monkeypox_df = read.csv("data/monkeypox-eu.csv")
max_cases = max(monkeypox_df$ConfCases)

# available countries
avail_countries = unique(monkeypox_df$CountryExp)

# avaliable dates
avail_dates = sort(unique(monkeypox_df$DateRep), decreasing=FALSE)

# shapes of countries with data
data_countries_shapes = world_countries_shapes[world_countries_shapes$name %in% avail_countries,]

# shapes of countries without data
nodata_countries_shapes = world_countries_shapes[!world_countries_shapes$name %in% avail_countries,]

# merge monkeypox data into data_countries_shapes
for (date_temp in avail_dates) {
  conf_cases_temp = left_join(
    x = data_countries_shapes@data,
    y = monkeypox_df[monkeypox_df$DateRep == date_temp,],
    by = c('name' = "CountryExp"),
  )$ConfCases
  index_cases_date = as.character(date_temp)
  data_countries_shapes@data[index_cases_date] = conf_cases_temp
  rm(conf_cases_temp)
}

saveRDS(avail_dates, file = "data/avail_dates.rds")
saveRDS(data_countries_shapes, file = "data/data_countries_shapes.rds")
saveRDS(nodata_countries_shapes, file = "data/nodata_countries_shapes.rds")