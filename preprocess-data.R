library(dplyr)

# read monkeypox data
monkeypox_df = read.csv("data/monkeypox-eu.csv")
monkeypox_df$DateRep = as.Date(monkeypox_df$DateRep)

# aggregate per week
monkeypox_week_df = monkeypox_df %>% 
  group_by(week = format(DateRep, "%Y-%U"), CountryExp) %>%
  summarise_if(is.numeric, sum)
max_cases_per_week = max(monkeypox_week_df$ConfCases)

# read map data from RDS
world_countries_shapes = readRDS(file = "data/countries.rds")

# available countries
avail_countries = unique(monkeypox_df$CountryExp)

# avaliable dates
avail_weeks = sort(unique(monkeypox_week_df$week), decreasing=FALSE)

# friday of each week with data
avail_weeks_fridays = as.Date(paste(avail_weeks, '5', sep = '-'), format="%Y-%U-%u")

# shapes of countries with data
data_countries_shapes = world_countries_shapes[world_countries_shapes$name %in% avail_countries,]

# shapes of countries without data
nodata_countries_shapes = world_countries_shapes[!world_countries_shapes$name %in% avail_countries,]

# merge monkeypox data into data_countries_shapes
for (week_temp in avail_weeks) {
  conf_cases_temp = left_join(
    x = data_countries_shapes@data,
    y = monkeypox_week_df[monkeypox_week_df$week == week_temp,],
    by = c('name' = "CountryExp"),
  )$ConfCases
  data_countries_shapes@data[week_temp] = conf_cases_temp
  rm(conf_cases_temp)
}

saveRDS(max_cases_per_week, file = "data/max_cases_per_week.rds")
saveRDS(avail_weeks, file = "data/avail_weeks.rds")
saveRDS(avail_weeks_fridays, file = "data/avail_weeks_fridays.rds")
saveRDS(data_countries_shapes, file = "data/data_countries_shapes.rds")
saveRDS(nodata_countries_shapes, file = "data/nodata_countries_shapes.rds")