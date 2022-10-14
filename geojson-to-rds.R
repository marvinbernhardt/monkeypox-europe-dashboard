library(rgdal)

# Reading polygons from json is very slow. Save as RDS once
world_countries_shapes <- rgdal::readOGR(dsn = "data/countries.geo.json")
saveRDS(world_countries_shapes, file = "data/countries.rds")