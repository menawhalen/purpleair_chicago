library(sf)

city_bounds <- st_read("/Users/nastaranghorbani/Downloads/city.shx")
sensors <-read.csv("/Users/nastaranghorbani/Downloads/new_sensors.csv")

# Convert the sensors data to an sf object
sensors_sf <- st_as_sf(sensors, coords = c("longitude", "latitude"), crs = st_crs(city_bounds))

# Find sensors that are within the city bounds
sensors_in_city <- st_join(sensors_sf, city_bounds, join = st_within)

View(sensors_in_city)
