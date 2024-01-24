library(tidyverse)
library(janitor)
library(readr)
library(sf)
library(here)

## read in census p1 demographic data (race)
census_blocks <- read_csv("census/p1/DECENNIALPL2020.P1-Data.csv") %>% 
  clean_names()

# remove first col
census_blocks <- census_blocks[-1,]
## pull out white/black and total with geo id
### using column and data info in folder for col name
census_blocks <- census_blocks %>% 
  select(geo_id, name, p1_001n, p1_003n, p1_004n) %>% 
  rename(total = p1_001n, white = p1_003n, black = p1_004n) %>% 
  mutate(total = as.numeric(total),
         white = as.numeric(white),
         black = as.numeric(black),
         geo_id = str_remove(geo_id, "1000000US"))
## shape file of census block
## very small level so a large dataset
il_blocks <- here("census/census_block_shapefile_IL/tl_2020_17_tabblock20.shp") %>%
  st_read() %>% 
  clean_names()

## put together two so that it is only those in cook county 83018 rows
cook_blocks <- census_blocks %>% 
  left_join(il_blocks, by = c("geo_id" = "geoid20")) %>% 
  st_as_sf()
## check if all the totals from cook blocks match total from shapefile, they do
cook_blocks %>% 
  summarise(sum(total != pop20))

## load in community area shape file
comm_ar <- here::here("shapefiles/comm_areas/comm_areas.shp") %>%
  st_read(quiet = TRUE) %>%
  st_transform(crs = st_crs(cook_blocks)) %>% 
  mutate(community = tolower(community)) %>%
  select(community, geometry) %>%
  arrange(community)

## combine spatially community area and cook county blocks together
## will take forever
community_demo <- comm_ar %>% 
  st_join(cook_blocks) %>% 
  group_by(community) %>% 
  summarise(total = sum(total),
            white = sum(white),
            black = sum(black))

## need to save as rds file with each race type and community areas