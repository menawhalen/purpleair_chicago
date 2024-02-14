library(tidyverse)
library(janitor)
library(readr)
library(sf)
library(here)

## read in census p1 demographic data (Hispanic/Latino)
census_blocks <- read_csv("census/p2/DECENNIALPL2020.P2-Data.csv") %>% 
  clean_names()

# remove first col
census_blocks <- census_blocks[-1,]

## pull out white/black and total with geo id
### using column and data info in folder for col name
census_blocks <- census_blocks %>% 
  select(geo_id, name, p2_001n, p2_002n, p2_003n) %>% 
  rename(total = p2_001n, HisLat = p2_002n, NotHisLat = p2_003n) %>% 
  mutate(total = as.numeric(total),
         HisLat = as.numeric(HisLat),
         NotHisLat = as.numeric(NotHisLat),
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
  select(total, pop20) %>% 
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
            HisLat = sum(HisLat),
            NotHisLat = sum(NotHisLat))

## need to save as rds file with race type and community areas
saveRDS(community_demo, "p2.test")
