library(tidyverse)
library(janitor)
library(readr)
library(sf)
library(here)

## read in census h1 demographic data (all housing)
census_blocksh1 <- read_csv("census/h1/DECENNIALDHC2020.H1-Data.csv") %>% 
  clean_names()

## read in census h8 demographic data (occupied housing)
census_blocksh8 <- read_csv("census/h8/DECENNIALDHC2020.H8-Data.csv") %>% 
  clean_names()

# remove first cols
census_blocksh1 <- census_blocksh1[-1,]

census_blocksh8 <- census_blocksh8[-1,]

#rename columns to display number of housing units
census_blocksh1 <- census_blocksh1 %>% 
  rename(units = h1_001n)
  
census_blocksh8 <- census_blocksh8 %>% 
  rename(pop = h8_001n)

join <- merge(census_blocksh1, census_blocksh8, by = "geo_id") 

#check if all the name blocks line up
join %>% 
  summarise(sum(name.x != name.y)) #they do

join <- join %>% 
  select(geo_id, name.x, units, pop) %>% 
  rename(name = name.x)

join <- join %>% 
  mutate(units = as.numeric(units), pop = as.numeric(pop),
         geo_id = str_remove(geo_id, "1000000US"))

## shape file of census block
## very small level so a large dataset
il_blocks <- here("census/census_block_shapefile_IL/tl_2020_17_tabblock20.shp") %>%
  st_read() %>% 
  clean_names()

## put together two so that it is only those in cook county 83018 rows
cook_blocks <- join %>% 
  left_join(il_blocks, by = c("geo_id" = "geoid20")) %>% 
  st_as_sf()
## check if all the totals from cook blocks match total from shapefile, they do
##cook_blocks %>% 
##  summarise(sum(total != pop20))

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
  summarise(units = sum(units), pop = sum(pop), avg_household = sum(pop/units))

## need to save as rds file with housing unit counts and community areas
saveRDS(community_demo, "h1h8.test")
