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
  select(geo_id, name, p1_001n, p1_003n, p1_004n, p1_005n, p1_006n, p1_007n) %>% 
  rename(total = p1_001n, white = p1_003n, black = p1_004n, native = p1_005n, asian = p1_006n, pacificisl = p1_007n) %>% 
  mutate(total = as.numeric(total),
         white = as.numeric(white),
         black = as.numeric(black),
         native = as.numeric(native),
         asian = as.numeric(asian),
         pacificisl = as.numeric(pacificisl),
         geo_id = str_remove(geo_id, "1000000US")) %>% 
  mutate(otherTot = native + asian + pacificisl)
#added other category with Native Americans, Asians, and Pacific Islanders

meanOther <- census_blocks %>% 
  mutate(otherp = otherTot/total) %>% 
  summarize(x = mean(otherp, na.rm = T))
#7.3% of residents identify in other

meanNative <- census_blocks %>% 
  mutate(nativep = native/total) %>% 
  summarize(x = mean(nativep, na.rm = T))
#1.26 % of residents identify as native

meanAsian <- census_blocks %>% 
  mutate(asianp = asian/total) %>% 
  summarize(x = mean(asianp, na.rm = T))
#6.01 % of residents identify as Asian

meanPI <- census_blocks %>% 
  mutate(PIp = pacificisl/total) %>% 
  summarize(x = mean(PIp, na.rm = T))
#0.0522% of residents identify as Pacific Islander

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
            black = sum(black),
            asian = sum(asian))

## need to save as rds file with each race type and community areas
saveRDS(community_demo, "p1.test")
