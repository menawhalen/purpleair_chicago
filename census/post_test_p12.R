library(tidyverse)
library(janitor)
library(readr)
library(sf)
library(here)

## read in census p1 demographic data (age and sex)
census_blocks <- read_csv("census/p12/DECENNIALDHC2020.P12-Data.csv") %>% 
  clean_names()

# remove first col
census_blocks <- census_blocks[-1,]

#rename columns to display sex and age groups
census_blocks <- census_blocks %>% 
  rename(total = p12_001n, totalM = p12_002n, M5u = p12_003n, M5to9 = p12_004n, M10to14 = p12_005n, 
         M15to17 = p12_006n, M18to19 = p12_007n, M20 = p12_008n, M21 = p12_009n, M22to24 = p12_010n, 
         M25to29 = p12_011n, M30to34 = p12_012n, M35to39 = p12_013n, M40to44 = p12_014n, 
         M45to49 = p12_015n, M50to54 = p12_016n, M55to59 = p12_017n, M60to61 = p12_018n, M62to64 = p12_019n, 
         M65to66 = p12_020n, M67to69 = p12_021n,M70to74 = p12_022n, M75to79 = p12_023n, M80to84 = p12_024n, 
         M85plus = p12_025n) %>% 
  rename(totalF = p12_026n, F5u = p12_027n, F5to9 = p12_028n, F10to14 = p12_029n, F15to17 = p12_030n, 
         F18to19 = p12_031n, F20 = p12_032n,F21 = p12_033n, F22to24 = p12_034n, F25to29 = p12_035n, 
         F30to34 = p12_036n, F35to39 = p12_037n, F40to44 = p12_038n, F45to49 = p12_039n, F50to54 = p12_040n, 
         F55to59 = p12_041n, F60to61 = p12_042n, F62to64 = p12_043n, F65to66 = p12_044n, F67to69 = p12_045n,
         F70to74 = p12_046n, F75to79 = p12_047n, F80to84 = p12_048n, F85plus = p12_049n)

#turn all variables into numbers except geo_id and name
census_blocks <- census_blocks %>% 
  mutate(across(-c(name, geo_id), as.numeric)) %>% 
  select(geo_id, name, everything()) %>% 
  mutate(geo_id = str_remove(geo_id, "1000000US")) 

#transform to age groups: <18, 18-65, 65+
census_blocks<- census_blocks %>% 
  mutate(M18u = M5u + M5to9 + M10to14 + M15to17, 
         M18to65 = M18to19 + M20 + M21 + M22to24 + M25to29 + M30to34 + M35to39 + M40to44 + M45to49 + 
           M50to54 + M55to59 + M60to61 + M62to64, 
         M65plus = M65to66 + M67to69 + M70to74 + M75to79 + M80to84 + M85plus, 
         F18u = F5u + F5to9 + F10to14 + F15to17, 
         F18to65 = F18to19 + F20 + F21 + F22to24 + F25to29 + F30to34 + F35to39 + F40to44 + F45to49 + 
           F50to54 + F55to59 + F60to61 + F62to64, 
         F65plus = F65to66 + F67to69 + F70to74 + F75to79 + F80to84 + F85plus) %>% 
  select(geo_id, name, total, totalM, M18u, M18to65, M65plus, totalF, F18u, F18to65, F65plus)

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
            totalM = sum(totalM),
            M18u = sum(M18u),
            M18to65 = sum(M18to65),
            M65plus = sum(M65plus),
            totalF = sum(totalF),
            F18u = sum(F18u),
            F18to65 = sum(F18to65),
            F65plus = sum(F65plus))

## need to save as rds file with each age and sex group and community areas
saveRDS(community_demo, "p12.test")