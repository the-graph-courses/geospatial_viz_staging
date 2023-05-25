# Script to read in shapefiles and save to RDS
## The GRAPH Courses team
## 2022-07-05

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, sf, here, mdsr)


#### Sacramento rivers ----

sacramento_rivers <- st_read(here("ch06_basic_geospatial_viz/prep/data/SAC_river_types/SAC_channel-types_predictions_v1.shp"))
sacramento_rivers %>% 
  head(500) %>% 
  saveRDS(file = here("ch06_basic_geospatial_viz/data/sacramento_rivers.rds"))

#### John snow map reprojected ----

cholera_deaths <- 
  mdsr::CholeraDeaths %>% 
  st_set_crs(27700) %>%
  st_transform(crs = 4326)

cholera_deaths %>% 
  saveRDS(file = here("ch06_basic_geospatial_viz/data/cholera_deaths.rds"))

