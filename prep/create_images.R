# Script to create and save images for the lessons
## The GRAPH Courses team
## 2022-07-05

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, rnaturalearth)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## River with two segments ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# To explain why rivers are multilinestring as opposed to simple linestring
### Heading X.1 ----

world_rivers <-
  ne_download(scale = 10,
              category = "physical",
              type = "rivers_lake_centerlines", 
              returnclass = "sf") 

chico <- 
  world_rivers %>% 
  select(name_en, geometry) %>% 
  group_by(name_en) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n == 1) %>%
  filter(name_en == "Chico River") 

chico_plot <- 
  chico %>% 
  ggplot() + 
  geom_sf() +
  labs(subtitle = "The Chico river\n is made of two linestrings") + 
  theme(plot.subtitle = element_text(hjust = 0.5))
  
ggsave(filename = here("ch06_basic_geospatial_viz-KENE-ACTIVE-EDIT/images/chico_plot.png"), chico_plot, width = 3, height = 3)
