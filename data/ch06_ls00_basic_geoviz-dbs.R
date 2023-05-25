
# cholera -----------------------------------------------------------------

pacman::p_load(ggspatial)
pacman::p_load(ggplot2)
pacman::p_load(dplyr)
pacman::p_load(readr)
pacman::p_load(mdsr)
pacman::p_load(here)
pacman::p_load(sf)


mdsr::CholeraDeaths %>%
  # british national grid
  st_set_crs(27700) %>%
  # to wgs84
  st_transform(4326) %>% 
  # st_simplify(dTolerance = 1e3) %>% 
  write_rds(here::here("ch06_basic_geospatial_viz",
                      "data",
                      "cholera_deaths.rds"))

cholera_deaths <- read_rds(here("ch06_basic_geospatial_viz/data/cholera_deaths.rds"))

ggplot(cholera_deaths) + 
  annotation_map_tile(type = "osm", zoomin = 0) + 
  geom_sf(mapping = aes(size = Count), alpha = 0.7)

# cholera_deaths <- read_rds(here("ch06_basic_geospatial_viz/data/cholera_deaths.rds"))
# 
# cholera_deaths
# 
# ggplot(data = cholera_deaths) + 
#   annotation_map_tile(zoomin = 0) +
#   geom_sf(mapping = aes(size = Count, geometry = geometry), alpha = 0.7)


# gambia ------------------------------------------------------------------

pacman::p_load(dplyr)
pacman::p_load(readr)
pacman::p_load(here)
pacman::p_load(rio)

gambia <- rio::import("https://github.com/cran/geoR/raw/master/data/gambia.rda")

#' Data in `gambia` are given at an **individual level**. To get an estimate of the prevalence per village, we need to **aggregate** the malaria tests *by village*.
#' 
#' Using the `dplyr::distinct()` function, we can see that `gambia` has 2035 rows and 65 **unique pair** of coordinates. This indicates that 2035 malaria tests were conducted at 65 locations.
#' 
#' Here we use `dplyr` to create a data frame called `gambia_point_summary` containing, **for each village** the:

gambia %>% 
  distinct(x,y) %>% 
  nrow()

gambia_point_summary <- 
  gambia %>% 
  group_by(x, y) %>%
  summarize(
    total = n(),
    positive = sum(pos),
    prev = positive / total
  ) %>% 
  ungroup()

gambia_point_summary %>% 
  write_rds(here("ch06_basic_geospatial_viz/data/gambia_summarized.rds"))


# test st simplify --------------------------------------------------------

# pacman::p_load(sf)
# pacman::p_load(dplyr)
# pacman::p_load(ggplot2)
# pacman::p_load(patchwork)
# 
# nc = st_read(system.file("shape/nc.shp", package="sf"))
# nc
# nc_g = st_geometry(nc)
# nc_g
# 
# g1 <- nc_g %>% 
#   ggplot() +
#   geom_sf()
# g2 <- nc_g %>% 
#   st_simplify() %>% 
#   ggplot() +
#   geom_sf()
# g3 <- nc_g %>% 
#   st_simplify(dTolerance = 1e3) %>% 
#   ggplot() +
#   geom_sf()
# g4 <- nc_g %>% 
#   st_simplify(dTolerance = 5e3) %>% 
#   ggplot() +
#   geom_sf()
# 
# g1 + g2 + g3 + g4


# thematic map with lines -------------------------------------------------

pacman::p_load(rnaturalearth, 
               sf, 
               here, 
               tidyverse)

roads <-
  ne_download(scale = 10,
              category = "cultural",
              type = "roads", 
              returnclass = "sf")

south_am_roads <- 
  roads %>%  # filter the dataset to reduce time needed to plot
  filter(continent == "South America")

south_am_roads %>% 
  write_rds(here("ch06_basic_geospatial_viz/data/south_am_roads.rds"))

ggplot(data = south_am_roads) + 
  geom_sf(aes(color = type))

# preston crimes ----------------------------------------------------------

preston_crimes <- read_rds(here("ch06_basic_geospatial_viz/data/backup/pcrime-spatstat.rds.gz.rds"))

preston_crimes_raw <- tibble(x = preston_crimes$x,
                             y = preston_crimes$y,
                             marks = preston_crimes$marks)

preston_crimes_raw %>% 
  write_rds(here("ch06_basic_geospatial_viz/data/prestoncrimes/raw_pcrime.rds"))

# _ british national grid -------------------------------------------------

preston_crimes_utm <- preston_crimes_raw %>% 
  
  st_as_sf(coords = c("x", "y")#,
           # crs = "+proj=utm +zone=30"
           ) %>% 
  
  # british national grid
  st_set_crs(27700)

preston_crimes_utm %>% 
  write_rds(here("ch06_basic_geospatial_viz/data/prestoncrimes/uk_pcrime.rds"))

preston_crimes_utm

preston_crimes_utm %>% 
  ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf()

preston_crimes_utm %>% 
  
  # extract coordinates
  epihelper::st_coordinates_tidy() %>% 
  
  # ggplot
  ggplot(aes(x = X, y = Y)) +
  
  annotation_map_tile(type = "osm", zoomin = 0) +
  
  # *with a specific geom*
  geom_bin_2d() +
  
  # transform axis
  coord_sf() +
  facet_wrap(~marks)

# _ wgs84 -----------------------------------------------------------------


preston_crimes_sf <- preston_crimes_utm %>% 
  # st_transform(crs = "+proj=longlat +datum=WGS84")
  # to wgs84
  st_transform(4326)

preston_crimes_sf

preston_crimes_sf %>% 
  
  # extract coordinates
  epihelper::st_coordinates_tidy() %>% 
  
  # ggplot
  ggplot(aes(x = X, y = Y)) +
  
  annotation_map_tile(type = "osm", zoomin = 0) +
  
  # *with a specific geom*
  geom_bin_2d() +
  
  # transform axis
  coord_sf() +
  facet_wrap(~marks)

preston_crimes_sf %>% 
  write_rds(here("ch06_basic_geospatial_viz/data/pcrime.rds"))

# _ write_sf --------------------------------------------------------------


preston_crimes_sf %>% 
  write_sf(here("ch06_basic_geospatial_viz/data/prestoncrimes/pcrime.shp"))

# _ read_sf ---------------------------------------------------------------


read_sf(here("ch06_basic_geospatial_viz/data/prestoncrimes/pcrime.shp"))
