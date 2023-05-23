
# choropleth map ----------------------------------------------------------

pacman::p_load(patchwork)
pacman::p_load(dplyr)
pacman::p_load(ggplot2)
theme_set(theme_bw())
set.seed(33)

sle_adm3 <- sf::read_sf(here::here("ch06_basic_geospatial_viz","data", "gis", "shp", "sle_adm3.shp")) %>%
  janitor::clean_names() %>% # standardize column names
  filter(admin2name %in% c("Western Area Urban", "Western Area Rural"))


linelist_adm_raw <- 
  rio::import(
    here::here("ch06_basic_geospatial_viz",
               "data", "case_linelists", 
               "linelist_cleaned.rds")) %>%
  sample_n(size = 1000) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

linelist_adm <- linelist_adm_raw %>%
  # join the administrative boundary file to the linelist, based on spatial intersection
  sf::st_join(sle_adm3, join = sf::st_intersects)

case_adm3 <- linelist_adm %>%          # begin with linelist with new admin cols
  as_tibble() %>%                      # convert to tibble for better display
  group_by(admin3pcod, admin3name) %>% # group by admin unit, both by name and pcode 
  summarise(cases = n()) %>%           # summarize and count rows
  arrange(desc(cases))                     # arrange in descending order

# inner join
sle_adm3_dat <- sle_adm3 %>% 
  inner_join(case_adm3, by = "admin3pcod")

intro_choropleth_01 <- ggplot(data=sle_adm3_dat) + 
  geom_sf(aes(fill=cases)) +
  colorspace::scale_fill_continuous_sequential() +
  labs(fill = "Ebola\ncases") +
  theme_bw()

ggsave(
  plot = intro_choropleth_01,
  filename = here::here("ch06_basic_geospatial_viz",
                        "images", "intro_choropleth_01.png"),
  width = 5,height = 3,
  dpi = "retina")


# dot distribution map ----------------------------------------------------

intro_dotdist_01 <- 
  ggplot(data = linelist_adm %>% filter(!is.na(outcome))) +
  geom_sf(mapping = aes(color = outcome), alpha = 0.5) +
  colorspace::scale_color_discrete_qualitative() +
  labs(color = "Outcome") +
  theme_bw()

ggsave(
  plot = intro_dotdist_01,
  filename = here::here("ch06_basic_geospatial_viz",
                        "images", "intro_dotdist_01.png"),
  width = 5,height = 3,
  dpi = "retina")


# physical features -------------------------------------------------------

pacman::p_load(dplyr)
pacman::p_load(ggplot2)
pacman::p_load(cholera)
pacman::p_load(sf)
pacman::p_load(patchwork)
theme_set(theme_bw())

fatalities_point_sf <- 
  cholera::fatalities %>% 
  st_as_sf(coords = c("x","y"),
           crs = 4326)

roads_line_sf <- cholera::roads %>% 
  filter(name!="Map Frame") %>% 
  st_as_sf(coords = c("x","y"),
           crs = 4326) %>% 
  group_by(street) %>% 
  summarise(mean=mean(n)) %>% 
  st_cast("LINESTRING")

spatial_type_point_1 <-
  ggplot(data = fatalities_point_sf) +
  geom_sf(alpha = 0.1, color = "#EE253A") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# spatial_type_point_2 <- 
#   ggplot(data = roads_line_sf) +
#   geom_sf(alpha = 0.5)

spatial_type_point_3 <- ggplot() +
  geom_sf(data = roads_line_sf,
          alpha = 0.5) +
  geom_sf(data = fatalities_point_sf,
          alpha = 0.1, color = "#EE253A") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# patch_layout <- "
# AABB
# #CC#
# #CC#
# "

# (spatial_type_point_1 | spatial_type_point_2) / spatial_type_point_3

physical_feature_01 <- 
  spatial_type_point_1 +
  # spatial_type_point_2 +
  spatial_type_point_3 +
  # plot_layout(design = patch_layout) +
  plot_annotation(tag_levels = "A")

ggsave(
  plot = physical_feature_01,
  filename = here::here("ch06_basic_geospatial_viz",
                        "images", "physical_feature_01.png"),
  width = 5,height = 3,
  dpi = "retina")

# spatial data types ------------------------------------------------------

pacman::p_load_gh("afrimapr/afrilearndata")
pacman::p_load(patchwork)
theme_set(theme_bw())

spatial_data_01 <- ggplot() +
  geom_sf(data = africountries,
          mapping = aes(fill = pop_est)) +
  colorspace::scale_fill_continuous_sequential() +
  labs(title = "Choropleth map") +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 11),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggplot() +
  geom_sf(data = africapitals,
          mapping = aes(size = pop), alpha =0.5) +
  # colorspace::scale_color_continuous_sequential() +
  labs(title = "Dot map") +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 11),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggplot(data = afrihighway) +
  geom_sf() +
  labs(title = "Physical features") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 11),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  plot_annotation(tag_levels = "A") #+
# plot_layout(guides = 'collect')

ggsave(
  plot = spatial_data_01,
  filename = here::here("ch06_basic_geospatial_viz",
                        "images", "spatial_data_01.png"),
  width = 5,height = 2,
  dpi = "retina")

# vector data -------------------------------------------------------------

pacman::p_load_gh("afrimapr/afrilearndata")
pacman::p_load(patchwork)
pacman::p_load(ggplot2)
pacman::p_load(sf)
theme_set(theme_bw())

geo1 <- 
  st_as_sfc("POLYGON((1 5, 2 2, 4 1, 4 4, 1 5))") %>% 
  ggplot() + geom_sf() + labs(title = "POLYGON")
geo2 <- 
  st_as_sfc(c("POINT(5 2)")) %>% 
  ggplot() + geom_sf() + labs(title = "POINT")
geo3 <- 
  st_as_sfc("LINESTRING(1 5, 4 4, 4 1, 2 2, 3 2)") %>% 
  ggplot() + geom_sf() + labs(title = "LINESTRING")

vector_data_01 <- 
  # geo1 +
  # theme(axis.ticks = element_blank(),
  #       axis.text = element_blank(),
  #       panel.grid.major = element_blank(), 
  #       panel.grid.minor = element_blank()) +
  # geo2 +
  # theme(axis.ticks = element_blank(),
  #       axis.text = element_blank(),
  #       panel.grid.major = element_blank(), 
  #       panel.grid.minor = element_blank()) +
  # geo3 +
  # theme(axis.ticks = element_blank(),
  #       axis.text = element_blank(),
  #       panel.grid.major = element_blank(), 
  #       panel.grid.minor = element_blank()) +
  ggplot(data = africountries) +
  geom_sf() +
  labs(title = "Polygons") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggplot(data = africapitals) +
  geom_sf() +
  labs(title = "Points") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggplot(data = afrihighway) +
  geom_sf() +
  labs(title = "Lines") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  plot_layout(#nrow = 2, 
              ncol = 3, 
              widths = c(1,1,1), 
              # heights = c(1,1)
              ) +
  plot_annotation(tag_levels = "A")

ggsave(
  plot = vector_data_01,
  filename = here::here("ch06_basic_geospatial_viz",
                        "images", "vector_data_01.png"),
  width = 5,height = 2,
  dpi = "retina")


# vector 2 ----------------------------------------------------------------

pacman::p_load_gh("afrimapr/afrilearndata")
pacman::p_load(patchwork)
pacman::p_load(ggplot2)
pacman::p_load(sf)
theme_set(theme_bw())

ggplot(data = africountries) +
  geom_sf() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggplot(data = africapitals) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_sf() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggplot(data = afrihighway) +
  geom_sf() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  plot_layout(nrow = 2, ncol = 3, 
              widths = c(1,1,1), heights = c(1,1)) +
  plot_annotation(tag_levels = "A")

ggsave(
  plot = vector_data_01,
  filename = here::here("ch06_basic_geospatial_viz",
                        "images", "vector_data_02.png"),
  width = 5,height = 4,
  dpi = "retina")

# coordinate reference system ---------------------------------------------

pacman::p_load(tmap)
pacman::p_load(sf)
pacman::p_load_gh("Nowosad/spDataLarge")

vector_filepath = system.file("vector/zion.gpkg", package = "spDataLarge")

new_vector = read_sf(vector_filepath)
new_vector2 = st_transform(new_vector, "EPSG:4326")

# new_vector = africountries
# new_vector2 = st_transform(africountries, "EPSG:4326")

tm1 = tm_shape(new_vector2) +
  tm_graticules(n.x = 3, n.y = 4) +
  tm_polygons() +
  tm_ylab("Latitude", space = 0.5) +
  tm_xlab("Longitude")

tm2 = tm_shape(new_vector) +
  tm_grid(n.x = 3, n.y = 4) +
  tm_polygons() +
  tm_ylab("y") +
  tm_xlab("x")

tm = tmap_arrange(tm1, tm2)
tm

tmap_save(
  tm = tm,
  filename =  here::here("ch06_basic_geospatial_viz",
                         "images", "crs_01.png"),
  width = 5,height = 3,
  dpi = 320)

# density map -------------------------------------------------------------

pacman::p_load(dplyr)
pacman::p_load(ggplot2)
pacman::p_load(cholera)

spatial_type_density_1 <- ggplot() +
  geom_line(data = cholera::roads %>% 
              filter(name!="Map Frame"),
            mapping = aes(x,y, group=street),
            alpha = 0.5) +
  geom_point(data = cholera::fatalities,
             mapping = aes(x,y),
             alpha = 0.1, color = "#EE253A") +
  coord_equal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

spatial_type_density_2 <- ggplot() +
  geom_line(data = cholera::roads %>% 
              filter(name!="Map Frame"),
            mapping = aes(x,y, group=street),
            alpha = 0.75) +
  stat_density_2d(data = cholera::fatalities,
                  mapping = aes(x,y, 
                                fill = ..level..),
                  alpha=0.5,
                  geom = "polygon"
  ) +
  colorspace::scale_fill_continuous_sequential(
    palette="Reds 3",rev = TRUE) +
  theme(legend.position = "none") +
  coord_equal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

pacman::p_load(patchwork)

density_map_01 <- spatial_type_density_1 +
  spatial_type_density_2 +
  plot_annotation(tag_levels = "A")

ggsave(
  plot = density_map_01,
  filename = here::here("ch06_basic_geospatial_viz",
                        "images", "density_map_01.png"),
  width = 5,height = 3,
  dpi = "retina")


# thematic maps -----------------------------------------------------------

pacman::p_load(ggplot2)
pacman::p_load_gh("afrimapr/afrilearndata")
pacman::p_load(patchwork)
theme_set(theme_bw())

thematic_map_01 <- 
  ggplot(africountries,
       aes(fill = pop_est)) +
  geom_sf() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggplot(africapitals) +
  geom_sf(aes(size = pop),
          alpha =0.5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggplot() +
  geom_stars(data = st_as_stars(afripop2020)) +
  scale_fill_continuous(trans = "log10") +
  coord_sf() +
  labs(fill = "pop2020") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  # theme(legend.position = "none") +
  plot_layout(nrow = 1) +
  plot_annotation(tag_levels = "A")

ggsave(
  plot = thematic_map_01,
  filename = here::here("ch06_basic_geospatial_viz",
                        "images", "thematic_map_01.png"),
  width = 11,height = 3,
  dpi = "retina")


# final map ---------------------------------------------------------------

pacman::p_load_gh("afrimapr/afrihealthsites")
pacman::p_load_gh("wmgeolab/rgeoboundaries")
pacman::p_load_gh("yutannihilation/ggsflabel")
pacman::p_load(tidyverse)
pacman::p_load(ggspatial)
pacman::p_load(janitor)
pacman::p_load(ggsn)
pacman::p_load(sf)
pacman::p_load(units)
theme_set(theme_bw())

# sites <- tibble(longitude = c(-13.1491617, -13.1066807), 
#                 latitude = c(8.1920813, 8.2180983),
#                 gps_name = "gps_name")

sites <- tribble(
  ~gps_name, ~longitude, ~latitude,
  "Hospital A", -13.1491617, 8.1920813,
  "Hospital B", -13.1066807, 8.2180983
)

sites_sf <- sites %>% 
  st_as_sf(coords = c("longitude","latitude"),
           crs = 4326)

sierra_leone <- geoboundaries(country = "Sierra Leone",adm_lvl = 1)

sle_healthsites_all <- afrihealthsites(
  country = "Sierra Leone", datasource='who',
  plot = FALSE, returnclass = "dataframe") %>% 
  janitor::clean_names()

sle_healthsites_set <- sle_healthsites_all %>%
  filter(admin1=="Western Area") %>% 
  filter(tier==3) %>%
  # filter(facility_type == "Maternal & Child Health Post") %>%
  st_as_sf(coords = c("long","lat"),
           crs = 4326)


sierra_leone_shp <- 
  sf::read_sf(dsn = here::here("ch06_basic_geospatial_viz",
                               "data","gis","shp",
                               "sle_adm3.shp"),
              quiet = TRUE) %>% 
  filter(admin1Name=="Western") %>% 
  mutate(area_m2 = st_area(.)) %>% 
  mutate(area_km2 = units::set_units(.$area_m2,km^2)) %>% 
  mutate(area_km2 = as.numeric(area_km2)) %>% 
  select(area_km2)

# sierra_leone_shp

multilayer_map_01 <- ggplot() +
  ## geometries
  # background map
  geom_sf(data = sierra_leone) +
  # district polygons filled by area
  geom_sf(data = sierra_leone_shp, 
          mapping = aes(fill=area_km2)) +
  # hospital points 
  geom_sf(data = sle_healthsites_set) +
  # field site points 
  geom_sf(data = sites_sf,
          mapping = aes(color = gps_name),
          shape = 18, size = 4) +
  ## labels
  # hospital names with repelled text 
  geom_sf_text_repel(data = sle_healthsites_set,
                     mapping = aes(label = facility_name),
                     # force = 40,
                     size         = 2,
                     fontface     = "bold",
                     box.padding  = 0.6,
                     force        = 0.5,
                     nudge_x      = -0.25,
                     direction    = "y",
                     hjust        = 1,
                     segment.size = 0.2) +
  # province names with repelled labels
  geom_sf_label_repel(data = sierra_leone %>% filter(shapeName!="Eastern"),
                      mapping = aes(label=shapeName)) +
  ## aesthetics
  # alternative color scale for fill
  colorspace::scale_fill_continuous_sequential(
    palette="Reds 3", alpha = 0.8) +
  # map annotation
  annotation_north_arrow(location="tl") +
  annotation_scale(location="bl") +
  # map extent
  coord_sf(xlim = c(-13.5,-12.7),
           ylim = c(8.0,8.7)) +
  # ggplot labels
  labs(x = "Longitude",
       y = "Latitude",
       fill = expression(Area~km^2),
       color = "GPS data",
       title = "How are Hospitals distributed in the Western Province of Sierra Leone?")

ggsave(
  plot = multilayer_map_01,
  filename = here::here("ch06_basic_geospatial_viz",
                        "images", "multilayer_map_01.png"),
  width = 8,height = 7,
  dpi = "retina")

# sle_healthsites_set %>% 
#   ggplot() +
#   # geometry
#   geom_sf() +
#   # label
#   geom_sf_text_repel(mapping = aes(label= facility_name),
#                      size = 3,#Make a little bigger
#                      fontface = "bold",
#                      force = 40, 
#                      box.padding = 0.6, 
#                      min.segment.length = 0) #Add arrows everywhere
