
# choropleth map ----------------------------------------------------------

pacman::p_load(patchwork)
pacman::p_load(dplyr)
pacman::p_load(ggplot2)
theme_set(theme_bw())
set.seed(33)

sle_adm3 <- sf::read_sf(here::here("ch06_basic_geospatial_viz","data", "boundaries", "sle_adm3.shp")) %>%
  janitor::clean_names() %>% # standardize column names
  filter(admin2name %in% c("Western Area Urban", "Western Area Rural"))


linelist_adm_raw <- 
  rio::import(
    here::here("ch06_basic_geospatial_viz",
               "data", "linelist_cleaned.rds")) %>%
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
                        "lessons","images", "intro_choropleth_01.png"),
  width = 5,height = 3,
  dpi = "retina")


# dot distribution map ----------------------------------------------------

intro_dotdist_01 <- 
  ggplot(data = linelist_adm %>% filter(!is.na(outcome))) +
  geom_sf(mapping = aes(color = outcome), 
          alpha = 0.5) +
  colorspace::scale_color_discrete_qualitative() +
  labs(color = "Ebola\ncase\noutcome") +
  theme_bw()

intro_dotdist_02 <- 
  ggplot(data = linelist_adm %>% filter(!is.na(outcome))) +
  geom_sf(mapping = aes(color = outcome), 
          alpha = 0.5,
          size = 0.5) +
  colorspace::scale_color_discrete_qualitative() +
  labs(color = "Ebola\ncase\noutcome") +
  theme_bw()

ggsave(
  plot = intro_dotdist_01,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "intro_dotdist_01.png"),
  width = 5,height = 3,
  dpi = "retina")

# merge choropleth and dot distribution -----------------------------------

merge_intro_plots <- intro_choropleth_01 +
  intro_dotdist_02 +
  plot_annotation(tag_levels = "A")

ggsave(
  plot = merge_intro_plots,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "introplots_merge_01.png"),
  width = 8,height = 2,
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
  geom_sf(alpha = 0.25, 
          size = 0.5,
          color = "#EE253A") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# spatial_type_point_2 <- 
#   ggplot(data = roads_line_sf) +
#   geom_sf(alpha = 0.5)

spatial_type_point_3 <- ggplot() +
  geom_sf(data = roads_line_sf,
          alpha = 0.5) +
  geom_sf(data = fatalities_point_sf,
          alpha = 0.25, 
          size = 0.5,
          color = "#EE253A") +
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
                        "lessons","images", "physical_feature_01.png"),
  width = 6,height = 3,
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
                        "lessons","images", "spatial_data_01.png"),
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
                        "lessons","images", "vector_data_01.png"),
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
                        "lessons","images", "vector_data_02.png"),
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
                         "lessons","images", "crs_01.png"),
  width = 5,height = 3,
  dpi = 320)

# density map -------------------------------------------------------------

pacman::p_load(dplyr)
pacman::p_load(ggplot2)
pacman::p_load(cholera)
theme_set(theme_bw())

spatial_type_density_1 <- ggplot() +
  geom_line(data = cholera::roads %>% 
              filter(name!="Map Frame"),
            mapping = aes(x,y, group=street),
            alpha = 0.5) +
  geom_point(data = cholera::fatalities,
             mapping = aes(x,y),
             alpha = 0.25, 
             size = 0.5,
             color = "#EE253A") +
  coord_sf(crs = 4326) +
  labs(x = NULL, y = NULL) +
  # labs(subtitle = "London 1854") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

spatial_type_density_2 <- ggplot() +
  geom_line(data = cholera::roads %>% 
              filter(name!="Map Frame"),
            mapping = aes(x,y, group=street),
            alpha = 0.75) +
  stat_density_2d(
    data = cholera::fatalities,
    mapping = aes(x,y,fill = after_stat(count)),
    geom = "raster",
    contour = FALSE,
    alpha=0.8) +
  # stat_density_2d(data = cholera::fatalities,
  #                 mapping = aes(x,y, 
  #                               fill = ..level..),
  #                 alpha=0.5,
  #                 geom = "polygon"
  # ) +
  colorspace::scale_fill_continuous_sequential(
    palette="Reds 3",rev = TRUE) +
  # theme(legend.position = "none") +
  coord_sf(crs = 4326) +
  labs(x = NULL, y = NULL) +
  labs(fill = "Cholera\ndeaths"#,subtitle = "London 1854"
       ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

pacman::p_load(patchwork)

density_map_01 <- spatial_type_density_1 +
  spatial_type_density_2 +
  plot_annotation(tag_levels = "A")

ggsave(
  plot = density_map_01,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "density_map_01.png"),
  width = 6,height = 3,
  dpi = "retina")


# _ new ---------------------------------------------------------------------

phase_03 <- 
  intro_choropleth_01 +
  # theme(legend.position="bottom") +
  intro_dotdist_02 +
  # theme(legend.position="bottom") +
  spatial_type_density_1 +
  spatial_type_density_2 +
  plot_annotation(tag_levels = "A")

ggsave(
  plot = phase_03,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "intro_thematic_map_05.png"),
  width = 7,height = 5,
  dpi = "retina")

# _ new intro ---------------------------------------------------------------

pacman::p_load(ggspatial)
pacman::p_load(readr)
pacman::p_load(here)

cholera_deaths <- read_rds(here("ch06_basic_geospatial_viz/data/cholera_deaths.rds"))

basemap_01 <- ggplot(data = cholera_deaths) + 
  
  # 👉 add a basemap 👈
  annotation_map_tile(zoomin = 0) + 
  
  # continue with ggplot
  geom_sf(mapping = aes(size = Count), alpha = 0.5) +
  
  labs(size = "Cholera \ndeaths")

pacman::p_load(patchwork)

density_map_02 <- spatial_type_density_2 +
  basemap_01 +
  plot_annotation(tag_levels = "A")

ggsave(
  plot = density_map_02,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "density_map_02.png"),
  width = 11,height = 4,
  dpi = "retina")

# _ new ---------------------------------------------------------------------

phase_04 <- 
  intro_choropleth_01 +
  # theme(legend.position="bottom") +
  intro_dotdist_01 +
  # theme(legend.position="bottom") +
  spatial_type_density_2 +
  basemap_01 +
  plot_annotation(tag_levels = "A")

ggsave(
  plot = phase_04,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "intro_thematic_map_06.png"),
  width = 11,height = 8,
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
                        "lessons","images", "thematic_map_01.png"),
  width = 11,height = 3,
  dpi = "retina")


# update boundaries shp ---------------------------------------------------
pacman::p_load(tidyverse)
pacman::p_load(sf)

sf::read_sf(here::here("ch06_basic_geospatial_viz",
                         "data","backup",
                         "sle_adm3.shp"),
              quiet = TRUE) %>% 
  # filter(admin1Name=="Western") %>% 
  mutate(area_m2 = st_area(.)) %>% 
  mutate(area_km2 = units::set_units(.$area_m2,km^2)) %>% 
  mutate(area_km2 = as.numeric(area_km2)) %>% 
  select(OBJECTID:date,area_km2) %>% 
  sf::write_sf(here::here("ch06_basic_geospatial_viz",
                          "data","boundaries",
                          "sle_adm3.shp"))

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
  sf::read_sf(here::here("ch06_basic_geospatial_viz",
                         "data","boundaries",
                         "sle_adm3.shp"),
              quiet = TRUE) %>% 
  filter(admin1Name=="Western") %>% 
  # mutate(area_m2 = st_area(.)) %>% 
  # mutate(area_km2 = units::set_units(.$area_m2,km^2)) %>% 
  # mutate(area_km2 = as.numeric(area_km2)) %>% 
  # select(area_km2) %>% 
  identity()

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
                        "lessons","images", "multilayer_map_01.png"),
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


# colorspace examples -----------------------------------------------------

pacman::p_load(patchwork)
pacman::p_load(dplyr)
pacman::p_load(ggplot2)
pacman::p_load(colorspace)
pacman::p_load(palmerpenguins)
pacman::p_load(Hmisc)
set.seed(33)
# theme_set(theme_bw())

# _ 01 --------------------------------------------------------------------

df <- data.frame(height = c(volcano), x = c(row(volcano)), y = c(col(volcano)))

t <- ggplot(df, aes(x, y, fill = height)) + 
  geom_raster() + 
  coord_fixed(expand = FALSE)

colorspace_t <- 
  # t + 
  # labs(title = "Default", subtitle = "p") +
  (t + 
     colorspace::scale_fill_continuous_sequential(palette = "Blues",rev = TRUE) + 
     labs(title = "Continuous",
          subtitle = "p + scale_fill_continuous_sequential()")) +
  (t + 
     colorspace::scale_fill_binned_sequential(palette = "Blues",rev = TRUE) +
     labs(title = "Binned", 
          subtitle = "p + scale_fill_binned_sequential()")) +
  plot_layout(widths = c(#1, 
                         1.15, 1.05))

ggsave(
  plot = colorspace_t,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "colorspace_01.png"),
  width = 8,height = 2.7,
  # width = 11,height = 3,
  dpi = "retina")


# _ 02 --------------------------------------------------------------------

p <- ggplot(faithfuld, 
            aes(x = waiting, y = eruptions)) +
  geom_raster(aes(fill = density))

colorspace_p <- 
  # p + 
  # labs(title = "default", subtitle = "q") +
  (p + 
     colorspace::scale_fill_continuous_sequential(palette="Viridis",rev=FALSE) + 
     labs(title = "Sequential",
          subtitle = "r + scale_fill_continuous_sequential()")) +
  (p + 
     colorspace::scale_fill_continuous_diverging(palette = "Blue-Red",
                                                 mid = 0.02) +
     labs(title = "Diverging", 
          subtitle = "r + scale_fill_continuous_diverging( mid=0.02 )")) +
  plot_layout(widths = c(#1, 
                         1.15, 1.05))

ggsave(
  plot = colorspace_p,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "colorspace_02.png"),
  width = 8,height = 2.7,
  dpi = "retina")

# # _ 03 ------------------

q <- ggplot(diamonds %>% sample_n(size = 1000),
            aes(x=carat, y=price,
                color=cut)) +
  geom_point(alpha = 0.75)

colorspace_q <- 
  # q +
  # labs(title = "default", subtitle = "q") +
  (q +
     colorspace::scale_color_discrete_diverging(palette = "Blue-Red") +
     labs(title = "Diverging",
          subtitle = "q + scale_color_discrete_diverging()")) +
  (q +
     colorspace::scale_color_discrete_sequential(palette="Reds 3",rev=TRUE, nmax = 6,order = 2:6) +
     labs(title = "Sequential",
          subtitle = "q + scale_color_discrete_sequential()")) +
  plot_layout(widths = c(#1, 
                         1.15, 1.05))

ggsave(
  plot = colorspace_q,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "colorspace_03.png"),
  width = 8,height = 2.7,
  dpi = "retina")


#  change tribble ---------------------------------------------------------

pacman::p_load(tidyverse)

sites <- tribble(~gps_name, ~gps_longitude, ~gps_latitude,
                 "site A", -13.1491617, 8.1920813,
                 "site B", -13.1066807, 8.2180983
)

sites %>% 
  write_csv(here::here("ch06_basic_geospatial_viz",
                       "data", "gps_healthsites.csv"))


q1_correct <- tribble(~gps_data, ~gps_longitude, ~gps_latitude, 
                      "household", -13.1856942, 8.2851963)

q1_correct %>% 
  writexl::write_xlsx(here::here("ch06_basic_geospatial_viz",
                                 "data", "gps_healthsites.xlsx"))

readxl::read_xlsx(here::here("ch06_basic_geospatial_viz",
                             "data", "gps_healthsites.xlsx"))
# 
# haven::read_dta()



# density map 1 -----------------------------------------------------------

# SOURCE https://raw.githubusercontent.com/Robinlovelace/geocompr/main/code/02-raster-intro-plot.R
pacman::p_load(patchwork)
pacman::p_load(terra)
pacman::p_load(sf)
pacman::p_load(tmap)
pacman::p_load(spData)
pacman::p_load(ggplot2)
# theme_set(theme_bw())
set.seed(2021-09-09)

small_ras = rast(matrix(1:16, 4, 4, byrow = TRUE))
crs(small_ras) = "EPSG:4326"
polys = st_as_sf(as.polygons(small_ras, na.rm = FALSE))
polys$vals = sample.int(100, 16)
polys$vals[c(7, 9)] = "NA"
suppressWarnings({polys$valsn = as.numeric(polys$vals)})

polys_a <- polys %>% 
  ggplot() + 
  geom_sf(fill = "white") +
  ggsflabel::geom_sf_text(aes(label = lyr.1)) +
  labs(x = NULL, y = NULL,
       title = "A. Cell IDs") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = NA))

polys_b <- polys %>% 
  ggplot() + 
  geom_sf(fill = "white") +
  ggsflabel::geom_sf_text(aes(label = vals)) +
  labs(x = NULL, y = NULL,
       title = "B. Cell values") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = NA))

polys_c <- polys %>% 
  ggplot() + 
  geom_sf(aes(fill = valsn)) +
  colorspace::scale_fill_continuous_diverging(palette = "Blue-Red",rev = FALSE, 
                                              mid = 50) +
  labs(title = "C. Colored values") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "white", 
                                        colour = NA))


polys_all <- 
  polys_a +
  polys_b +
  polys_c #+
  # plot_layout(design = patch_layout) +
  # plot_annotation(tag_levels = "A")

ggsave(
  plot = polys_all,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "densitymap_02.png"),
  width = 5,height = 2,
  dpi = "retina")


# cholera simple ----------------------------------------------------------

pacman::p_load(readr)
pacman::p_load(here)
pacman::p_load(ggplot2)
theme_set(theme_bw())

cholera_deaths <- read_rds(here("ch06_basic_geospatial_viz/data/cholera_deaths.rds"))

cholera_simple <- ggplot(data = cholera_deaths) + 
  
  # continue with ggplot
  geom_sf(mapping = aes(size = Count), alpha = 0.5) +
  labs(size = "Cholera\ndeaths")

ggsave(
  plot = cholera_simple,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "cholera_dot_size.png"),
  width = 6,height = 5,
  dpi = "retina")



# _test --------------------------------------------------------------------

pacman::p_load(dplyr)
pacman::p_load(ggplot2)
pacman::p_load(cholera)
theme_set(theme_bw())

spatial_type_density_2 <- ggplot() +
  # geom_line(data = cholera::roads %>% 
  #             filter(name!="Map Frame"),
  #           mapping = aes(x,y, group=street),
  #           alpha = 0.75) +
  stat_density_2d(
    data = cholera::fatalities,
    mapping = aes(x,y,fill = after_stat(count)),
    geom = "raster",
    contour = FALSE,
    alpha=0.8) +
  # stat_density_2d(data = cholera::fatalities,
  #                 mapping = aes(x,y, 
  #                               fill = ..level..),
  #                 alpha=0.5,
  #                 geom = "polygon"
  # ) +
  colorspace::scale_fill_continuous_sequential(
    palette="Reds 3",rev = TRUE) +
  # theme(legend.position = "none") +
  coord_sf(crs = 4326) +
  labs(x = NULL, y = NULL) +
  labs(fill = "Cholera\ndeaths"#,subtitle = "London 1854"
  ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


# malaria gambia ----------------------------------------------------------

pacman::p_load_gh("wmgeolab/rgeoboundaries")
pacman::p_load(tidyverse,
               ggspatial,
               leaflet,
               raster,
               stars,
               here)

# get boundary data
gambia_boundaries <- geoboundaries(country = "Gambia", adm_lvl = 1)
# read prevalence data
gambia_prevalence <- read_rds(here("ch06_basic_geospatial_viz",
                                   "data", "gambia_prevalence.rds"))

malaria_gambia_01 <- 
  ggplot() +
  # background
  annotation_map_tile(data = gambia_boundaries, 
                      zoomin = 0) +
  # prevalence surface
  geom_stars(data = st_as_stars(gambia_prevalence)) +
  # color scale
  scale_fill_viridis_c(na.value = "transparent", 
                       alpha = 0.75,
                       limits = c(0,1),
                       breaks = seq(0,1,0.2),
                       labels = scales::label_percent()) +
  # coordinate system
  coord_sf() +
  labs(y = NULL,
       x = NULL,
       fill = "Malaria\nPrevalence")

ggsave(
  plot = malaria_gambia_01,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "malaria_gambia_01.png"),
  width = 9,height = 3,
  dpi = "retina")



# rnaturalearth | rgeoboundaries ------------------------------------------

pacman::p_load(patchwork)
pacman::p_load(dplyr)
pacman::p_load(ggplot2)
pacman::p_load(rnaturalearth)
pacman::p_load(rgeoboundaries)
theme_set(theme_bw())

pa <- ne_countries(returnclass = "sf", 
                   country = "ireland") %>% 
  ggplot() +
  geom_sf() +
  labs(title = "{rnaturalearth}")

pb <- geoboundaries(country = "ireland") %>% 
  ggplot() +
  geom_sf() +
  labs(title = "{rgeoboundaries}")

# px <- gadm(country = "ireland",level = 0,path = tempdir()) %>% 
#   st_as_sf() %>% 
#   ggplot() +
#   geom_sf() +
#   labs(title = "{geodata}")

geoboundaries_01 <- pa + pb + 
  plot_annotation(tag_levels = "A")

ggsave(
  plot = geoboundaries_01,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "geoboundaries_01.png"),
  width = 5,height = 3,
  dpi = "retina")


# summary gadm ------------------------------------------------------------

pacman::p_load(patchwork)
pacman::p_load(dplyr)
pacman::p_load(ggplot2)
pacman::p_load(rnaturalearth)
pacman::p_load(rgeoboundaries)
pacman::p_load(geodata)
pacman::p_load(sf)
theme_set(theme_bw())


ire1 <- gadm(country = "bolivia",level = 1,path = tempdir())
ire3 <- gadm(country = "bolivia",level = 2,path = tempdir())

# ire3 %>%
#   st_as_sf() %>%
#   as_tibble() %>%
#   count(NAME_1,NAME_2) %>%
#   count(NAME_1,sort = T)
# 
# ire3 %>%
#   st_as_sf() %>%
#   filter(NAME_1 == "Santa Cruz") %>%
#   ggplot() +
#   geom_sf(data = geoboundaries(country = "bolivia")) +
#   geom_sf(aes(fill = NAME_1)) +
#   ggsflabel::geom_sf_text_repel(
#     mapping = aes(label = NAME_2),
#     size = 2)

pc <- ggplot() +
  geom_sf(data = geoboundaries(country = "bolivia")) +
  geom_sf(data = ire3 %>% 
            st_as_sf() %>% 
            filter(NAME_1 == "Santa Cruz"),
          mapping = aes(fill = NAME_1)) +
  # ggsflabel::geom_sf_label_repel(data = ire1 %>% 
  #                                  st_as_sf() %>% 
  #                                  filter(NAME_1 == "Kilkenny"),
  #                                mapping = aes(label = NAME_1),
  #                                ) +
  ggsflabel::geom_sf_text_repel(data = ire3 %>%
                                   st_as_sf() %>%
                                   filter(NAME_1 == "Santa Cruz"),
                                 mapping = aes(label = NAME_2),
                                size = 2,
                                # force        = 0.5,
                                # box.padding  = 0.6,
                                # nudge_x      = -0.25,
                                # direction    = "y",
                                # hjust        = 1,
                                # segment.size = 0.2
                                ) +
  theme(legend.position = c(0.75,0.9),
        legend.key.height = unit(0.2,"in"),
        legend.key.width = unit(0.2,"in"),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7)) +
  labs(title = "{geodata}",
       subtitle = "Bolivia: departments and provinces",
       fill = "Department",
       x = NULL,
       y = NULL)
# pc

ggsave(
  plot = pc,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "geoboundaries_03.png"),
  width = 3,height = 4,
  dpi = "retina")

bolivia_province <- gadm(country = "bolivia",
                         level = 2,
                         path = tempdir()) %>% 
  st_as_sf() %>% 
  filter(NAME_1 == "Santa Cruz")

bolivia_only <- ggplot() +
  geom_sf(data = geoboundaries(country = "bolivia")) +
  geom_sf(data = bolivia_province,
          mapping = aes(fill = NAME_1))

ggsave(
  plot = bolivia_only,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "geoboundaries_04.png"),
  width = 4,height = 3,
  dpi = "retina")

pa <- ne_countries(returnclass = "sf", 
                   continent = "south america") %>% 
  ggplot() +
  geom_sf() +
  labs(title = "{rnaturalearth}",
       subtitle = "South American countries")

pb <- geoboundaries(country = c("peru","bolivia"),
                    adm_lvl = 1) %>% 
  ggplot() +
  geom_sf() +
  labs(title = "{rgeoboundaries}",
       subtitle = "Perú and Bolivia departments")

geoboundaries_02 <- pa + pb + pc +
  plot_annotation(tag_levels = "A")

ggsave(
  plot = geoboundaries_02,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "geoboundaries_02.png"),
  width = 9,height = 4,
  dpi = "retina")


# intro boundaries --------------------------------------------------------

pacman::p_load_gh("afrimapr/afrilearndata")
pacman::p_load(dplyr)
pacman::p_load(ggplot2)
pacman::p_load(rgeoboundaries)
pacman::p_load(sf)
pacman::p_load(patchwork)
theme_set(theme_bw())

poly_country <- geoboundaries(country = c("libya",
                                          "egypt"))

union_country <- poly_country#st_union(poly_country)

air_country <- sf::st_intersection(x = afriairports,
                                   y = union_country)

sfa <- ggplot() +
  geom_sf(data = afriairports,
          color = "purple",
          alpha = 0.5) +
  labs(caption = "ggplot() +\n  geom_sf(data = afriairports)") +
  theme_void() +
  theme(plot.caption = element_text(size = 11,
                                    hjust=0))

sfb <- ggplot() +
  geom_sf(data = poly_country) +
  ggsflabel::geom_sf_label(data = poly_country,
                           aes(label = shapeName)) + 
  labs(caption = "ggplot() +\n  geom_sf(data = country_borders)") +
  theme_void() +
  theme(plot.caption = element_text(size = 11,
                                    hjust=0))

# ggplot() +
#   geom_sf(data = poly_country) +
#   geom_sf(data = afriairports)

sfc <- ggplot() +
  geom_sf(data = poly_country) +
  geom_sf(data = air_country,
          color = "purple",
          alpha = 0.75) +
  labs(caption = "new_intersection <- sf::st_intersection( x = afriairports,\n                                                             y = country_borders )\nggplot() +\n  geom_sf(data = country_borders) +\n  geom_sf(data = new_intersection)") +
  theme_void() +
  theme(plot.caption = element_text(size = 11,
                                    hjust=0))

st_fig_intersection <- 
  sfa + 
  wrap_elements(
    grid::textGrob(label = '+',
                   gp=grid::gpar(fontsize=20))) +
  sfb + 
  wrap_elements(
    grid::textGrob(label = '=>',
                   gp=grid::gpar(fontsize=20))) +
  sfc +
  plot_layout(ncol = 5,
              widths = c(12,1,12,1,14))
  # plot_annotation(tag_levels = "A")

ggsave(
  plot = st_fig_intersection,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "sf_intersection_01.png"),
  width = 12,height = 4,
  dpi = "retina")

# ggplot() +
#   geom_sf(data = africapitals,
#           mapping = aes(size = pop), alpha =0.5) +
#   # colorspace::scale_color_continuous_sequential() +
#   labs(title = "Dot map") +
#   theme(legend.position = "none",
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         plot.title = element_text(size = 11),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank())
# 
# ggplot() +
#   geom_sf(data = geoboundaries(country = c""))
#   geom_sf(data = africapitals,
#           mapping = aes(size = pop), alpha =0.5) +
#   # colorspace::scale_color_continuous_sequential() +
#   labs(title = "Dot map") +
#   theme(legend.position = "none",
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         plot.title = element_text(size = 11),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank())


# france example ----------------------------------------------------------

# pacman::p_load(patchwork)
# pacman::p_load(dplyr)
# pacman::p_load(stringr)
# pacman::p_load(ggplot2)
# pacman::p_load(rnaturalearth)
# pacman::p_load(rgeoboundaries)
# pacman::p_load(geodata)
# pacman::p_load(sf)
# theme_set(theme_bw())
# 
# paris <- gadm(country = "France", level = 3, path = tempdir()) %>% 
#   st_as_sf() %>% 
#   filter(NAME_2 == "Paris") %>% 
#   mutate(NAME_3 = str_trim(NAME_3,side = "left")) %>% 
#   mutate(NAME_3 = str_replace(NAME_3,"12e arronissement","10e arrondissement")) %>% 
#   mutate(NAME_3 = str_replace(NAME_3," arrondissement","")) %>% 
#   mutate(NAME_3 = str_replace(NAME_3,"Paris, ",""))
# 
# paris %>%
#   ggplot() +
#   geom_sf(data = geoboundaries(country = "france")) +
#   geom_sf() +
#   ggsflabel::geom_sf_text_repel(
#     mapping = aes(label = NAME_3),
#     size = 2)
#   
# ire3 %>%
#   st_as_sf() %>%
#   filter(NAME_1 == "Santa Cruz") %>%
#   ggplot() +
#   geom_sf(data = geoboundaries(country = "bolivia")) +
#   geom_sf(aes(fill = NAME_1)) +
#   ggsflabel::geom_sf_text_repel(
#     mapping = aes(label = NAME_2),
#     size = 2)
# 
# pc <- ggplot() +
#   geom_sf(data = geoboundaries(country = "bolivia")) +
#   geom_sf(data = ire3 %>% 
#             st_as_sf() %>% 
#             filter(NAME_1 == "Santa Cruz"),
#           mapping = aes(fill = NAME_1)) +
#   # ggsflabel::geom_sf_label_repel(data = ire1 %>% 
#   #                                  st_as_sf() %>% 
#   #                                  filter(NAME_1 == "Kilkenny"),
#   #                                mapping = aes(label = NAME_1),
#   #                                ) +
#   ggsflabel::geom_sf_text_repel(data = ire3 %>%
#                                   st_as_sf() %>%
#                                   filter(NAME_1 == "Santa Cruz"),
#                                 mapping = aes(label = NAME_2),
#                                 size = 2,
#                                 # force        = 0.5,
#                                 # box.padding  = 0.6,
#                                 # nudge_x      = -0.25,
#                                 # direction    = "y",
#                                 # hjust        = 1,
#                                 # segment.size = 0.2
#   ) +
#   theme(legend.position = c(0.75,0.9),
#         legend.key.height = unit(0.2,"in"),
#         legend.key.width = unit(0.2,"in"),
#         legend.title = element_text(size = 7),
#         legend.text = element_text(size = 7)) +
#   labs(title = "{geodata}",
#        subtitle = "Bolivia: Departments and Provinces",
#        fill = "Department",
#        x = NULL,
#        y = NULL)


# ls08 intro figure -------------------------------------------------------


pacman::p_load(patchwork)
pacman::p_load(dplyr)
pacman::p_load(ggplot2)
pacman::p_load(rnaturalearth)
pacman::p_load(rgeoboundaries)
pacman::p_load(geodata)
pacman::p_load(sf)
pacman::p_load(malariaAtlas)
theme_set(theme_bw())


ire1 <- gadm(country = "bolivia",level = 1,path = tempdir())
ire3 <- gadm(country = "bolivia",level = 2,path = tempdir())

# ire3 %>%
#   st_as_sf() %>%
#   as_tibble() %>%
#   count(NAME_1,NAME_2) %>%
#   count(NAME_1,sort = T)
# 
# ire3 %>%
#   st_as_sf() %>%
#   filter(NAME_1 == "Santa Cruz") %>%
#   ggplot() +
#   geom_sf(data = geoboundaries(country = "bolivia")) +
#   geom_sf(aes(fill = NAME_1)) +
#   ggsflabel::geom_sf_text_repel(
#     mapping = aes(label = NAME_2),
#     size = 2)

pc <- ggplot() +
  geom_sf(data = geoboundaries(country = "bolivia")) +
  geom_sf(data = ire3 %>% 
            st_as_sf() %>% 
            filter(NAME_1 == "Santa Cruz"),
          mapping = aes(fill = NAME_1)) +
  # ggsflabel::geom_sf_label_repel(data = ire1 %>% 
  #                                  st_as_sf() %>% 
  #                                  filter(NAME_1 == "Kilkenny"),
  #                                mapping = aes(label = NAME_1),
  #                                ) +
  ggsflabel::geom_sf_text_repel(data = ire3 %>%
                                  st_as_sf() %>%
                                  filter(NAME_1 == "Santa Cruz"),
                                mapping = aes(label = NAME_2),
                                size = 2,
                                # force        = 0.5,
                                # box.padding  = 0.6,
                                # nudge_x      = -0.25,
                                # direction    = "y",
                                # hjust        = 1,
                                # segment.size = 0.2
  ) +
  theme(legend.position = c(0.75,0.9),
        legend.key.height = unit(0.2,"in"),
        legend.key.width = unit(0.2,"in"),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7)) +
  labs(title = "Bolivia: departments and provinces",
       subtitle = "{geodata} different from sf",
       fill = "Department",
       x = NULL,
       y = NULL)
# pc


brazil_malaria_all <- getPR(country = "brazil", species = "BOTH")

brazil_malaria_all %>% 
  as_tibble() %>% 
  count(year_start) %>% 
  epihelper::print_inf()

brazil_malaria_pr <- brazil_malaria_all %>% 
  as_tibble() %>% filter(year_start>2000)

brazil_malaria_pr_sf <- 
  # data frame
  brazil_malaria_pr %>% 
  # convert to sf
  sf::st_as_sf(coords = c("longitude","latitude"),  #👈👈👈👈👈👈
               crs = 4326)

bra1 <- brazil_malaria_pr %>% 
  mutate(pr = 100*pr) %>% 
  ggplot() +
  geom_point(aes(x = longitude,y = latitude,
                 size = pr),
             alpha=0.25) +
  # theme(legend.position = "none") +
  labs(title = "Foreign spatial data",
       subtitle = "data.frame class object",
       caption = "Brazil, 2000-2013",
       size = "% annual\nprevalence\nof malaria",
       x = NULL,
       y = NULL)

bra2 <- ggplot() +
  geom_sf(data = geoboundaries(country = "brazil",adm_lvl = 1)) +
  geom_sf(data = brazil_malaria_pr_sf %>% mutate(pr = 100*pr),
          mapping = aes(size = pr),
          color = "red",
          alpha=0.15) +
  # scale_color_continuous()
  labs(title = "Converted spatial data",
       subtitle = "sf class object",
       caption = "Brazil, 2000-2013",
       size = "% annual\nprevalence\nof malaria")

ls08intro <- 
  bra1 +
  bra2 +
  # pc +
  plot_annotation(tag_levels = "A") +
  plot_layout(ncol = 2,
              widths = c(4,4))

# ls08intro

ggsave(
  plot = ls08intro,
  filename = here::here("ch06_basic_geospatial_viz",
                        "lessons","images", "geoboundaries_05.png"),
  width = 9,height = 4,
  dpi = "retina")
