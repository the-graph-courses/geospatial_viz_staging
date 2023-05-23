# density Autograder
## Andree Valle Campos
## 2022-05-05

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load_gh("afrimapr/afrilearndata")
pacman::p_load(tidyverse,
               ggspatial,
               praise,
               here)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 4)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.internal_pcrime <- read_rds(here("ch06_basic_geospatial_viz/data/pcrime.rds"))

# .internal_pcrime %>% 
#   epihelper::st_coordinates_tidy() %>% 
#   ggplot(aes(x = X,y = Y)) +
#   geom_bin_2d() +
#   coord_sf() +
#   facet_wrap(~marks)

.solution_q1 <- function(){
  'pcrime %>% 
    epihelper::st_coordinates_tidy() %>% 
    ggplot(aes(x = X,y = Y)) +
    geom_bin_2d() +
    coord_sf() +
    facet_wrap(~marks)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# .internal_pcrime %>% 
#   epihelper::st_coordinates_tidy() %>% 
#   ggplot(aes(x = X,y = Y)) +
#   geom_density_2d_filled() +
#   coord_sf() +
#   facet_wrap(~marks)

.solution_q2 <- function(){
  'pcrime %>% 
    epihelper::st_coordinates_tidy() %>% 
    ggplot(aes(x = X,y = Y)) +
    geom_density_2d_filled() +
    coord_sf() +
    facet_wrap(~marks)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ggplot(data = africapitals) +
#   annotation_map_tile() + 
#   geom_sf(mapping = aes(size = pop), alpha = 0.5)

.solution_q3 <- function(){
  'ggplot(data = africapitals) +
    annotation_map_tile() + 
    geom_sf(mapping = aes(size = pop), alpha = 0.5)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# afriairports %>% 
#   epihelper::st_coordinates_tidy() %>% 
#   ggplot(aes(x = X, y = Y)) +
#   annotation_map_tile() + 
#   geom_bin_2d() +
#   coord_sf()

.solution_q4 <- function(){
  'afriairports %>% 
    epihelper::st_coordinates_tidy() %>% 
    ggplot(aes(x = X, y = Y)) +
    annotation_map_tile() + 
    geom_bin_2d() +
    coord_sf()' -> out
  cat(out)
}
