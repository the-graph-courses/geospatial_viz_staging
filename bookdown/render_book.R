# Render bookdown
## GRAPH Courses team
## 2022-06-19

#' Copy all Rmd's and images to the bookdown/lessons folder, then render from there.

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, fs, here)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Delete all Rmds except index.Rmd ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Useful in case there are old lessons from a previous copy we no longer want to use. (Wouldn't be solved with a simple overwrite)

rmds_to_del <-
  fs::dir_ls(
    path = here("bookdown/lessons"),
    type = "file",
    regexp = "[.]Rmd$"
  ) %>% 
  .[-which(str_detect(., "index.Rmd"))]
fs::file_delete(rmds_to_del)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Copy Rmds and images to bookdown/lessons ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Copy Rmds ----

rmds_to_copy <- 
  fs::dir_ls(type = "file", 
             regexp = "lessons/ch\\d\\d_ls\\d\\d_.*.Rmd", 
             recurse = T) %>% 
    # drop template
    .[-which(str_detect(., "ch99"))] %>% 
    .[-which(str_detect(., "ls99"))]  %>% 
    # temporarily remove lesson 4 on functions, which seems to be triggering some pagedown error
    .[-which(str_detect(., "ch04_ls04"))] 
  
rmds_basename <- basename(rmds_to_copy)

fs::file_copy(rmds_to_copy, 
              new_path = here(paste0("bookdown/lessons/", rmds_basename)))

### Copy images ----

images_to_copy <- 
  fs::dir_ls(type = "file", 
             regexp = "ch\\d\\d_.*/images", 
             recurse = T) %>% 
  # drop template
  .[-which(str_detect(., "ch99"))]

images_basename <- basename(images_to_copy)

fs::file_copy(images_to_copy, 
              new_path = here(paste0("bookdown/images/", images_basename)), 
              overwrite = T)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Render bookdown ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bookdown::render_book(input = here("bookdown/lessons"))

