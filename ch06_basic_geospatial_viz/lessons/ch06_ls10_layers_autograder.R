# Layers Autograder 
## Andree Valle Campos
## 2022-06-04

#' Autograder for the layers lesson

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(tidyverse,
               praise,
               here)

pacman::p_load(tidyverse,
               ggspatial,
               janitor,
               ggplot2,
               readxl,
               sf)

pacman::p_load_gh("afrimapr/afrihealthsites",
                  "wmgeolab/rgeoboundaries",
                  "yutannihilation/ggsflabel")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 5)   # Put total number of questions as `times` argument


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- readxl::read_xlsx(here::here("ch06_basic_geospatial_viz",
                                                "data", "gps_healthsites.xlsx")) # write correct answer
    # .q1_mistake1 <- readxl::read_xlsx(here::here("ch06_basic_geospatial_viz",
    #                                              "data", "gps_healthsites.xlsx")) # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (is.character(q1)) .na(message = "Your result should be tibble data frame.")
        # if (isTRUE(all.equal(q1, .q1_mistake1))) .fail(message = "! Check if the coordinate values are correctly located.")
        if (isTRUE(all.equal(q1, .q1_correct))) .pass()
        else .fail()
      }
    .run_autograder()
  }

# [backend]
# create one hint per question
.hint_q1 <- function(){
'Be careful with the file format extention of the file!' -> out
cat(out)
}
# solution of question
.solution_q1 <- function(){
  'read_xlsx(here("ch06_basic_geospatial_viz/data/gps_healthsites.xlsx"))' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q1 <- "YOUR ANSWER HERE"
# .check_q1()
# .hint_q1()

# # [backend]
# # test solution
# .solution_q1()
# # test the check function
# q1 <- c(1,1)
# .check_q1()
# q1 <- read_xlsx(here("ch06_basic_geospatial_viz/data/gps_healthsites.xlsx"))
# .check_q1()
# #q1 <- read_xlsx(here("ch06_basic_geospatial_viz/data/gps_healthsites.xlsx"))
# #.check_q1()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.shape_data02 <- rgeoboundaries::geoboundaries(country = "Zimbabwe", adm_lvl = 1)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- .shape_data02 %>% ggplot() + geom_sf() + geom_sf_label_repel(aes(label = shapeName)) # write correct answer
    .q2_mistake1 <- .shape_data02 %>% ggplot() + geom_sf() + geom_sf_label(aes(label = shapeName)) # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (is.character(q2)) .na(message = "Your result should be a data frame object.")
        if (isTRUE(all.equal(q2, .q2_mistake1))) .fail(message = "! The labels look overlapped, try to use the *_repel variant.")
        if (isTRUE(all.equal(q2, .q2_correct))) .pass()
        else .fail()
      }
    .run_autograder()
  }

# [backend]
# create one hint per question
.hint_q2 <- function(){
'The name for each province is inside the shapeName variable' -> out
cat(out)
}
# solution of question
.solution_q2 <- function(){
'zimbabwe_adm1 %>% 
   ggplot() + 
   geom_sf() + 
   geom_sf_label_repel(aes(label = shapeName))' -> out
cat(out)
}

# # [frontend]
# # to paste in lesson
# q2 <- "YOUR ANSWER HERE"
# .check_q2()
# .hint_q2()

# # [backend]
# # test solution
# .solution_q2()
# # test the check function
# q2 <- .shape_data02
# .check_q2()
# q2 <- .shape_data02 %>% ggplot() + geom_sf() + geom_sf_label(aes(label = shapeName))
# .check_q2()
# q2 <- .shape_data02 %>% ggplot() + geom_sf() + geom_sf_label_repel(aes(label = shapeName))
# .check_q2()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pacman::p_load_gh("afrimapr/afrilearndata")
.shape_data03 <- afriairports %>% 
  ggplot() +
  geom_sf(aes(color = elevation_ft))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- .shape_data03 + colorspace::scale_color_continuous_diverging(mid = 5000) # write correct answer
    .q3_mistake1 <- .shape_data03 + colorspace::scale_color_continuous_diverging() # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (is.character(q3)) .na(message = "Your result should be a ggplot2 object.")
        
        .q3_test0 <- ggplot_build(q3) %>% pluck("data") %>% pluck(1) %>% as_tibble() %>% pull(colour)
        .q3_test1 <- ggplot_build(.q3_correct) %>% pluck("data") %>% pluck(1) %>% as_tibble() %>% pull(colour)
        .q3_test2 <- ggplot_build(.q3_mistake1) %>% pluck("data") %>% pluck(1) %>% as_tibble() %>% pull(colour)
        
        if (isTRUE(all.equal(.q3_test0, .q3_test2))) .fail(message = "! This map have a default midpoint for the color scale.")
        if (isTRUE(all.equal(.q3_test0, .q3_test1))) .pass()
        else .fail()
      }
    .run_autograder()
  }

# [backend]
# create one hint per question
.hint_q3 <- function(){
'Scales in {colorspace} are called via the scheme: 
  scale_<aesthetic>_<datatype>_<colorscale>()' -> out
cat(out)
}
# solution of question
.solution_q3 <- function(){
'afriairports %>% 
   ggplot() + 
   geom_sf(aes(color = elevation_ft)) + 
   colorspace::scale_color_continuous_diverging(mid = 5000)' -> out
cat(out)
}

# # [frontend]
# # to paste in lesson
# q3 <- "YOUR ANSWER HERE"
# .check_q3()
# .hint_q3()
# 
# # [backend]
# # test solution
# .solution_q3()
# # test the check function
# q3 <- .shape_data03
# .check_q3()
# q3 <- .shape_data03 + colorspace::scale_color_continuous_diverging()
# .check_q3()
# q3 <- .shape_data03 + colorspace::scale_color_continuous_diverging(mid = 5000)
# .check_q3()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.shape_data04 <- rgeoboundaries::geoboundaries(country = "Zimbabwe", 
                                               adm_lvl = 1) %>% 
  ggplot() + 
  geom_sf()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- .shape_data04 + annotation_north_arrow(location = "tl") + annotation_scale(location = "br") # write correct answer
    .q4_correct2 <- .shape_data04 + annotation_scale(location = "br") + annotation_north_arrow(location = "tl") # write correct answer
    .q4_mistake1 <- .shape_data04 + annotation_north_arrow() + annotation_scale() # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (is.character(q4)) .na(message = "Your result should be a ggplot object.")
        if (!is.ggplot(q4)) .fail()
        
        .q4_test0 <- any(str_detect(capture.output(q4$layers),"geom_north_arrow")) | any(str_detect(capture.output(q4$layers),"geom_scale_bar"))
        
        if (!isTRUE(.q4_test0)) .fail(message = "! This map do not have annotations.")
        
        .q4_test1_a <- ggplot_build(q4) %>% pluck("data") %>% pluck(2) %>% pull(location)
        .q4_test2_a <- ggplot_build(q4) %>% pluck("data") %>% pluck(3) %>% pull(location)
        .q4_test1_c <- ggplot_build(.q4_correct) %>% pluck("data") %>% pluck(2) %>% pull(location)
        .q4_test2_c <- ggplot_build(.q4_correct) %>% pluck("data") %>% pluck(3) %>% pull(location)
        .q4_test3_c <- ggplot_build(.q4_correct2) %>% pluck("data") %>% pluck(2) %>% pull(location)
        .q4_test4_c <- ggplot_build(.q4_correct2) %>% pluck("data") %>% pluck(3) %>% pull(location)
        .q4_test1_m <- ggplot_build(.q4_mistake1) %>% pluck("data") %>% pluck(2) %>% pull(location)
        .q4_test2_m <- ggplot_build(.q4_mistake1) %>% pluck("data") %>% pluck(3) %>% pull(location)
        
        .q4_eval1 <- isTRUE(all.equal(.q4_test1_a, .q4_test1_m))
        .q4_eval2 <- isTRUE(all.equal(.q4_test2_a, .q4_test2_m))
        .q4_eval3 <- isTRUE(all.equal(.q4_test1_a, .q4_test1_c))
        .q4_eval4 <- isTRUE(all.equal(.q4_test2_a, .q4_test2_c))
        .q4_eval5 <- isTRUE(all.equal(.q4_test1_a, .q4_test3_c))
        .q4_eval6 <- isTRUE(all.equal(.q4_test2_a, .q4_test4_c))
        
        if (.q4_eval1 && .q4_eval2) .fail(message = "! The location of the North arrow or Scale bar are set by default.")
        if (.q4_eval3 && .q4_eval4) .pass()
        if (.q4_eval5 && .q4_eval6) .pass()
        else .fail()
      }
    .run_autograder()
  }

# [backend]
# create one hint per question
.hint_q4 <- function(){
'Locations need to be defined in function arguments by acronyms' -> out
cat(out)
}
# solution of question
.solution_q4 <- function(){
'zimbabwe_adm1 %>%
  ggplot() +
  geom_sf() +
  annotation_north_arrow(location = "tl") +
  annotation_scale(location = "br")' -> out
cat(out)
}

# # [frontend]
# # to paste in lesson
# q4 <- "YOUR ANSWER HERE"
# .check_q4()
# .hint_q4()
# 
# # [backend]
# # test solution
# .solution_q4()
# # test the check function
# q4 <- iris
# .check_q4()
# q4 <- .shape_data04
# .check_q4()
# q4 <- .shape_data04 + annotation_north_arrow() + annotation_scale()
# .check_q4()
# q4 <- .shape_data04 + annotation_scale(location = "br") + annotation_north_arrow(location = "tl")
# .check_q4()
# q4 <- .shape_data04 + annotation_north_arrow(location = "tl") + annotation_scale(location = "br")
# .check_q4()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q5 data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.shape_data05 <- afrihealthsites(country = "Zimbabwe", 
                                 datasource='healthsites',
                                 plot = FALSE) %>% 
  filter(amenity == "hospital")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q5 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q5 <-
  function() {
    .problem_number <<- 5
    
    .q5_correct <- .shape_data05 %>% ggplot() + geom_sf() + geom_sf_text_repel(aes(label = name)) # write correct answer
    .q5_mistake1 <- .shape_data05  %>% ggplot() + geom_sf() + geom_sf_label_repel(aes(label = name)) # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (is.character(q5)) .na(message = "Your result should be a data frame object.")
        if (isTRUE(all.equal(q5, .q5_mistake1))) .fail(message = "! This map have the names as labels.")
        if (isTRUE(all.equal(q5, .q5_correct))) .pass()
        else .fail()
      }
    .run_autograder()
  }

# [backend]
# create one hint per question
.hint_q5 <- function(){
' - Find the column that refers to the name of the health site.
 - The name need to be specified in the label argument of the aes() function' -> out
cat(out)
}
# solution of question
.solution_q5 <- function(){
'Type:
  afrihealthsites(country = "Zimbabwe", 
                                 datasource="healthsites",
                                 plot = FALSE) %>% 
  filter(amenity == "hospital") %>% 
  ggplot() +
  geom_sf() + 
  geom_sf_text_repel(aes(label = name))' -> out
cat(out)
}

# # [frontend]
# # to paste in lesson
# q5 <- "YOUR ANSWER HERE"
# .check_q5()
# .hint_q5()
# # 
# # # [backend]
# # # test solution
# .solution_q5()
# # test the check function
# q5 <- .shape_data05
# .check_q5()
# q5 <- .shape_data05 %>% ggplot() + geom_sf() + geom_sf_label_repel(aes(label = name))
# .check_q5()
# q5 <- .shape_data05 %>% ggplot() + geom_sf() + geom_sf_text_repel(aes(label = name))
# .check_q5()
