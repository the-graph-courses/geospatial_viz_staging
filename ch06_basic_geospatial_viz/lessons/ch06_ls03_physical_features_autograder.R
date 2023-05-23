# density Autograder
## Andree Valle Campos
## 2022-05-05

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(tidyverse,
               praise,
               here)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 1)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pacman::p_load(terra,
               spData)
.shape_file1 <- world

pacman::p_load_gh("afrimapr/afrilearndata")
.shape_file3 <- afrihighway

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- ggplot() + geom_sf(data = .shape_file1) + geom_sf(data = .shape_file3) # write correct answer
    
    .autograder <<-
      function(){
        if (!is.ggplot(q1)) return(c(value = -1, message = "Your result should be a ggplot2 object."))
        
        .q1_twolayers <- try(layer_data(q1,2),silent = TRUE)
        .q1_twolayers_test <- class(.q1_twolayers) == "try-error"
        
        if (isTRUE(.q1_twolayers_test)) return(c(value = 0, message = "! Do not forget to use two geometry layers."))
        
        # test set
        .q1_correct_layer1 <- layer_data(.q1_correct,i = 1) %>% as_tibble() %>% select(-geometry)
        .q1_correct_layer2 <- layer_data(.q1_correct,i = 2) %>% as_tibble() %>% select(-geometry)
        .q1_layer1 <- layer_data(q1,i = 1) %>% as_tibble() %>% select(-geometry)
        .q1_layer2 <- layer_data(q1,i = 2) %>% as_tibble() %>% select(-geometry)
        
        # test 1
        # that database layer 1 is correct
        pacman::p_load(dplyr)
        .q1_test1 <- all_equal(
          target = .q1_layer1, 
          current = .q1_correct_layer1)
        
        # test 2
        # that database layer 2 is correct
        .q1_test2 <- all_equal(
          target = .q1_layer2, 
          current = .q1_correct_layer2)
        
        if (!(isTRUE(.q1_test1))) return(c(value = 0, message = "! Do not forget to use the 'world' dataset as your first layer."))
        if (isTRUE(.q1_test1 && .q1_test2)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q1 <- function(){
  'Data objects should be located in different ggplot2 layers' -> out
  cat(out)
}
# solution of question
.solution_q1 <- function(){
  'ggplot() + 
    geom_sf(data = world) + 
    geom_sf(data = afrihighway)' -> out
  cat(out)
}

# [frontend]
# to paste in lesson
q1 <- "YOUR ANSWER HERE"
.check_q1()
.hint_q1()

# [backend]
# test the check geom_sf(aes(fill = type)) function
q1 <- ggplot() + geom_sf(data = .shape_file1)
.check_q1()
q1 <- ggplot() + geom_sf(data = .shape_file3) + geom_sf(data = .shape_file1)
.check_q1()
q1 <- ggplot() + geom_sf(data = .shape_file1) + geom_sf(data = .shape_file3)
.check_q1()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# _rstudio cloud 1_ ---------------------------------------------------------------

# ggplot(data = sacramento_rivers) + 
#   geom_sf(aes(color = FTYPE), size = 1)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# _rstudio cloud 2_ ---------------------------------------------------------------

# ggplot(data = south_am_roads) + 
#   geom_sf(aes(color = length_km), size = 1)
