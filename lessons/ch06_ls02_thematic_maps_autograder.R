# Intro GIS part 1 Autograder
## Andree Valle Campos
## 2022-05-05

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(tidyverse,
               praise,
               here,
               dplyr)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 4)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pacman::p_load(terra,
               spData)
.shape_file1 <- world

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- ggplot(data = .shape_file1) + geom_sf(aes(fill = pop)) # write correct answer
    # .q1_mistake1 <- ggplot(data = .shape_file1) # optional: highlight common mistake
    # .q1_mistake2 <- ggplot(data = .shape_file1) + geom_sf() # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!is.ggplot(q1)) return(c(value = -1, message = "Your result should be a ggplot2 object."))
        
        # reference: https://renkulab.io/gitlab/the-graph-courses/general/-/blob/master/notebooks/interrogating_ggplot_objects.Rmd
        
        # test 1
        # that database is correct
        pacman::p_load(dplyr)
        .q1_test1 <- all_equal(
          target = as_tibble(q1$data) %>% select(-geom), 
          current = as_tibble(.q1_correct$data) %>% select(-geom))
        
        # test 2
        # that learner used geom_sf()
        .q1_test2 <- any(stringr::str_detect(capture.output(q1$layers), 
                                             "geom_sf"))
        # test 3
        # that learner used fill to shapeleng
        .q1_test3 <- any(stringr::str_detect(capture.output(q1$layers), "fill = ~pop"))
        
        if (isTRUE(!(.q1_test1 && .q1_test2))) return(c(value = 0, message = "! Do not forget to use ggplot2 geometry function for spatial data objects."))
        if (isTRUE(!(.q1_test1 && .q1_test2 && .q1_test3))) return(c(value = 0, message = "! Do not forget to add the variable 'pop' inside a mapping argument."))
        if (isTRUE(.q1_test1 && .q1_test2 && .q1_test3)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q1 <- function(){
  'First, identify the Vector data type of the spatial data object. 
 Then decide if you need the "fill" or "color" argument inside the aes() function' -> out
  cat(out)
}
# solution of question
.solution_q1 <- function(){
  'ggplot(data = world) + 
    geom_sf(aes(fill = pop))' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q1 <- "YOUR ANSWER HERE"
# .check_q1()
# .hint_q1()
# 
# # [backend]
# # test the check geom_sf(aes(fill = pop)) function
# q1 <- ggplot(data = .shape_file1)
# .check_q1()
# q1 <- ggplot(data = .shape_file1) + geom_sf()
# .check_q1()
# q1 <- ggplot(data = .shape_file1) + geom_sf(aes(fill = pop))
# .check_q1()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pacman::p_load_gh("afrimapr/afrilearndata")
.shape_file2 <- afriairports


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- ggplot(data = .shape_file2) + geom_sf(aes(fill = type)) # write correct answer
    # .q2_mistake1 <- ggplot(data = .shape_file2) # optional: highlight common mistake
    # .q2_mistake2 <- ggplot(data = .shape_file2) + geom_sf() # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!is.ggplot(q2)) return(c(value = -1, message = "Your result should be a ggplot2 object."))
        
        # reference: https://renkulab.io/gitlab/the-graph-courses/general/-/blob/master/notebooks/interrogating_ggplot_objects.Rmd
        
        # test 1
        # that database is correct
        pacman::p_load(dplyr)
        .q2_test1 <- all_equal(
          target = as_tibble(q2$data) %>% select(-geometry), 
          current = as_tibble(.q2_correct$data) %>% select(-geometry))
        
        # test 2
        # that learner used geom_sf()
        .q2_test2 <- any(stringr::str_detect(capture.output(q2$layers), 
                                             "geom_sf"))
        # test 3
        # that learner used fill to shapeleng
        .q2_test3 <- any(stringr::str_detect(capture.output(q2$layers), "fill = ~type"))
        
        if (isTRUE(!(.q2_test1 && .q2_test2))) return(c(value = 0, message = "! Do not forget to use ggplot2 geometry function for spatial data objects."))
        if (isTRUE(!(.q2_test1 && .q2_test2 && .q2_test3))) return(c(value = 0, message = "! Do not forget to add the variable 'type' inside a mapping argument."))
        if (isTRUE(.q2_test1 && .q2_test2 && .q2_test3)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q2 <- function(){
  'First, identify the Vector data type of the spatial data object. 
 Then decide if you need the "fill" or "color" argument inside the aes() function' -> out
  cat(out)
}
# solution of question
.solution_q2 <- function(){
  'ggplot(data = afriairports) + 
    geom_sf(aes(fill = type))' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q2 <- "YOUR ANSWER HERE"
# .check_q2()
# .hint_q2()
# 
# # [backend]
# # test the check geom_sf(aes(fill = type)) function
# q2 <- ggplot(data = .shape_file2)
# .check_q2()
# q2 <- ggplot(data = .shape_file2) + geom_sf()
# .check_q2()
# q2 <- ggplot(data = .shape_file2) + geom_sf(aes(fill = type))
# .check_q2()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- "choropleth_map" # write correct answer
    .q3_mistake3 <- "dot_distribution_map" # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!is.character(q3) || grepl(pattern = " ",x = q3)) 
          return(c(value = -1, message = "! You should paste the whole text of only one of the options."))
        if(q3=="a"| q3=="a.")
          return(c(value = -1, message = "! You should input the name of the map, not a. or b."))
        if (isTRUE(all.equal(q3, .q3_mistake3))) return(c(value = 0, message = "! If data is depicted inside a whole region, it may not be a dot distribution map."))
        if (isTRUE(all.equal(q3, .q3_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q3 <- function(){
  'Identify if cases are depicted by regions or dots.' -> out
  cat(out)
}
# solution of question
.solution_q3 <- function(){
  'choropleth_map' -> out
  cat(out)
}

# # [frontend]
# q3 <- "YOUR ANSWER HERE"
# .check_q3()
# 
# # [backend test]
# q3 <- 5
# .check_q3()
# 
# q3 <- "dot_distribution_map"
# .check_q3()
# 
# q3 <- "choropleth_map"
# .check_q3()
# 
# .hint_q3()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- "dot_distribution_map" # write correct answer
    .q4_mistake4 <- "choropleth_map" # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!is.character(q4) || grepl(pattern = " ",x = q4)) 
          return(c(value = -1, message = "! You should paste the whole text of only one of the options."))
        if (isTRUE(all.equal(q4, .q4_mistake4))) return(c(value = 0, message = "! Differenciate between the background map and the data that depict the cases distribution."))
        if (isTRUE(all.equal(q4, .q4_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q4 <- function(){
  'Identify if cases are depicted by regions or dots.' -> out
  cat(out)
}
# solution of question
.solution_q4 <- function(){
  'dot_distribution_map' -> out
  cat(out)
}

# # [frontend]
# q4 <- "YOUR ANSWER HERE"
# .check_q4()
# 
# # [backend test]
# q4 <- 5
# .check_q4()
# 
# q4 <- "choropleth_map"
# .check_q4()
# 
# q4 <- "dot_distribution_map"
# .check_q4()
# 
# .hint_q4()