# Select Autograder (Template)
## GRAPH Courses Team
## 2022-04-05

#' Autograder for the template script

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Packages and settings ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,
               praise,
               here)
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load_gh("afrimapr/afrilearndata")
.shape_file2 <- afriairports



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 4)   # Put total number of questions as `times` argument

.NUM_Q_add_two <- 1
.NUM_Q_fever_function <- 2
.NUM_Q_loop_mean <- 3
.NUM_Q_gg_chor <- 4

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q_add_two ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_add_two <-
  function() {
    
    .problem_number <<- .NUM_Q_add_two
    
    .Q_add_two <- function(x) x + 2
  
    .autograder <<-
      function(){
        if(!exists("Q_add_two")) 
          .na("You have not yet defined the answer.")
        if (!is.function(Q_add_two)) 
          .na(message = "Your answer should be a function.")
        
        if (isTRUE(all.equal(.Q_add_two(2), 
                             Q_add_two(2))) &&
            isTRUE(all.equal(.Q_add_two(1:10), 
                             .Q_add_two(1:10))))
          .pass()
        else 
          .fail()
      }
    .run_autograder()
  }

# [backend]
# create one hint per question
.HINT_Q_add_two <- function(){
'HINT: 
  function(x){
    # code to add 2 to x in here
  }' -> out
cat(out)
}
# solution of question
.SOLUTION_Q_add_two <- function(){
'SOLUTION: 
  function(x) {
  x + 2
  }' -> out
cat(out)
}

# [backend]
# test solution
# .SOLUTION_Q_add_two()
# # test the check function
# Q_add_two <- "YOUR ANSWER HERE"
# .CHECK_Q_add_two()
# Q_add_two <- function(x) x
# .CHECK_Q_add_two()
# Q_add_two <- function(x) x + 2
# .CHECK_Q_add_two()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q_fever_function ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_fever_function <-
  function() {
    .problem_number <<- .NUM_Q_fever_function

    .autograder <<-
      function(){
        if(!exists("Q_fever_function")) 
          .na("You have not yet defined the answer.")
        if (!is.function(Q_fever_function)) 
          .na(message = "Your answer should be a function.")
        if (is.null(Q_fever_function(35)) && 
            str_extract(tolower(Q_fever_function(38)), "[[:alpha:]]+") == "fever" && 
            str_extract(tolower(Q_fever_function(42)), "[[:alpha:]]+") == "fever")
          .pass()
        else 
          .fail()
      }
    .run_autograder()
  }

# create one hint per question
.HINT_Q_fever_function <- function(){
  'HINT: 
    function(x){
      if(CONDITION_HERE) "STRING_OUTPUT_HERE"
    }' -> out
  cat(out)
}

# solution of question
.SOLUTION_Q_fever_function <- function(){
'SOLUTION: 
    function(x){
    if(x >= 38) "Fever"
    }' -> out
  cat(out)
}

# # tests
# .HINT_Q_fever_function()
# .SOLUTION_Q_fever_function()
# # test the check function
# Q_fever_function <- "YOUR ANSWER HERE"
# .CHECK_Q_fever_function()
# Q_fever_function <- function(x) x
# .CHECK_Q_fever_function()
# Q_fever_function <- function(x) if(x >= 38) "Fever"
# .CHECK_Q_fever_function()




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q_loop_mean ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_loop_mean <-
  function() {
    .problem_number <<- .NUM_Q_loop_mean
    
    set.seed(1)
    .df2 <- 
      tibble(
        a = rnorm(10),
        b = rnorm(10),
        c = rpois(10, 10),
        d = rnorm(10),
        e = rpois(10, 3),
        f = rnorm(10),
      )
    
    .df2_means <- vector()
    
    for (i in 1:ncol(df2)) {
      .df2_means[[i]] <- mean(.df2[[i]])
    }
    
    .Q_loop_mean <- .df2_means
    
    .autograder <<-
      function(){
        if(!exists("Q_loop_mean")) 
          .na("You have not yet defined the answer. Please assign the answer to the object Q_loop_mean.")
        if (!is.vector(Q_loop_mean)) 
          .na(message = "Your answer should be a vector.")
        if (isTRUE(all.equal(Q_loop_mean, .Q_loop_mean)))
          .pass()
        else 
          .fail()
      }
    .run_autograder()
  }


.HINT_Q_loop_mean <- function(){
  'HINT: 
  Inside your for loop, you should iterate over `.df2_means` with `.df2_means[[i]]` 
  and iterate over `.df2` with `.df2[[i]]`' -> out
  cat(out)
}

.SOLUTION_Q_loop_mean <- function(x){
  '
  SOLUTION:
  
  .df2_means <- vector()
    
    for (i in 1:ncol(df2)) {
      .df2_means[[i]] <- mean(.df2[[i]])
    }
    
    .Q_loop_mean <- .df2_means '-> out
  
  cat(out)
}


# Test hint and solution
# .HINT_Q_loop_mean()
# .SOLUTION_Q_loop_mean()
# 
# # Test NULL
# rm(Q_loop_mean)
# .CHECK_Q_loop_mean()
# 
# # Test incorrect
# Q_loop_mean <- 1:10
# .CHECK_Q_loop_mean()
# 
# # Test correct 
# set.seed(1)
# df2 <- tibble(
#   a = rnorm(10),
#   b = rnorm(10),
#   c = rpois(10, 10),
#   d = rnorm(10),
#   e = rpois(10, 3),
#   f = rnorm(10),
# )
# df2_means <- vector()
# for (i in 1:ncol(df2)) {
#   df2_means[[i]] <- mean(df2[[i]])
# }
# Q_loop_mean <- df2_means
# .CHECK_Q_loop_mean()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q_gg_chor ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.CHECK_Q_gg_chor <-
  function() {
    .problem_number <<- .NUM_Q_gg_chor
    
    .Q_gg_chor_correct <- ggplot(data = .shape_file2) + geom_sf(aes(fill = type)) # write correct answer
    # .Q_gg_chor_mistake1 <- ggplot(data = .shape_file2) # optional: highlight common mistake
    # .Q_gg_chor_mistake2 <- ggplot(data = .shape_file2) + geom_sf() # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!is.ggplot(Q_gg_chor)) return(c(value = -1, message = "Your result should be a ggplot2 object."))
        
        # reference: https://renkulab.io/gitlab/the-graph-courses/general/-/blob/master/notebooks/interrogating_ggplot_objects.Rmd
        
        # test 1
        # that database is correct
        pacman::p_load(dplyr)
        .Q_gg_chor_test1 <- all_equal(
          target = as_tibble(Q_gg_chor$data) %>% select(-geometry), 
          current = as_tibble(.Q_gg_chor_correct$data) %>% select(-geometry))
        
        # test 2
        # that learner used geom_sf()
        .Q_gg_chor_test2 <- any(stringr::str_detect(capture.output(Q_gg_chor$layers), 
                                             "geom_sf"))
        # test 3
        # that learner used fill to shapeleng
        .Q_gg_chor_test3 <- any(stringr::str_detect(capture.output(Q_gg_chor$layers), "fill = ~type"))
        
        if (isTRUE(!(.Q_gg_chor_test1 && .Q_gg_chor_test2))) return(c(value = 0, message = "! Do not forget to use ggplot2 geometry function for spatial data objects."))
        if (isTRUE(!(.Q_gg_chor_test1 && .Q_gg_chor_test2 && .Q_gg_chor_test3))) return(c(value = 0, message = "! Do not forget to add the variable 'type' inside a mapping argument."))
        if (isTRUE(.Q_gg_chor_test1 && .Q_gg_chor_test2 && .Q_gg_chor_test3)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_Q_gg_chor <- function(){
  'HINT: 
  First, identify the Vector data type of the spatial data object. 
  Then decide if you need the "fill" or "color" argument inside the aes() function' -> out
  cat(out)
}
# solution of question
.SOLUTION_Q_gg_chor <- function(){
  'SOLUTION: 
    ggplot(data = afrilearndata::afriairports) + 
    geom_sf(aes(fill = type))' -> out
  cat(out)
}

