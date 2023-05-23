# Select Autograder (Template)
## GRAPH Courses Team
## 2022-04-05

#' Autograder for the template script

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Packages and settings ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(tidyverse,
               praise,
               here)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 2)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1 <- function(x) x + 2
  
    .autograder <<-
      function(){
        if(!exists("q1")) 
          .na("You have not yet defined the answer.")
        if (!is.function(q1)) 
          .na(message = "Your answer should be a function.")
        
        if (isTRUE(all.equal(.q1(2), 
                             q1(2))) &&
            isTRUE(all.equal(.q1(1:10), 
                             .q1(1:10))))
          .pass()
        else 
          .fail()
      }
    .run_autograder()
  }

# [backend]
# create one hint per question
.hint_q1 <- function(){
'function(x){
 # code to add 2 to x in here
}' -> out
cat(out)
}
# solution of question
.solution_q1 <- function(){
'function(x){
  x + 2
}' -> out
cat(out)
}

# [backend]
# test solution
# .solution_q1()
# # test the check function
# q1 <- "YOUR ANSWER HERE"
# .check_q1()
# q1 <- function(x) x
# .check_q1()
# q1 <- function(x) x + 2
# .check_q1()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.check_q2 <-
  function() {
    .problem_number <<- 2

    .autograder <<-
      function(){
        if(!exists("q2")) 
          .na("You have not yet defined the answer.")
        if (!is.function(q2)) 
          .na(message = "Your answer should be a function.")
        if (is.null(q2(35)) && 
            str_extract(tolower(q2(38)), "[[:alpha:]]+") == "fever" && 
            str_extract(tolower(q2(42)), "[[:alpha:]]+") == "fever")
          .pass()
        else 
          .fail()
      }
    .run_autograder()
  }

# create one hint per question
.hint_q2 <- function(){
  'function(x){
  if(CONDITION_HERE) "STRING_OUTPUT_HERE"
}' -> out
  cat(out)
}
# solution of question
.solution_q2 <- function(){
'function(x){
  if(x >= 38) "Fever"
}' -> out
  cat(out)
}

# # tests
# .hint_q2()
# .solution_q2()
# # test the check function
# q2 <- "YOUR ANSWER HERE"
# .check_q2()
# q2 <- function(x) x
# .check_q2()
# q2 <- function(x) if(x >= 38) "Fever"
# .check_q2()




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q3 <-
  function() {
    .problem_number <<- 3
    
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
    
    .q3 <- .df2_means
    
    .autograder <<-
      function(){
        if(!exists("q3")) 
          .na("You have not yet defined the answer. Please assign the answer to the object q3.")
        if (!is.vector(q3)) 
          .na(message = "Your answer should be a vector.")
        if (isTRUE(all.equal(q3, .q3)))
          .pass()
        else 
          .fail()
      }
    .run_autograder()
  }


.hint_q3 <- function(){
  'Inside your for loop, you should iterate over `.df2_means` with `.df2_means[[i]]` 
  and iterate over `.df2` with `.df2[[i]]`' -> out
  cat(out)
}

.solution_q3 <- function(x){
  ' .df2_means <- vector()
    
    for (i in 1:ncol(df2)) {
      .df2_means[[i]] <- mean(.df2[[i]])
    }
    
    .q3 <- .df2_means '-> out
  
  cat(out)
}


# Test hint and solution
# .hint_q3()
# .solution_q3()
# 
# # Test NULL
# rm(q3)
# .check_q3()
# 
# # Test incorrect
# q3 <- 1:10
# .check_q3()
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
# q3 <- df2_means
# .check_q3()


