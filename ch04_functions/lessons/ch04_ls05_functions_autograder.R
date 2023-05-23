# Select Autograder (Template)
## Kene David Nwosu
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

print.answer <- function(x) cat(x) # set print behavior for answers


## ~~~~ New functions ----

.pass <- function(message = paste0("Correct! ", praise())) {
  .autograder_value <<- 1
  .autograder_message <<- message
  rlang::eval_bare(rlang::expr(return()), env = parent.frame())
}

.fail <- function(message = "Wrong. Please try again") {
  .autograder_value <<- 0
  .autograder_message <<- message
  rlang::eval_bare(rlang::expr(return()), env = parent.frame()) 
}

.na <- function(message = "Invalid answer. Please check your work.") {
  .autograder_value <<- -1
  .autograder_message <<- message
  rlang::eval_bare(rlang::expr(return()), env = parent.frame())
}

.apply_autograder <-  function () {
  tryCatch({.autograder()}, 
           # if there is an error, return NA score and message.
           error = function(e) {
             .autograder_value <<- -1
             .autograder_message <<- "Invalid answer. Please check your work"})
  .scores[.problem_number] <<- .autograder_value
  cat(.autograder_message)
  .score_grid()
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 1)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1 <- "D" # code answer here
    
    .autograder <<- 
      function(){
        if ( (q1 == "YOUR ANSWER SHOULD BE HERE") ) .na("Did you forget to answer the question?")
        if (!is.character(q1)) .na("Your answer should be a string.")
        if ( stringr::str_detect(q1, .q1)) .pass()
        else .fail()
      }
    .apply_autograder()
  }

.hint_q1 <- function(){ 
"Look at the help of the `cut()` function." %>% cat() }
