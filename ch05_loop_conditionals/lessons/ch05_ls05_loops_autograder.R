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

.flu <- read_csv(here("ch05_loop_conditionals/data/fluH7N9_china_2013.csv"))

set.seed(1)
.df2 <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rpois(10, 10),
  d = rnorm(10),
  e = rpois(10, 3),
  f = rnorm(10),
)

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
    
    .q1 = class(.flu[["case_id"]])

    .autograder <<-
      function(){
        if (q1 == "YOUR ANSWER SHOULD BE HERE") .na("Did you forget to answer the question? .")
        if (!is.character(q1)) .na("Your answer should be a string.")
        if (stringr::str_detect(q1, .q1)) .pass()
        else .fail()
      }
    .apply_autograder()
  }

.hint_q1 <- function(){ 
"Use the 'class() method'" %>% cat()
  }


# tests
# q2 <- .yaounde %>% select(13) 
# .check_q2()
# q2 <- .yaounde %>% select(14)
# .check_q2()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .flu_var_types <- vector("character", ncol(.flu))

    for (i in seq_along(.flu)) {
      .flu_var_types[[i]]    <-    class(  .flu[[i]]  )
    }

    .q2 <- .flu_var_types

    .autograder <<-
      function(){
        if (is.character(q2)){
          if (q2 == "YOUR ANSWER SHOULD BE HERE") .na("Did you forget to answer the question? .")
          }
        if (!is.vector(q2)) .na("Your answer should be a vector.")
        if (setequal(q2, .q2)) .pass()
        else .fail()
      }
    .apply_autograder()
  }

.hint_q2 <- function(){ 
"
Look at the last example of the lesson.
" %>% cat()
  }


