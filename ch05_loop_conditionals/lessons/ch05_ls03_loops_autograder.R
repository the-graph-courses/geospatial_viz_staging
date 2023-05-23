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
.scores <- rep(-1, times = 1)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.df2_means = c()

for (i in 1:ncol(.df2)){
  
  .df2_means = append(.df2_means, mean(.df2[[i]]))
}

.q1 <- .df2_means

.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .autograder <<-
      function(){
        if (!is.vector(q1)) .na("Your answer should be a vector.")
        if (setequal(q1, .q1)) .pass()
        else .fail()
      }
    .apply_autograder()
  }

.hint_q1 <- function(){ 
"
Look at the last example of the lesson.
" %>% cat()
}

