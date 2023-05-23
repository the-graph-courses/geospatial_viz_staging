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

.yaounde <- read_csv(here("ch04_functions/data/yaounde_data.csv"))

.yao <-
  .yaounde %>% select(age,
                      sex,
                      highest_education,
                      occupation,
                      is_smoker,
                      is_pregnant,
                      igg_result,
                      igm_result)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 3)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1 <- "A" # code answer here
    
    .autograder <<- 
      function(){
        if ( (q1 == "YOUR ANSWER SHOULD BE HERE") ) .na("Did you forget to answer the question?")
        if (!is.character(q1)) .na("Your answer should be a string.")
        if ( stringr::str_detect(q1, .q1)) .pass()
        else .fail()
      }
    .apply_autograder()
  }

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.q2 <- function(x) { months <- x * 12
return(months) } 

.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .autograder <<- 
      function(){
        if (is.character(q2)) 
          .na("Your answer should be a function, not a string. Did you forget to
              answer the question?")
        if (!is.function(q2)) .na("Your answer should be a function.")
        if (q2(10) == .q2(10))  .pass()
        else .fail()
      }
    .apply_autograder()
  }

.hint_q2 <- function(){ 
"Remember the structure of an function: 
age_months <- function(argument){
              result <- operation with the 'argument' 
              return (result)
}" %>% cat() }


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3 <- .q2(.yaounde$age)
    #.q3_mistake1 <- # optional: highlight mistake
    
    .autograder <<-
      function(){
        if (is.character(q3)) 
          .na("Your answer should be a vector, not a string. Did you forget to
              answer the question?")
        if (!is.vector(q3)) 
          .na("Your answer should be a vector")
        if ( setequal(q3, .q3))
          .pass()
        else 
          .fail()
      }
    .apply_autograder()
  }
.hint_q3 <- function() {
  "Pay attention if you are submitting a column to the function. 
  Remember that you can select a specific column using 'yaounde${column_name} '" %>% cat()
}
