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

.yaounde <- read_csv(here('ch99_template/data/yaounde_data.csv'))

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
    
    .q1 <- .yaounde %>% select(age) # code answer here
    .q1_mistake <- .yaounde %>% select(age_category) # code common mistake here
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q1)) .na("Your answer should be a data frame.")
        if (isTRUE(all_equal(q1, .q1))) .pass()
        if (isTRUE(all_equal(q1, .q1_mistake))) .fail("Wrong. You selected `age_category` column, not the `age` column.")
        else .fail()
      }
    .apply_autograder()
  }

.hint_q1 <- function(){ 
"
YOURDATA %>% 
  select(COLUMN_NAME)
" %>% cat()
}

.a1 <- function(){
"
yaounde %>% 
  select(age) 
" %>%  cat()
}

# tests
# q1 <- .yaounde %>% select(sequelae)
# .check_q1()
# q1 <- .yaounde %>% select(age_category)
# .check_q1()
# q1 <- .yaounde %>% select(age)
# .check_q1()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2 <- .yaounde %>% select(14)
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q2)) .na("Your answer should be a dataframe.")
        if (isTRUE(all_equal(q2, .q2))) .pass()
        else .fail()
      }
    .apply_autograder()
  }

.hint_q2 <- function(){ 
"YOURDATA %>% 
  select(COL_POSITION)" %>% cat()
  }

.a2 <- function(){
"
yaounde %>% 
  select(14)
" %>%  cat()
}

# tests
# q2 <- .yaounde %>% select(13) 
# .check_q2()
# q2 <- .yaounde %>% select(14)
# .check_q2()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3 <- "B"
    .q3_mistake1 <- "A" # optional: highlight mistake
    
    .autograder <<-
      function(){
        
        if (!is.character(q3) || length(q3) > 1 || nchar(q3) > 1) 
          .na("Your answer should be a single letter, in quotes.")
        if (toupper(q3) == .q3)
          .pass()
        if (toupper(q3) == .q3_mistake1)
          .fail("Wrong. You are working in an {Rmarkdown} script. But the `select()` function comes from a different package.")
        else 
          .fail()
      }
    .apply_autograder()
  }

.hint_q3 <- function() {
  "You can type '?select' to find out" %>% cat()
}

.a3 <- function(){
  "`select()` is in the {dplyr} package" %>% cat()
}

# tests
# q3 <- 5
# .check_q3()
# 
# q3 <- "C"
# .check_q3()
# 
# q3 <- "D"
# .check_q3()
# 
# q3 <- "A"
# .check_q3()


