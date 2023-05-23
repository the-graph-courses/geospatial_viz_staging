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

.NUM_Q_age_category <- 1
.NUM_Q_14th_column <- 2
.NUM_Q_select_package <- 3

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q_age_category ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_age_category <-
  function() {
    .problem_number <<- .NUM_Q_age_category
    
    .Q_age_category <- .yaounde %>% select(age_category) # code answer here
    .Q_age_category_mistake <- .yaounde %>% select(age) # code common mistake here
    
    .autograder <<- 
      function(){
        if (!is.data.frame(Q_age_category)) .na("Invalid answer. Your answer should be a data frame.")
        if (isTRUE(all_equal(Q_age_category, .Q_age_category))) .pass()
        if (isTRUE(all_equal(Q_age_category, .Q_age_category_mistake))) .fail("Wrong. You selected the `age` column, not the `age_category` column.")
        else .fail()
      }
    .run_autograder()
  }

.HINT_Q_age_category <- function(){ 
"
Hint: 
YOURDATA %>% select(COLUMN_NAME)
" %>% cat()
}

.SOLUTION_Q_age_category <- function(){
"
Solution:
yaounde %>% select(age) 
" %>%  cat()
}

# tests
# q1 <- .yaounde %>% select(sequelae)
# .CHECK_q1()
# q1 <- .yaounde %>% select(age_category)
# .CHECK_q1()
# q1 <- .yaounde %>% select(age)
# .CHECK_q1()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q_14th_column ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_14th_column <-
  function() {
    
    .problem_number <<- .NUM_Q_14th_column
    
    .Q_14th_column <- .yaounde %>% select(14)
    
    .autograder <<- 
      function(){
        if (!is.data.frame(Q_14th_column)) .na("Your answer should be a dataframe.")
        if (isTRUE(all_equal(Q_14th_column, .Q_14th_column))) .pass()
        else .fail()
      }
    .run_autograder()
  }

.HINT_Q_14th_column <- function(){ 
"
Hint: YOURDATA %>% select(COL_POSITION)
" %>% cat()
  }

.SOLUTION_Q_14th_column <- function(){
"
Solution:
yaounde %>% select(14)
" %>%  cat()
}

# tests
# Q_14th_column <- .yaounde %>% select(13) 
# .CHECK_Q_14th_column()
# Q_14th_column <- .yaounde %>% select(14)
# .CHECK_Q_14th_column()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q_select_package ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_select_package <-
  function() {
    
    .problem_number <<- .NUM_Q_select_package
    
    .Q_select_package <- "B"
    .Q_select_package_mistake1 <- "A" # optional: highlight mistake
    
    .autograder <<-
      function(){
        
        if (!is.character(Q_select_package) || length(Q_select_package) > 1 || nchar(Q_select_package) > 1) 
          .na("Your answer should be a single letter, in quotes.")
        if (toupper(Q_select_package) == .Q_select_package)
          .pass()
        if (toupper(Q_select_package) == .Q_select_package_mistake1)
          .fail("Wrong. You are working in an {Rmarkdown} script. But the `select()` function comes from a different package.")
        else 
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_select_package <- function() {
  "Hint: You can type '?select' to find out" %>% cat()
}

.SOLUTION_Q_select_package <- function(){
  "`select()` is in the {dplyr} package" %>% cat()
}

# tests
# Q_select_package <- 5
# .CHECK_Q_select_package()
# 
# Q_select_package <- "C"
# .CHECK_Q_select_package()
# 
# Q_select_package <- "D"
# .CHECK_Q_select_package()
# 
# Q_select_package <- "A"
# .CHECK_Q_select_package()


