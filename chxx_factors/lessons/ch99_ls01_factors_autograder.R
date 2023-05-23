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

library('readxl')
.mdataset <- read_excel('chxx_factors/data/llin.xls')


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
    
    .Age <- ordered(.mdataset$Age_RECODED, levels=c("<5", "5-9", "10-19", "20-24", "25-34", "35>"))
  
    .autograder <<- 
      function(){
        if (!is.ordered(Age)) .na()
        if (isTRUE(all.equal(levels(Age), levels(.Age)) && all.equal(Age, .Age))) .pass()
        else .fail()
      }
    .run_autograder()
  }

.hint_q1 <- function() { 
"
Age <- ordered(COLUMN_NAME, levels=c(...))
" %>% cat()
}

.solution_q1 <- function(){
"
Age <- ordered(.mdataset$Age_RECODED, levels=c('<5', '5-9', '10-19', '20-24', '25-34', '35>'))
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


