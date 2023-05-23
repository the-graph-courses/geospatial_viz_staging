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

.yaounde <- read_csv(here('ch04_functions/data/yaounde_data.csv'))

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

.q1 <- function(weight, height) { 
                        x <- weight / (height ^ 2) 
                        return(x) }  

.weight = .yaounde$weight_kg 
.height = .yaounde$height_cm/100

.check_q1 <-
  function() {
    .problem_number <<- 1
    .autograder <<- 
      function(){
        if (is.character(q1)) 
          .na("Your answer should be a function, not a string. Did you forget to
              answer the question?")
        if (!is.function(q1)) .na("Your answer should be a function.")
        if (  q1(weight = 70.00, height = 1.70) == .q1(weight = 70.00, height = 1.70) )  .pass()
        else .fail()
      }
    .apply_autograder()
  }

.hint_q1 <- function(){ 
"Remember the structure of the function in this case: 

age_months <- function(weight, height){
              result <- compute the BMI with the 'weight' and 'height'
              return (result)
}" %>% cat() }

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

    .weight = .yaounde$weight_kg 
    .height = .yaounde$height_cm/100
    
    .q2 <- .q1(weight = .weight,height = .height)
    
    .autograder <<- 
      function(){
        if (is.character(q2)) 
          .na("Your answer should be a vector, not a string. Did you forget to
              answer the question?")
        if (!is.vector(q2)) .na("Your answer should be a vector.")
        if ( setequal(q2, .q2) )  .pass()
        else .fail()
      }
    .apply_autograder()
  }

.hint_q2 <- function() {
  "Pay attention if you are submitting columns of the dataframe in the parameters of the function. 
  Remember that you can select a specific column using 'yaounde${column_name}'" %>% cat()
  
}


# tests
# q2 <- .yaounde %>% select(13) 
# .check_q2()
# q2 <- .yaounde %>% select(14)
# .check_q2()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.age_range = c(5,15,30,45,65, Inf)
.age_name = c('5 - 14', '15 - 29', '30 - 44', '45 - 64', '65 +')

.q3 <- function(data, age_column, age_range, age_name){
  
  age_groups <- cut(data %>% pull(age_column), 
                    breaks = age_range, 
                    labels = age_name, include.lowest = T, right = F)
  
  data['age_group'] = age_groups
  
  return (data)
}

.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .autograder <<-
      function(){
        if (is.character(q3)) 
          .na("Your answer should be a function, not a string. Did you forget to
              answer the question?")
        if (!is.function(q3)) .na("Your answer should be a function.")
        if ( all.equal(q3(.yaounde, 'age', .age_range, .age_name), .q3(.yaounde, 'age', .age_range, .age_name) ) )  .pass()
        else .fail()
      }

    .apply_autograder()
    }

.hint_q3 <- function() {
  "Take a look at the `BMI` function created above.
  Look at the help of the 'cut' function and change the parameters 'include.lowest' and 'right'
  of the 'cut' function." %>% cat()
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



