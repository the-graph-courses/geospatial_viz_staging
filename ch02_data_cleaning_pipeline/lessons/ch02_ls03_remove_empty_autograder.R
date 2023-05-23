##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(skimr,
               praise,
               here,
               tidyverse,
               DescTools)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  DATA ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

typhoid <- read_csv(here::here('ch02_data_cleaning_pipeline/data/typhoid_clean_names.csv'))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 3)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- inspectdf::inspect_na(typhoid)  # write correct answer
#    .q1_mistake1 <- inspect(typhoid) # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q1)[1],"tbl_df"))) return(c(value = -1, message = "Your result should be a dataframe."))
#        if (isTRUE(all.equal(q1, .q1_mistake1))) return(c(value = 0, message = "It's ok to copy and paste code, remember to change the argument."))
        if (isTRUE(all.equal(q1, .q1_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again. It's ok to copy and paste code, remember to change the argument."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q1 <- function(){
  'Use the inspect_na() function' -> out
  cat(out)
}
# solution of question
.solution_q1 <- function(){
  'inspect_na(typhoid)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- typhoid %>% remove_empty("cols") 
#    .q2_mistake1 <- typhoid %>% remove_empty() # optional: highlight common mistake
    
    # write correct answer
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q2)[1],"tbl_df"))) return(c(value = -1, message = "Your result should be a dataframe."))
#        if (isTRUE(all.equal(q2, .q2_mistake1))) return(c(value = 0, message = "It's ok to copy and paste code, remember to change the argument."))
        if (isTRUE(all.equal(q2, .q2_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again. It's ok to copy and paste code, remember to change the argument."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q2 <- function(){
  'The syntax should be YOURDATA %>% remove_empty("cols")' -> out
  cat(out)
}
# solution of question
.solution_q2 <- function(){
  'typhoid %>% remove_empty("cols")' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- typhoid %>% remove_empty(which = c("rows", "cols")) 
#    .q3_mistake1 <- typhoid %>% remove_empty()  # optional: highlight common mistake
    
    # write correct answer
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q3)[1],"tbl_df"))) return(c(value = -1, message = "Your result should be a dataframe."))
#        if (isTRUE(all.equal(q3, .q3_mistake1))) return(c(value = 0, message = "It's ok to copy and paste code, remember to change the argument."))
        if (isTRUE(all.equal(q3, .q3_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again. It's ok to copy and paste code, remember to change the argument."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q3 <- function(){
  'The syntax should be YOURDATA %>% remove_empty(which = c("rows", "cols"))' -> out
  cat(out)
}
# solution of question
.solution_q3 <- function(){
  'typhoid %>% remove_empty(which = c("rows", "cols")) ' -> out
  cat(out)
}

