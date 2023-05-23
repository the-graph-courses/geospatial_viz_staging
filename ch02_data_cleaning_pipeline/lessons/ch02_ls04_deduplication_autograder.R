##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(skimr,
               praise,
               here,
               tidyverse,
               DescTools,
               janitor)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  DATA ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

typhoid <- read_csv(here::here('ch02_data_cleaning_pipeline/data/typhoid_clean_structure.csv'))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 4)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- duplicated(typhoid) %>% head(50)
    # write correct answer
#    .q1_mistake1 <- duplicated(typhoid) # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q1)[1],"logical"))) return(c(value = -1, message = "Your result should be a vector with TRUE/FALSE."))
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
  'Use the duplicated() and the head() functions. Remember to connect the functions using the pipe operator (%>%)' -> out
  cat(out)
}
# solution of question
.solution_q1 <- function(){
  'duplicated(typhoid) %>% head(50)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- typhoid %>% get_dupes() 
#    .q2_mistake1 <- duplicated(typhoid) # optional: highlight common mistake
    
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
  'The syntax should be YOURDATA %>% get_dupes()' -> out
  cat(out)
}
# solution of question
.solution_q2 <- function(){
  'typhoid %>% get_dupes()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- typhoid %>% distinct() 
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
  'Use the distinct() function' -> out
  cat(out)
}
# solution of question
.solution_q3 <- function(){
  'typhoid %>% distinct()' -> out
  cat(out)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- typhoid %>% distinct(age,county) %>% 
      nrow()
#    .q4_mistake1 <- typhoid %>% distinct(age,county) # optional: highlight common mistake
    
    # write correct answer
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q4)[1],"integer"))) return(c(value = -1, message = "Your result should be an integer."))
#        if (isTRUE(all.equal(q4, .q4_mistake1))) return(c(value = 0, message = "It's ok to copy and paste code, remember to change the argument."))
        if (isTRUE(all.equal(q4, .q4_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again. It's ok to copy and paste code, remember to change the argument."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q4 <- function(){
  'The syntax should be YOURDATA %>% distinct(variables)' -> out
  cat(out)
}
# solution of question
.solution_q4 <- function(){
  'typhoid %>% 
  distinct(age,county) %>% 
      nrow()' -> out
  cat(out)
}

