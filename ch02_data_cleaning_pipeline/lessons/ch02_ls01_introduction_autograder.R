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

typhoid <- read_csv(here::here('ch02_data_cleaning_pipeline/data/typhoid_uganda.csv'))
  
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 2)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- skim(typhoid) # write correct answer
#    .q1_mistake1 <- skimr(typhoid) # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q1)[1],"skim_df"))) return(c(value = -1, message = "Your result should be a skim dataframe."))
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
  'Use the skim() function' -> out
  cat(out)
}
# solution of question
.solution_q1 <- function(){
  'skim(typhoid)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- skim(typhoid,Age,Levelofeducation) # write correct answer
#    .q2_mistake1 <- skimr(typhoid) # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q2)[1],"skim_df"))) return(c(value = -1, message = "Your result should be a skim dataframe."))
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
  'Use the skim() function and specify the variables' -> out
  cat(out)
}
# solution of question
.solution_q2 <- function(){
  'skim(typhoid,Age,Levelofeducation)' -> out
  cat(out)
}

