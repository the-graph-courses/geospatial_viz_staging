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
.scores <- rep(-1, times = 6)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- names(typhoid) # write correct answer
#    .q1_mistake1 <- name(typhoid) # optional: highlight common mistake
    
    .autograder <<-
      function(){
 #       if (isTRUE(all.equal(q1, .q1_mistake1))) return(c(value = 0, message = "It's ok to copy and paste code, remember to change the argument."))
        if (isTRUE(all.equal(q1, .q1_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q1 <- function(){
  'Use the names() function' -> out
  cat(out)
}
# solution of question
.solution_q1 <- function(){
  'names(typhoid)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- typhoid %>% clean_names() 
#    .q2_mistake1 <- typhoid %>% clean_name() # optional: highlight common mistake
    
    # write correct answer
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q2)[1],"spec_tbl_df"))) return(c(value = -1, message = "Your result should be a dataframe."))
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
  'The syntax should be YOURDATA %>% clean_names()' -> out
  cat(out)
}
# solution of question
.solution_q2 <- function(){
  'typhoid %>% clean_names()' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- typhoid %>% rename(case_control = CaseorControl, education_level = Levelofeducation) 
#    .q3_mistake1 <- typhoid %>% rename(CaseorControl = case_control,Levelofeducation = education_level) # optional: highlight common mistake
    
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
  'The syntax should be YOURDATA %>% rename(new name = old name)' -> out
  cat(out)
}
# solution of question
.solution_q3 <- function(){
  'typhoid %>% rename(case_control = CaseorControl, education_level = Levelofeducation)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- typhoid %>% rename_with(tolower) 
#    .q4_mistake1 <- typhoid %>% rename_with(lower) # optional: highlight common mistake
    
    # write correct answer
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q4)[1],"tbl_df"))) return(c(value = -1, message = "Your result should be a dataframe."))
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
  'The syntax should be YOURDATA %>% rename_with(function)' -> out
  cat(out)
}
# solution of question
.solution_q4 <- function(){
  'typhoid %>% rename_with(tolower)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q5 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q5 <-
  function() {
    .problem_number <<- 5
    
    .q5_correct <- typhoid %>% 
      clean_names() %>%
      rename_with(str_replace_all, pattern = "or_",replacement = "_") %>%
      rename_with(str_replace_all, pattern = "of",replacement = "_") %>%
      rename(num_below_10_yrs = below10years,
             num_11_19_yrs = n1119years,
             num_20_35_yrs = n2035years,
             num_36_44_yrs = n3644years,
             num_45_65_yrs = n4565years,
             num_above_65_yrs = above65years)
    
#    .q5_mistake1 <- typhoid %>% 
#      clean_names() %>%
#      rename_with(str_replace_all, pattern = "or",replacement = "_") %>%
#      rename_with(str_replace_all, pattern = "of_",replacement = "_") %>%
#      rename(num_below_10_yrs = below10years,
#             num_11_19_yrs = n1119years,
#             num_20_35_yrs = n2035years,
#             num_36_44_yrs = n3644years,
#             num_45_65_yrs = n4565years,
#             num_above_65_yrs = above65years) # optional: highlight common mistake

    # write correct answer
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q5)[1],"tbl_df"))) return(c(value = -1, message = "Your result should be a dataframe."))
#        if (isTRUE(all.equal(q5, .q5_mistake1))) return(c(value = 0, message = "It's ok to copy and paste code, remember to change the argument."))
        if (isTRUE(all.equal(q5, .q5_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again. It's ok to copy and paste code, remember to change the argument."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q5 <- function(){
  'The syntax should be YOURDATA %>% 
  clean_names() %>%
  rename_with(function,pattern,replacement) %>%
  rename(new name = old name)
  ' -> out
  cat(out)
}
# solution of question
.solution_q5 <- function(){
  'typhoid %>% 
      clean_names() %>%
      rename_with(str_replace_all, pattern = "or_",replacement = "_") %>%
      rename_with(str_replace_all, pattern = "of",replacement = "_") %>%
      rename(num_below_10_yrs = below10years,
             num_11_19_yrs = n1119years,
             num_20_35_yrs = n2035years,
             num_36_44_yrs = n3644years,
             num_45_65_yrs = n4565years,
             num_above_65_yrs = above65years)' -> out
  cat(out)
}

