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

typhoid <- read_csv(here::here('ch02_data_cleaning_pipeline/data/typhoid_deduped.csv'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 8)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- typhoid %>% count(householdmembers, name = "Count")
    # write correct answer
#    .q1_mistake1 <- count(householdmembers, name = "Count") # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q1)[1],"spec_tbl_df"))) return(c(value = -1, message = "Your result should be a dataframe."))
 #       if (isTRUE(all.equal(q1, .q1_mistake1))) return(c(value = 0, message = "It's ok to copy and paste code, remember to change the argument."))
        if (isTRUE(all.equal(q1, .q1_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again. It's ok to copy and paste code, remember to change the argument."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q1 <- function(){
  'Use the count() function. Remember to use the pipe operator (%>%)' -> out
  cat(out)
}
# solution of question
.solution_q1 <- function(){
  'typhiod %>% count(householdmembers, name = "Count")' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- typhoid %>% mutate(householdmembers = recode(householdmembers,`01-May` = "1-5")) %>%
      
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
  'Use the recode() function inside mutate().' -> out
  cat(out)
}
# solution of question
.solution_q2 <- function(){
  'typhoid %>% mutate(householdmembers = recode(householdmembers,`01-May` = "1-5"))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- typhoid %>% mutate(positioninthehousehold = replace(positioninthehousehold, unique_key == 75 , "Household head"))
#    .q3_mistake1 <- typhoid %>% mutate(positioninthehousehold = replace(positioninthehousehold, "Household head", unique_key == 75 ))  # optional: highlight common mistake
    
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
  'Use the replace() function inside mutate().' -> out
  cat(out)
}
# solution of question
.solution_q3 <- function(){
  'typhoid %>% mutate(positioninthehousehold = replace(positioninthehousehold, unique_key == 75 , "Household head"))' -> out
  cat(out)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- typhoid %>% mutate(county = gsub("1","",county))

#    .q4_mistake1 <- typhoid %>% mutate(county = gsub(county,"1","")) # optional: highlight common mistake
    
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
  'The syntax should be YOURDATA %>% mutate(yourvariable = gsub(pattern,replacement,yourvariable))' -> out
  cat(out)
}
# solution of question
.solution_q4 <- function(){
  'typhoid %>% 
  mutate(county = gsub("1","",county))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q5 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q5 <-
  function() {
    .problem_number <<- 5
    
    .q5_correct <- typhoid %>%   mutate(across(where(is.character),
                                                ~ tolower(.x)))
    
    
#    .q5_mistake1 <- typhoid %>% mutate(across(where(character),
#                                               ~ tolower(.x))) # optional: highlight common mistake
    
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
  'Use the function across inside the mutate function' -> out
  cat(out)
}
# solution of question
.solution_q5 <- function(){
  'typhoid %>% 
  mutate(across(where(is.character),~ tolower(.x)))' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q6 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q6 <-
  function() {
    .problem_number <<- 6
    
    .q6_correct <- typhoid %>% mutate(across(c(13:29),
                                                ~ as.factor(.x)))
    
    
#    .q6_mistake1 <- typhoid %>% mutate(across(c(13:29),
#                                               ~ factor(.x))) # optional: highlight common mistake
    
    # write correct answer
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q6)[1],"tbl_df"))) return(c(value = -1, message = "Your result should be a dataframe."))
#        if (isTRUE(all.equal(q6, .q6_mistake1))) return(c(value = 0, message = "It's ok to copy and paste code, remember to change the argument."))
        if (isTRUE(all.equal(q6, .q6_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again. It's ok to copy and paste code, remember to change the argument."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q6 <- function(){
  'The syntax should be YOURDATA %>% distinct(variables)' -> out
  cat(out)
}
# solution of question
.solution_q6 <- function(){
  'typhoid %>% 
 mutate(across(c(13:29),~ as.factor(.x)))' -> out
  cat(out)
}

