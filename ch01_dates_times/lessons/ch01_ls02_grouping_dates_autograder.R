# 03_date_groups Autograder
## Ama Owusu-Darko
## 2022-05-23

#' <To grade quizzes in 03_date_groups.Rmd>

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(tidyverse,
               praise,
               here,
               dplyr,
               clock,
               lubridate,
               DescTools)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 2)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <-lubridate::floor_date(ymd("2021-02-14"), unit= "months") 
    # write correct answer
    
    .q1_mistake1 <- lubridate::floor_date(ymd("2021-02-14"), unit= "months")  
    # wrong year 2021
    
    .q1_mistake2 <- lubridate::floor_date(ymd("2022-03-14"), unit= "months")
    #wrong month
                      
    
    .autograder <<-
      function(){
        if (!is.character(q1) || grepl(pattern = " ",x = q1)) 
          return(c(value = -1, 
                 message = "You should paste the whole text of only one of the options."))
        if (isTRUE(all.equal(q1, .q1_mistake1)))
          return(c(value = 0, message = paste("The year is incorrect, 
                                              you should choose an option with the year 2022")))
        if (isTRUE(all.equal(q1, .q1_mistake2))) 
          return(c(value = 0, message = paste("The month is incorrect, 
                                              you should choose an option with the month 03")))
        
        if (isTRUE(all.equal(q1, .q1_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        
        # wrong
        return(c(value = 0, message = "Wrong. Please try again.
                 Remember floor_date() will round 2022-02-14 down to 
 the date of first of the February in the year 2022"))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q1 <- function(){
  'In May 2014, there were 57 Ebola cases, 243 Ebola cases in June 2014 
  and 351 Ebola cases in July 2014 ' -> out
  cat(out)
}





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
              
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- mean(total_monthly_cases$n[1:3]) # write correct answer
    # May-July 2014
    
    .q2_mistake1 <- mean(total_monthly_cases$n[2:4]) # wrong month
    #June-August 2014
    
    .q2_mistake2 <- mean(total_monthly_cases$n[3:5]) #July-September 2014
    
    
    .autograder <<-
      function(){
        if (!is.integer(q2)) 
          return(c(value = -1, message = "Your result should be an integer."))
        if (isTRUE(all.equal(q2, .q2_mistake1)))
          return(c(value = 0, message = paste("You averaged over June-August 2014")))
        if (isTRUE(all.equal(q2, .q2_mistake2))) 
          return(c(value = 0, message = paste("You averaged over June-August 2014")))
        
        if (isTRUE(all.equal(q2, .q2_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        
        # wrong
        return(c(value = 0, message = "Wrong. Please try again.
                 Remember to average the monthly totals over May, June and July 2014"))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q2 <- function(){
  'In May 2014, there were 57 Ebola cases, 243 Ebola cases in June 2014 
  and 351 Ebola cases in July 2014. Use this information to find the average 
  number of cases over the 3 months ' -> out
  cat(out)
}                          
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## test ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


