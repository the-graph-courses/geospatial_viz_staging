# 00_Dates_times Autograder
## Ama Owusu-Darko
## 2022-05-10

#' <To grade quizzes in 00_dates_times.Rmd>

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
    
    .q1a_correct <- clock::date_build(2021,12,27) # write correct answer
    
    .q1_mistake1 <- c(clock::date_build(2021,1:11,27)) # wrong month
    
    .q1_mistake2 <- c(clock::date_build(-32767:2020,12,27),
                      clock::date_build(2022:32767,12,27))# wrong year
    .q1_mistake3 <- c(clock::date_build(2021,12,1:26), clock::date_build(2021,12,28:31))  # 1st- 31st December, 2021
    #without 27th December, 2021
    
    
    .autograder <<-
      function(){
        if (!lubridate::is.Date(q1)) 
          return(c(value = -1, message = "Your result should be a date object."))
        if (isTRUE(all.equal(q1, .q1_mistake1)))
          return(c(value = 0, message = paste("You have the day 27 right and the year right 2021 but the wrong month.
                                              Your code outputs", q1, 
                                              "which says the month is", clock::date_month_factor(q1), 
                                              "Hint December is the 12th month")))
        if (q1 %in% q1_mistake2) 
          return(c(value = 0, message = paste("You have the day 27 right and the month December right
                                              but the wrong year. Your code outputs", q1, 
                                              "which says the year is", clock::get_year(q1),
                                              "instead of 2021")))
        if (q1 %in% q1_mistake3) 
          return(c(value = 0, message = paste("You have the year 2021 right and the month December right
                                              but the wrong day. Your code outputs", q1, 
                                              "which says the day is", clock::get_day(q1), "instead of 27" )))
        
        if (isTRUE(all.equal(q1, .q1a_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        
        # wrong
        return(c(value = 0, message = "Wrong. Please try again.
                 Remember that the year is 2021 and the acceptable range for days is 1-31 
                 and range for months is 1-12"))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q1a <- function(){
  'The baby boy was born on 27th December, 2021' -> out
  cat(out)
}

.hint_q1b <- function(){
  'Remember that there acceptable range for days is 1-31 and range for months is 1-12' -> out
  cat(out)
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2aa_correct <- clock::date_time_build(2021,12,28,15:17,00,00, zone= "Africa/Nairobi") # write correct answer
    .q2bb_correct <- clock::date_time_build(2021,12,28,15:17,00, zone= "Africa/Nairobi")
    .q2c_correct <- clock::date_time_build(2021,12,28,15:17, zone= "Africa/Nairobi")
    
    .q2a_correct <- clock::date_time_build(2021,12,28,15:17,0,0, zone= "Africa/Nairobi") # write correct answer
    .q2b_correct <- clock::date_time_build(2021,12,28,15:17,0, zone= "Africa/Nairobi")
    
    .q2d_correct <- clock::date_time_build(2021,12,28,15:17,00, 0, zone= "Africa/Nairobi")
    .q2e_correct <- clock::date_time_build(2021,12,28,15:17,0, 00, zone= "Africa/Nairobi")
    
    
    .q2_mistake1 <- clock::date_time_build(2021,12,28,15,00,00, zone= "Africa/Nairobi") #wrong time
    
    .q2_mistake2 <- clock::date_time_build(2021,1:11,28,15,00,00, zone= "Africa/Nairobi")# wrong month
    .q2_mistake3 <- c(clock::date_time_build(2021,12,1:27,15,00:00,00, zone= "Africa/Nairobi"),
      clock::date_time_build(2021,12,29:31,15,00:00,00, zone= "Africa/Nairobi"))# 1st-31st December, 2021 
    #without 28th December, 2021
    .q2_mistake4 <- c(clock::date_time_build(-32767:2020,12,27,15,00:00,00, zone= "Africa/Nairobi"),
                      clock::date_time_build(2022:32767,12,27,15,00:00,00, zone= "Africa/Nairobi"))#wrong year
    
    
    .autograder <<-
      function(){
        if (!DescTools::IsDate(q2)) 
          return(c(value = -1, message = "Your result should be a date object with 7 arguments
                   (year, month, day, hour, minute, second, zone=timezone)"))
        
        if (q2 %in% q2_mistake1) 
          return(c(value = 0, message = paste("You have the day 28 right, the month December right and the year 2021 
                                              but you have the hour argument wrong . Your code outputs", q2, 
                                              "which outputs only one hour", clock::get_hour(q1), ":",
                                              clock::get_minute(q1), ":", clock::get_second(q1),
                                              
                                              "You need to type the interval 15:17 in the hour argument 
                                              to output three date-time objects for each of the three girls")))
        if (q2 %in% q2_mistake2) 
          return(c(value = 0, message = paste("You have the day 28 right and the year 2021 right but the wrong month. 
                                              Your code outputs", q2, 
                                              "which says the month is", clock::date_month_factor(q2),
                                              "instead of December" )))
        if (q2 %in% q2_mistake3) 
          return(c(value = 0, message = paste("You have the month December right and the 2021 year right 
                                              but you have the wrong day. 
                                              Your code outputs", q2, 
                                              "which says the day is", clock::get_day(q2), "instead of 28." )))
        if (q2 %in% q2_mistake4) 
          return(c(value = 0, message = paste("You have the day 27 right and the month December right
                                              but the wrong year. Your code outputs", q1, 
                                              "which says the year is", clock::get_year(q1),
                                              "instead of 2021")))
        
        if (isTRUE(all.equal(q2, .q2a_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        if (isTRUE(all.equal(q2, .q2b_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        if (isTRUE(all.equal(q2, .q2c_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        
        if (isTRUE(all.equal(q2, .q2d_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        if (isTRUE(all.equal(q2, .q2e_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        
        if (isTRUE(all.equal(q2, .q2aa_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        if (isTRUE(all.equal(q2, .q2bb_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        
        
        # wrong
        return(c(value = 0, message = "Wrong. Please try again.
                 Remember that the year is 2021 and the acceptable range for days is 1-31 
                 and range for months is 1-12, the range for hours is 0-23 and 
                 0-59 is the range for both minutes and seconds"))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q2a <- function(){
  "Use the function clock::date_time_build(year, month, day, hour, minute, second, zone=timezone)
  and fill in the 7 arguments" -> out
  cat(out)
}

.hint_q2b <- function(){
  'The three baby girls were born  in "Africa/Nairobi" on 28th December, 2021 at 15:00:00, 16:00:00, 17:00:00 respectively' -> out
  cat(out)
}

.hint_q2c <- function(){
  'The baby girls were born in Nairobi so the argument zone will be, zone = "Africa/Nairobi"' -> out
  cat(out)
}

.hint_q2d <- function(){
  'The baby girls were born on 28th December, 2021' -> out
  cat(out)
}

.hint_q2e <- function(){
      'Remember that the year is 2021 and the acceptable range for days is 1-31 
      and range for months is 1-12, the range for hours is 0-23 and 
      0-59 is the range for both minutes and seconds"' -> out
  cat(out)
}


.hint_q2f <- function(){
    'You have to create three date-time objects, one for each of the three girls.
    You need to type the interval 15:17 in the hour argument.
    
    Hint
    
    `clock::date_time_build(year, month, day, 15:17`,00,00, zone= "Africa/Nairobi'-> out
    cat(out)
}                                          
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## test ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


