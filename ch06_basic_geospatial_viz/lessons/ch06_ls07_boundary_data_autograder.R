# External data Autograder
## Andree Valle Campos
## 2022-05-24

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(tidyverse,
               praise,
               here)
pacman::p_load_gh("wmgeolab/rgeoboundaries")
pacman::p_load(malariaAtlas)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 4)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- geoboundaries(country = "Sierra Leone") # write correct answer
    .q1_mistake1 <- geoboundaries(country = "Zimbabwe") # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q1)[1],"sf"))) return(c(value = -1, message = "Your result should be sf and data frame class."))
        if (isTRUE(all.equal(q1, .q1_mistake1))) return(c(value = 0, message = "It's ok to copy and paste code, remember to change the argument."))
        if (isTRUE(all.equal(q1, .q1_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q1 <- function(){
'Use the geoboundaries() function' -> out
cat(out)
}
# solution of question
.solution_q1 <- function(){
  'geoboundaries(country = "Sierra Leone")' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q1 <- "YOUR ANSWER HERE"
# .check_q1()
# .hint_q1()

# # [backend]
# # test the check function
# q1 <- geoboundaries(country = "Zimbabwe")
# .check_q1()
# q1 <- geoboundaries(country = "Sierra Leone")
# .check_q1()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- geoboundaries(country = "Sierra Leone", adm_lvl = 3) # write correct answer
    .q2_mistake1 <- geoboundaries(country = "Sierra Leone") # optional: highlight common mistake
    .q2_mistake2 <- geoboundaries(country = "Zimbabwe", adm_lvl = 2)
    
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q2)[1],"sf"))) return(c(value = -1, message = "Your result should be an sf and data frame object."))
        if (isTRUE(all.equal(q2, .q2_mistake1))) return(c(value = 0, message = "You need to specify the administrative level that you want."))
        if (isTRUE(all.equal(q2, .q2_mistake2))) return(c(value = 0, message = "It's ok to copy and paste code, remember to change the argument."))
        if (isTRUE(all.equal(q2, .q2_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q2 <- function(){
'You need to use the `adm_lvl` argument.' -> out
cat(out)
}
# solution of question
.solution_q2 <- function(){
  'geoboundaries(country = "Sierra Leone", adm_lvl = 3)' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q2 <- "YOUR ANSWER HERE"
# .check_q2()
# .hint_q2()

# # [backend]
# # test the check function
# q2 <- geoboundaries(country = "Sierra Leone")
# .check_q2()
# q2 <- geoboundaries(country = "Sierra Leone", adm_lvl = 3)
# .check_q2()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- getPR(country = "Sierra Leone",species = "Pf") # write correct answer
    .q3_mistake1 <- getPR(country = "Sierra Leone",species = "Pv") # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!is.data.frame(q3)) return(c(value = -1, message = "Your result should be a data frame object."))
        if (isTRUE(all.equal(q3, .q3_mistake1))) return(c(value = 0, message = "We did not request Plasmodium vivax."))
        if (isTRUE(all.equal(q3, .q3_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q3 <- function(){
'Use the getPR function to get the Parasite Rate points' -> out
cat(out)
}
# solution of question
.solution_q3 <- function(){
  'getPR(country = "Sierra Leone",species = "Pf")' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q3 <- "YOUR ANSWER HERE"
# .check_q3()
# .hint_q3()

# # [backend]
# # test the check function
# q3 <- getPR(country = "Sierra Leone",species = "Pv")
# .check_q3()
# q3 <- getPR(country = "Sierra Leone",species = "Pf")
# .check_q3()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pacman::p_load(malariaAtlas)
.shape_file4 <- getPR(country = "Sierra Leone",species = "Pf")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- .shape_file4 %>% filter(!is.na(longitude)) %>% sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) # write correct answer
    .q4_mistake1 <- .shape_file4 %>% filter(!is.na(longitude)) %>% sf::st_as_sf(coords = c("longitude", "latitude")) # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q4)[1],"sf"))) return(c(value = -1, message = "Your result should be a sf object."))
        if (isTRUE(all.equal(q4, .q4_mistake1))) return(c(value = 0, message = "! You need to add the CRS code."))
        if (isTRUE(all.equal(q4, .q4_correct))) return(c(value = 1, message = paste("Correct!", praise::praise(), "Note: Here we needed to drop the rows with missing values in coordinates!") ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q4 <- function(){
'Do not forget to: put longitude first.' -> out
cat(out)
}
# solution of question
.solution_q4 <- function(){
  'q3 %>% filter(!is.na(longitude)) %>% sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q4 <- "YOUR ANSWER HERE"
# .check_q4()
# .hint_q4()

# # [backend]
# # test the check function
# q4 <- .shape_file4 %>% filter(!is.na(longitude)) %>% sf::st_as_sf(coords = c("longitude", "latitude"))
# .check_q4()
# q4 <- .shape_file4 %>% filter(!is.na(longitude)) %>% sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# .check_q4()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## asia africa ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.solution_asia_africa <- function(){
  'ne_countries(returnclass = "sf", 
                continent =  = c("asia", "africa"))' -> out
  cat(out)
}

# .q_check_asia_africa()
# .q_hint_asia_africa() 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## china indonesia ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.solution_china_indonesia <- function(){
  'ne_countries(returnclass = "sf",
                country = c("china", "indonesia"))' -> out
  cat(out)
}

# .q_check_china_indonesia()
# .q_hint_china_indonesia() 
