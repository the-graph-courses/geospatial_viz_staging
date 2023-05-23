# UTM Autograder
## Andree Valle Campos
## 2022-05-25

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(tidyverse,
               praise,
               here,
               dplyr)

pacman::p_load_gh("afrimapr/afrilearndata")
pacman::p_load(ggplot2)
pacman::p_load(sf)


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
    
    .q1_correct <- "projected_crs" # write correct answer
    .q1_mistake1 <- "geographic_crs" # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!is.character(q1) || grepl(pattern = " ",x = q1)) 
          return(c(value = -1, message = "! You should paste the whole text of only one of the options."))
        if (isTRUE(all.equal(q1, .q1_mistake1))) return(c(value = 0, message = "! The magnitude of the coordinate values do not seem to be inside the longitude or latitude range of values."))
        if (isTRUE(all.equal(q1, .q1_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q1 <- function(){
  'Compare the magnitude of the coordinates with the axis values in Figure 7.' -> out
  cat(out)
}
# solution of question
.solution_q1 <- function(){
  'projected_crs' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q1 <- "YOUR ANSWER HERE"
# 
# .check_q1()
# .hint_q1()

# # # [backend]
# # # test the check function
# q1 <- "geographic_crs"
# .check_q1()
# q1 <- "projected_crs"
# .check_q1()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- "geographic_crs" # write correct answer
    .q2_mistake1 <- "projected_crs" # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!is.character(q2) || grepl(pattern = " ",x = q2)) 
          return(c(value = -1, message = "! You should paste the whole text of only one of the options."))
        if (isTRUE(all.equal(q2, .q2_mistake1))) return(c(value = 0, message = "! This magnitude of the coordinate values may lay inside the Atlantic ocean."))
        if (isTRUE(all.equal(q2, .q2_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q2 <- function(){
  'Compare the magnitude of the coordinates with the axis values in Figure 2.' -> out
  cat(out)
}
# solution of question
.solution_q2 <- function(){
  'geographic_crs' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q2 <- "YOUR ANSWER HERE"
# 
# .check_q2()
# .hint_q2()
# 
# # # [backend]
# # # test the check function
# q2 <- "projected_crs"
# .check_q2()
# q2 <- "geographic_crs"
# .check_q2()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# pacman::p_load(geoR)
parana <- rio::import("https://github.com/cran/geoR/raw/master/data/parana.rda")
.shape_file3 <- as_tibble(parana$coords) %>% 
  mutate(Rainfall = parana$data)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- .shape_file3 %>% st_as_sf(coords = c("east", "north"), crs = st_crs("+proj=utm +zone=22")) # write correct answer
    .q3_mistake1 <- .shape_file3 %>% st_as_sf(coords = c("east", "north"), crs = st_crs("+proj=utm +zone=28")) # optional: highlight common mistake
    .q3_mistake2 <- .shape_file3 %>% st_as_sf(coords = c("north", "east"), crs = st_crs("+proj=utm +zone=22")) # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q3)[1],"sf"))) return(c(value = -1, message = "Your result should be an sf object."))
        if (isTRUE(all.equal(q3, .q3_mistake1))) return(c(value = 0, message = "! Parana state in Brazil is inside the UTM zone 28."))
        if (isTRUE(all.equal(q3, .q3_mistake2))) return(c(value = 0, message = "! Longitude needs to go first, and Latitude second."))
        if (isTRUE(all.equal(q3, .q3_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q3 <- function(){
  'You need to find the UTM zone number for Parana state in Brazil' -> out
  cat(out)
}
# solution of question
.solution_q3 <- function(){
  'parana_data %>% 
     st_as_sf(coords = c("east", "north"), 
              crs = st_crs("+proj=utm +zone=22"))' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q3 <- "YOUR ANSWER HERE"
# .check_q3()
# .hint_q3()

# # [backend]
# # test the check function
# q3 <- .shape_file3 %>% st_as_sf(coords = c("east", "north"), crs = st_crs("+proj=utm +zone=28"))
# .check_q3()
# q3 <- .shape_file3 %>% st_as_sf(coords = c("north", "east"), crs = st_crs("+proj=utm +zone=22"))
# .check_q3()
# q3 <- .shape_file3 %>% st_as_sf(coords = c("east", "north"), crs = st_crs("+proj=utm +zone=22"))
# .check_q3()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.shape_file4 <- as_tibble(parana$coords) %>% 
  mutate(Rainfall = parana$data) %>% 
  st_as_sf(coords = c("east", "north"), 
           crs = st_crs("+proj=utm +zone=22"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- .shape_file4 %>% st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) # write correct answer
    .q4_mistake1 <- .shape_file4 %>% st_transform() # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q4)[1],"sf"))) return(c(value = -1, message = "Your result should be a sf object."))
        if (isTRUE(all.equal(q4, .q4_mistake1))) return(c(value = 0, message = "! You need to specify the PROJ string with the projection and datum."))
        if (isTRUE(all.equal(q4, .q4_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q4 <- function(){
  'The st_transform() function allows you to change CRS projections' -> out
  cat(out)
}
# solution of question
.solution_q4 <- function(){
  'q6 %>% 
     st_transform(crs = st_crs("+proj=longlat +datum=WGS84"))' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q4 <- "YOUR ANSWER HERE"
# .check_q4()
# .hint_q4()

# # [backend]
# # test the check function
# q4 <- .shape_file4 %>% st_transform()
# .check_q4()
# q4 <- .shape_file4 %>% st_transform(crs = st_crs("+proj=longlat +datum=WGS84"))
# .check_q4()

