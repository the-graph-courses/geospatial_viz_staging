# CRS Autograder
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
.scores <- rep(-1, times = 7)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pacman::p_load_gh("afrimapr/afrilearndata")
.shape_file1 <- africountries

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- ggplot(data = .shape_file1) + geom_sf() + coord_sf(xlim = c(-14,-10), ylim = c(6,10), expand = TRUE) # write correct answer
    # .q1_mistake1 <- ggplot(data = .shape_file1) + geom_sf() + coord_sf(xlim = c(10,14), ylim = c(6,10), expand = TRUE) # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!is.ggplot(q1)) return(c(value = -1, message = "Your result should be a ggplot object."))
        
        # test 1
        # that database is correct
        pacman::p_load(dplyr)
        .q1_test1 <- all_equal(
          target = as_tibble(q1$data) %>% select(-geometry), 
          current = as_tibble(.q1_correct$data) %>% select(-geometry))
        
        # test 2
        # that learner used xlim
        .q1_test2 <- isTRUE(all.equal(q1$coordinates$limits$x,c(-14,-10)))
        # test 3
        # that learner used ylim
        .q1_test3 <- isTRUE(all.equal(q1$coordinates$limits$y,c(6,10)))
        
        if (isTRUE(!(.q1_test1 && .q1_test2))) return(c(value = 0, message = "! Careful with the values in xlim. They need to follow the signs of its cardinal directions."))
        if (isTRUE(!(.q1_test1 && .q1_test2 && .q1_test3))) return(c(value = 0, message = "! Careful with the values in ylim. They need to follow the signs of its cardinal directions."))
        if (isTRUE(.q1_test1 && .q1_test2 && .q1_test3)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q1 <- function(){
'You should use the xlim and ylim arguments of the coord_sf() function' -> out
cat(out)
}
# solution of question
.solution_q1 <- function(){
  'ggplot(data = africountries) + 
    geom_sf() + 
    coord_sf(xlim = c(-14,-10), ylim = c(6,10), expand = TRUE)' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q1 <- "YOUR ANSWER HERE"
# .check_q1()
# .hint_q1()

# # [backend]
# # test the check function
# q1 <- ggplot(data = .shape_file1) + geom_sf() + coord_sf(xlim = c(10,14), ylim = c(6,10), expand = TRUE)
# .check_q1()
# q1 <- ggplot(data = .shape_file1) + geom_sf() + coord_sf(xlim = c(-14,-10), ylim = c(-6,-10), expand = TRUE)
# .check_q1()
# q1 <- ggplot(data = .shape_file1) + geom_sf() + coord_sf(xlim = c(-14,-10), ylim = c(6,10), expand = TRUE)
# .check_q1()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pacman::p_load(spData)
.shape_file2 <- world

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- ggplot(data = .shape_file2) + geom_sf() + coord_sf(crs = 3857) # write correct answer
    # .q2_mistake2 <- ggplot(data = .shape_file2) + geom_sf() + coord_sf() # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!is.ggplot(q2)) return(c(value = -1, message = "Your result should be a ggplot object."))
        
        # test 2
        # that database is correct
        pacman::p_load(dplyr)
        .q2_test1 <- all_equal(
          target = as_tibble(q2$data) %>% select(-geom), 
          current = as_tibble(.q2_correct$data) %>% select(-geom))
        
        # test 2
        # that learner used crs
        .q2_test2 <- isTRUE(all.equal(q2$coordinates$crs,3857))
        
        if (isTRUE(!(.q2_test1 && .q2_test2))) return(c(value = 0, message = "! Remember that you need to specify the new EPSG code"))
        if (isTRUE(.q2_test1 && .q2_test2)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q2 <- function(){
  'You should use the crs argument of the coord_sf() function' -> out
  cat(out)
}
# solution of question
.solution_q2 <- function(){
  'ggplot(data = world) + 
    geom_sf() + 
    coord_sf(crs = 3857)' -> out
  cat(out)
}

# [frontend]
# # to paste in lesson
# q2 <- "YOUR ANSWER HERE"
# .check_q2()
# .hint_q2()

# # [backend]
# # test the check function
# q2 <- ggplot(data = .shape_file2) + geom_sf() + coord_sf()
# .check_q2()
# q2 <- ggplot(data = .shape_file2) + geom_sf() + coord_sf(crs = 3857)
# .check_q2()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pacman::p_load(spData)
.shape_file3 <- world

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- ggplot(data = .shape_file3) + geom_sf() + coord_sf(crs = "+proj=aitoff") # write correct answer
    # .q3_mistake3 <- ggplot(data = .shape_file3) + geom_sf() + coord_sf() # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!is.ggplot(q3)) return(c(value = -1, message = "Your result should be a ggplot object."))
        
        # test 3
        # that database is correct
        pacman::p_load(dplyr)
        .q3_test1 <- all_equal(
          target = as_tibble(q3$data) %>% select(-geom), 
          current = as_tibble(.q3_correct$data) %>% select(-geom))
        
        # test 2
        # that learner used crs
        .q3_test2 <- isTRUE(all.equal(q3$coordinates$crs,"+proj=aitoff"))
        
        if (isTRUE(!(.q3_test1 && .q3_test2))) return(c(value = 0, message = "! Remember that you need to specify the new PROJ string"))
        if (isTRUE(.q3_test1 && .q3_test2)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q3 <- function(){
  'You should use the crs argument of the coord_sf() function' -> out
  cat(out)
}
# solution of question
.solution_q3 <- function(){
  'ggplot(data = world) + 
    geom_sf() + 
    coord_sf(crs = "+proj=aitoff")' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q3 <- "YOUR ANSWER HERE"
# .check_q3()
# .hint_q3()

# # [backend]
# # test the check function
# q3 <- ggplot(data = .shape_file3) + geom_sf() + coord_sf()
# .check_q3()
# q3 <- ggplot(data = .shape_file3) + geom_sf() + coord_sf(crs = "+proj=aitoff")
# .check_q3()
