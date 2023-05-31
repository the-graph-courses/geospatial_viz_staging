# Local data Autograder
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
.scores <- rep(-1, times = 5)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- 
      sf::read_sf(here("data/healthsites/sle_hf.shp"),
                  quiet = TRUE) # write correct answer
    .q1_mistake1 <- 
      sf::read_sf(here("data/boundaries/sle_adm3.shp"),
                  quiet = TRUE) # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!isTRUE(all.equal(class(q1)[1],"sf"))) return(c(value = -1, message = "Your result should be a sf class object."))
        if (isTRUE(all.equal(q1, .q1_mistake1))) return(c(value = 0, message = "You need to replace the file path with the name of the new shapefile to read."))
        if (isTRUE(all.equal(q1, .q1_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q1 <- function(){
  'Use the sf::read_sf() function, AND 
   use the here::here() function to detail the path from the "basic_geospatial_viz" folder.' -> out
  cat(out)
}
# solution of question
.solution_q1 <- function(){
  'sf::read_sf(here("data/healthsites/sle_hf.shp")' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q1 <- "YOUR ANSWER HERE"
# .check_q1()
# .hint_q1()
# #
# # [backend]
# # test the check function
# q1 <-
#   sf::read_sf(here("data/boundaries/sle_adm3.shp"),
#               quiet = TRUE)
# .check_q1()
# q1 <-
#   sf::read_sf(here("data/healthsites/sle_hf.shp"),
#               quiet = TRUE)
# .check_q1()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.NUM_q_nz_features_fields <- 2

# [backend]
.check_q_nz_features_fields <-
  function() {
    .problem_number <<- .NUM_q_nz_features_fields
    
    .q_nz_features_fields_correct <- "A. 16 features and 6 fields" # write correct answer
    .q_nz_features_fields_mistake1 <- "B. 10 features and 6 fields" # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (is.numeric(q_nz_features_fields)) .na(message = "Your result should be character.")
        if (isTRUE(all.equal(q_nz_features_fields, .q_nz_features_fields_mistake1))) .fail(message = "! Carefull, 10 features are the default number of rows showed for an sf object.")
        if (isTRUE(all.equal(q_nz_features_fields, .q_nz_features_fields_correct))) .pass()
        else .fail()
      }
    .run_autograder()
  }

# [backend]
# create one hint per question
.hint_q_nz_features_fields <- function(){
  'Type:
  Check at the header of the sf object' -> out
  cat(out)
}
# solution of question
.solution_q_nz_features_fields <- function(){
  '"A. 16 features and 6 fields"' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q_nz_features_fields <- "YOUR ANSWER HERE"
# .check_q_nz_features_fields()
# .hint_q_nz_features_fields()

# # [backend]
# # test solution
# .solution_q_nz_features_fields()
# # test the check function
# q_nz_features_fields <- 99
# .check_q_nz_features_fields()
# q_nz_features_fields <- "B. 10 features and 6 fields"
# .check_q_nz_features_fields()
# q_nz_features_fields <- "A. 16 features and 6 fields"
# .check_q_nz_features_fields()

# for one more question, use the "check" snippet again!


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.NUM_q_rivers_geom_type <- 3

# [backend]
.check_q_rivers_geom_type <-
  function() {
    .problem_number <<- .NUM_q_rivers_geom_type
    
    .q_rivers_geom_type_correct <- "MULTILINESTRING" # write correct answer
    .q_rivers_geom_type_mistake1 <- "MULTIPOINT" # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (is.numeric(q_rivers_geom_type)) .na(message = "Your result should be character.")
        if (isTRUE(all.equal(q_rivers_geom_type, .q_rivers_geom_type_mistake1))) .fail(message = "! Try to find the Geometry specification in the sf output.")
        if (isTRUE(all.equal(q_rivers_geom_type, .q_rivers_geom_type_correct))) .pass()
        else .fail()
      }
    .run_autograder()
  }

# [backend]
# create one hint per question
.hint_q_rivers_geom_type <- function(){
'Type:
  You can check to the sf header or sf dataframe.' -> out
cat(out)
}
# solution of question
.solution_q_rivers_geom_type <- function(){
'"MULTILINESTRING"' -> out
cat(out)
}

# # [frontend]
# # to paste in lesson
# q_rivers_geom_type <- "YOUR ANSWER HERE"
# .check_q_rivers_geom_type()
# .hint_q_rivers_geom_type()

# # [backend]
# # test solution
# .solution_q_rivers_geom_type()
# # test the check function
# q_rivers_geom_type <- 99
# .check_q_rivers_geom_type()
# q_rivers_geom_type <- "MULTIPOINT"
# .check_q_rivers_geom_type()
# q_rivers_geom_type <- "MULTILINESTRING"
# .check_q_rivers_geom_type()

# for one more question, use the "check" snippet again!

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- "shp" # write correct answer
    .q4_mistake1 <- "dbf" # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!is.character(q4)) return(c(value = -1, message = "Your result should be character."))
        if (isTRUE(all.equal(q4, .q4_mistake1))) return(c(value = 0, message = "! dbf provides the covariates associated with each shape of the spatial data."))
        if (isTRUE(all.equal(q4, .q4_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q4 <- function(){
  'Check the figure that relates the collection of files in a shapefile' -> out
  cat(out)
}
# solution of question
.solution_q4 <- function(){
  'shp' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q4 <- "YOUR ANSWER HERE"
# .check_q4()
# .hint_q4()

# # [backend]
# # test the check function
# q4 <- "dbf"
# .check_q4()
# q4 <- "shp"
# .check_q4()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q5 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.check_q5 <-
  function() {
    .problem_number <<- 5
    
    .q5_correct <- "dbf" # write correct answer
    .q5_mistake1 <- "shx" # optional: highlight common mistake
    
    .autograder <<-
      function(){
        if (!is.character(q5)) return(c(value = -1, message = "Your result should be character."))
        if (isTRUE(all.equal(q5, .q5_mistake1))) return(c(value = 0, message = "! shx provides an index that relates the geometries and attributes of a shapefile."))
        if (isTRUE(all.equal(q5, .q5_correct))) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q5 <- function(){
  'Review the figure that relates the elements in a shapefiles' -> out
  cat(out)
}
# solution of question
.solution_q5 <- function(){
  'dbf' -> out
  cat(out)
}

# # [frontend]
# # to paste in lesson
# q5 <- "YOUR ANSWER HERE"
# .check_q5()
# .hint_q5()

# # [backend]
# # test the check function
# q5 <- "shx"
# .check_q5()
# q5 <- "dbf"
# .check_q5()


