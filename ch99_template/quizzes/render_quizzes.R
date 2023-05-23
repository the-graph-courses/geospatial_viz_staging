# Quiz knitter 
## GRAPH Courses team
## 2022-03-10

#' Create unique quiz copies and export them into  an Excel file for upload to Learndash
#' This script may take a while to run. So I recommend using RStudio's "Source as Local Job" functionality to run it.
#' This way you can continue to work while it runs in the background.

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load(parsermd, rmarkdown, here, fs, writexl, tidyverse, blogdown)
source(here("global/functions/misc_functions.R"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Connect to GDrive ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pacman::p_load(googledrive)
# Setup for uploading images and datasets
if (!drive_has_token()) drive_auth(email = "trainingteam@thegraphnetwork.org")
options(gargle_oauth_email = "trainingteam@thegraphnetwork.org")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Render the quiz ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Just provide the folder to your quiz, the function will read the quiz information 

render_quiz(
  quiz_folder="ch03_intro_to_data_viz/quizzes/ch03_ls04_layers",
  n_quizzes = 3,
  questions_per_quiz =5
) 


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Enter quiz information ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# course_abbrev <- "temp" # abbrevs for our four courses are "idar", "fdar", "err" and "egh"
# quiz_folder <- here::here("ch99_template/quizzes/ch99_ls01_qz01_strat_theory_filter")
# quiz_title <- "99.1.1 Theory quiz: filtering" # User-facing title. CHAPTER_NUM.LESSON_NUM.QUIZNUM Data/Theory quiz: QUIZ_NAME
# quiz_type <- "stratified_theory" # "data" "stratified_theory" or "simple_theory"
# n_quizzes <- 10 # Should default to 30. (We want at least 30 unique quiz samples for each quiz.) Using a lower number here for expedience.

# 
# course_abbrev <- "temp"
# quiz_folder <- here("ch99_template/quizzes/ch99_ls01_qz02_data_ebola")
# quiz_title <- "99.1.2 Data quiz: Ebola in Sierra Leone"
# quiz_type <- "data"
# n_quizzes <- 10

# course_abbrev <- "temp"
# quiz_folder <- here("ch99_template/quizzes/ch99_ls01_qz03_simple_theory_filter_select")
# quiz_title <- "99.1.3 Theory quiz: selecting and filtering"
# quiz_type <- "simple_theory"
# n_quizzes <- 3
# questions_per_quiz <- 3

# process_quiz(
#   course_abbrev = course_abbrev,
#   quiz_type = quiz_type,
#   quiz_folder = quiz_folder,
#   quiz_title = quiz_title,
#   n_quizzes = n_quizzes,
#   questions_per_quiz = ifelse(exists("questions_per_quiz"), questions_per_quiz, NULL)
# ) 

