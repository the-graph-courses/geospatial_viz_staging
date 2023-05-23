library(cli)
library(praise)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  FUNCTIONS ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## We will eventually move this out into a package.

.score_grid <- function(){
  score_icons <- list()
  for (i in (1:length(.scores))) {
    if(.scores[i] == -1 ) {
      score_icons[i] <- bg_black(style_bold(col_white(paste0(" ", i, " "))))
    } else if (.scores[i] == 0)  { 
      score_icons[i] <- bg_red(style_bold(col_white(paste0(" ", i, " "))))
    } else if (.scores[i] == 1) { 
      score_icons[i] <- bg_cyan(style_bold(col_white(paste0(" ", i, " "))))
    }   
  }
  cat("\n", paste(score_icons), sep = " ")
}

## ~~~~ Spinner ----
.spinner <- make_spinner("hearts")
.spinner_function <- function() {
  map(1:20, function(x) { .spinner$spin(); Sys.sleep(0.05) })
  .spinner$finish()
}

## ~~~~ Print scores ----
.score_print <- function(){
  num_questions <- length(.scores)
  num_correct <- sum(.scores == 1) 
  num_answered <- sum(.scores != -1)
  score_pct <- ifelse(num_answered == 0, 
                      NA, 
                      round(100 * num_correct/num_questions, digits = 3) ) 
  
  .score_grid()
  cat("\n")
  cli_text("{symbol$checkbox_off} {style_bold('Answered:')} {num_answered} of {num_questions}") 
  cli_text("{bg_cyan(col_white(paste0( symbol$tick ) ))} {style_bold('Correct:')} {num_correct} of {num_questions}") 
  cli_text("{col_magenta(paste0( symbol$arrow_right ) )} {style_bold('Score so far:')} {score_pct} %") 
  
  if (!is.na(score_pct) && score_pct >= 90){
    cat(praise("${Exclamation}! ${EXCLAMATION}!-${EXCLAMATION}! This is ${adverb} ${adjective}!\n")) 
    ansi_with_hidden_cursor(.spinner_function())
  }
  
}

## ~~~~ Apply autograder (internal function) ----

.apply_autograder <- 
  function(){
    .scores[.problem_number] <<- tryCatch({.autograder()[["value"]]}, error = function(e)-1)     ## assign Correct (1), Incorrect (0) or NA (-1) value to the appropriate position in .scores vector, or default (-1) in case of error
    tryCatch({cat(.autograder()[["message"]])}, error = function(e) cat("Wrong. Please try again."))    ## print .autograder message or default message in case of error
    .score_grid()
  }

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  DATA ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.yaounde <- read_csv(here::here('ch02_data_cleaning_pipeline/data/yaounde_data.csv'))

.yao_mini <-
  .yaounde 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 8)   # Put total number of questions as `times` argument

