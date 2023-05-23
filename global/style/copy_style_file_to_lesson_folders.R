all_folders <- list.dirs(here::here())
lesson_folders <- all_folders[stringr::str_ends(all_folders, "/lessons")]

file.copy(from = here::here("global/style/style.css"), 
          to = paste0(lesson_folders, "/style.css"), 
          overwrite = T)
