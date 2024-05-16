# Copies internal staging repo to a repo hosted on GitHub pages.
# Using this just for PDFs for now

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages and functions ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, fs, glue, xfun, parsermd)
pacman::p_load_gh("rstudio/pagedown")

# some tibble print options for the dfs
options(pillar.width = 60) # avoid overflow of tibbles
options(pillar.min_title_chars = 15,
        pillar.max_footer_lines = 2,
        pillar.min_chars = 15)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Render Rmds to regular HTML ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

current_dir <- here::here()

# Manually list the lessons to render. You can use ChatGPT to generate such list
selected_lessons <- 
  c(
    "ls02_thematic_maps.Rmd"
    ,
    "ls03_physical_features.Rmd"
    ,
    "ls04_density_map.Rmd"
  )


# Get full paths of Rmds to render
rmds_to_render <- 
  fs::dir_ls(current_dir, 
             regexp = paste0(selected_lessons, collapse = "|"),
             recurse = T)

# Render documents
for (rmd in rmds_to_render[1:length(rmds_to_render)]) {
  
  p_unload(here)  # unload here each time, so that each directory is set as the root directory on each render
  
  print(paste0("Rendering: \n", rmd, "\n(", 
               which(rmd == rmds_to_render), " of ", 
               length(rmds_to_render), ")"
  ))
  rmarkdown::render(rmd)
  
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Render Rmds to PDF ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for (rmd in rmds_to_render[1:length(rmds_to_render)]) {
  
  # Print progress
  print(paste0("Rendering: \n", rmd, "\n(",
               which(rmd == rmds_to_render), " of ", 
               length(rmds_to_render), ")"
  ))
  
  # duplicate rmd
  duplicate_rmd <- str_replace(rmd, ".Rmd", "-duplicate-for-pagedown.Rmd")
  fs::file_copy(path = rmd, new_path = duplicate_rmd, overwrite = T)
  
  # modify duplicate
  rmd_modified <- 
    read_lines(rmd) %>% 
    # reactable does not work for PDFs. Replace with regular renders
    str_replace_all("render = reactable_5_rows", "render = head_5_rows") %>% 
    str_replace_all("render = reactable_10_rows", "render = head_10_rows")
  # replace here::here with xfun::from_root
  
  write_lines(rmd_modified, file = duplicate_rmd)
  
  
  output_yaml <- here::here("global/style/_output_pagedown.yml")
  
  p_unload(here)  # unload here each time, so that each directory is set as the root directory on each render
  
  # render duplicate to pagedown html
  output_html_paged <- str_replace(rmd, ".Rmd", "-pagedown.html")
  rmarkdown::render(duplicate_rmd, 
                    output_file = output_html_paged, 
                    output_format = "pagedown::html_paged",
                    output_yaml = output_yaml) 
  
  
  # convert pagedown html to a pdf
  output_pdf <- str_replace(rmd, ".Rmd", ".pdf")
  chrome_print(output_html_paged, 
               output = output_pdf, wait = 10, timeout = 60)
  
  # delete duplicate rmd & html
  unlink(duplicate_rmd)
  unlink(output_html_paged)
  
}


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Copy the lessons folder into wp repo  ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p_unload(here)  # unload here so it does not conflict with previous render
from <- here::here()
to <- stringr::str_replace(from, "_staging", "")

folders_to_copy <- tools::file_path_sans_ext(basename(selected_lessons))
folders_to_copy_full_path <- str_c(from, "/", folders_to_copy)
folders_to_copy_new_path <- str_c(to, "/", folders_to_copy)

fs::dir_copy(path = folders_to_copy,
             new_path = folders_to_copy_new_path,
             overwrite =  TRUE)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Delete Rmds with the _TEACHER suffix in target folder ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get full paths of Rmds to render
rmds_to_delete <- 
  fs::dir_ls(folders_to_copy_new_path, 
             regexp = "_TEACHER.Rmd",
             recurse = T)

fs::file_delete(rmds_to_delete)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Zip folders----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 
# ## Zip the components of each folder 
# for (folder in folders_to_copy_new_path) {
#   
#   # get components of each folder
#   folder_components <- fs::dir_ls(folder)
#   
#   # put all components into single zip 
#   zip_file <- str_c(folder, ".zip")
#   
#   # delete zip if it already exists
#   if (fs::file_exists(zip_file)) {
#     fs::file_delete(zip_file)
#   }
#   
#   # zip the components
#   zip::zipr(zip_file, folder_components)
#   
# }

## Zip the components of each folder, excluding HTML and PDF files (we only put those in the public repo so GitHub can render them. Don't actually want students to get em in the zips. Too big, and not particularly useful)
for (folder in folders_to_copy_new_path) {
  
  # get components of each folder
  folder_components <- fs::dir_ls(folder)
  
  # Filter out HTML and PDF files
  files_to_zip <- folder_components[!grepl("\\.html$|\\.pdf$", folder_components)]
  
  # Include only Rmd files that end with CODE_ALONG.Rmd and all other non-Rmd files
  files_to_zip <- files_to_zip[grepl("CODE_ALONG\\.Rmd$", files_to_zip) | !grepl("\\.Rmd$", files_to_zip)]
  
  # put all components into single zip 
  zip_file <- str_c(folder, ".zip")
  
  # delete zip if it already exists
  if (fs::file_exists(zip_file)) {
    fs::file_delete(zip_file)
  }
  
  # zip the filtered components
  zip::zipr(zip_file, files_to_zip)
}





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Delete the PDFs and HTMLs from the source folder ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

selected_lessons_folder <- tools::file_path_sans_ext(basename(selected_lessons))
selected_lessons_full_path <- str_c(from, "/", selected_lessons_folder, selected_lessons)
selected_lessons_pdf <- str_replace(selected_lessons_full_path, ".Rmd", ".pdf")
selected_lessons_html <- str_replace(selected_lessons_full_path, ".Rmd", ".html")
selected_lessons_to_delete <- c(selected_lessons_pdf, selected_lessons_html)

for (file in selected_lessons_to_delete) {
  unlink(file)
}

