# 1.0 Load Package --------------------------------------------------------
rm(list = ls())

library(purrr)
map(c("data.table", "stringr", "dplyr", 
      "udeploy", "DT", "config", 
      "mongolite", "jsonlite"), require, 
    character.only = TRUE)

# 2.0 Setting Up Mongo DB parameters --------------------------------------
Sys.setenv(R_CONFIG_ACTIVE = "db_library")
config <- config::get(file = "config.yml")

# 3.0 Library Dataset -----------------------------------------------------
#' @description
#' The intention of this function is to obtain a dataset from the path of 
#' a folder that contains pdf files.
#' @return
#' Dataset with the title and author of the pdf files in a specific folder.
df_library <- function(element){
  element <- element %>% str_remove_all(pattern = ".pdf") %>% 
    str_split(pattern = ",")
  
  map(1:2, ~ map_chr(element, .x)) %>% 
    set_names("TITLE", "AUTHOR") %>% as.data.table() %>% return()
}

#' @description
#' The intention of this function is to obtain the final dataset with the topic,
#' subtopic, author, & title (in link format). of all the pdf files in  
#' *general_path*.
#' *If exclude_elements != NULL* will exclute some folders when reading the 
#' *general_path*.
#' @return
#' *if import_links = FALSE* will export a csv file of the dataset with the 
#' topic, subtopic, author, & title with the intention that we add the links
#' for each row (each book). With *link_path* as the new path of the csv file 
#' that we want to see on our computer.
#' *if import_links = TRUE* will create the same previous dataset in the global 
#' environment but with the title in link format (as we import the *link_path* 
#' csv file with the new links)
dataset_library <- function(general_path, exclude_elements = NULL,
                            link_path, import_links = FALSE){
  
  general_objects <- list.files(general_path)
  
  if(isFALSE(is.null(exclude_elements))){
    general_objects <- 
      str_subset(string = general_objects,
                 pattern = udeploy::obtain_regex(pattern = udeploy::obtain_regex(pattern = exclude_elements,
                                                                                 return_regex = "or"),
                                                 return_regex = "not_contains_pattern"))
  }
  
  element_object <- map(general_objects, function(i){
    
    particular_objects_path <- str_glue("{general_path}/{i}")
    particular_objects <- list.files(particular_objects_path)
    
    element_object <- tryCatch({
      if(particular_objects %>% 
         str_detect(pattern = ".pdf") %>% any() %>% isFALSE()){
        
        element_object <- map(particular_objects, function(j){
          list.files(str_glue("{particular_objects_path}/{j}")) %>% 
            df_library()
        }) %>% set_names(particular_objects) %>% rbindlist(idcol = "SUBTOPIC") 
      }
      element_object
    }, error = function(e) 
      particular_objects %>% df_library() %>% .[,SUBTOPIC:= i]) %>% 
      .[,TOPIC:= i]  
  }) %>% rbindlist(use.names = TRUE) %>% 
    .[,names(.):=lapply(.SD, str_replace_all, c("_" = " "))] %>% 
    udeploy::clean_df() %>% 
    .[, SUBTOPIC :=as.factor(SUBTOPIC)] 
  
  if(import_links){
    message("dataset_library is in your Global Environment")
    dataset_library <<- list(
      element_object,
      fread(link_path) %>% 
        .[,c("TOPIC", "SUBTOPIC"):=lapply(.SD, as.factor), .SDcols = c("TOPIC", "SUBTOPIC")]) %>% 
      udeploy::iterative_merge(key = names(element_object)) %>% 
      .[,':='(TITLE = str_glue('<a target="_blank" href="{.[,LINK]}','">{.[,TITLE]}</a>'),
              LINK = NULL)] %>% 
      .[,.SD, by = "SUBTOPIC"] %>% 
      .[,.SD, by = "TOPIC"]
  }else{
    message(str_glue("exported on {link_path}"))
    write.csv(x = element_object, file = link_path, row.names = FALSE)
  }
}
# Not excluding elements at the reading, and exporting dataset to create csv link
# dataset_library(general_path = "C:/Users/actje/Dropbox/LIBRARY",
#                 exclude_elements = NULL,
#                 link_path = "C:/Users/actje/Dropbox/LIBRARY/links.csv",
#                 import_links = FALSE)

# Excluding elements at the reading, and exporting dataset to create csv link (correct in this case)
# dataset_library(general_path = "C:/Users/actje/Dropbox/LIBRARY",
#                 exclude_elements = c("links.csv","ADITIONAL"),
#                 link_path = "C:/Users/actje/Dropbox/LIBRARY/links.csv",
#                 import_links = FALSE)

# Excluding elements at the reading, and importing csv links dataset to create final df  (correct in this case) **
# dataset_library(general_path = "C:/Users/actje/Dropbox/LIBRARY",
#                 exclude_elements = c("links.csv","ADITIONAL"),
#                 link_path = "C:/Users/actje/Dropbox/LIBRARY/links.csv",
#                 import_links = TRUE)

# Push Library Dataset ----------------------------------------------------
# udeploy::mongo_manipulation(
#   mongo_choice = "push",
#   push_record = dataset_library
# )

# 5.0 Pull Library Dataset ------------------------------------------------
udeploy::mongo_manipulation(mongo_choice = "pull")



