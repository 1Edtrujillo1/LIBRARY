
#' @used for JSON_FILES() function 


#(?<=[.])\\w{0,5} bring everything after a point in a string
#(?<=[.])json bring only de word json after a point in a string
#\\.json$ everything that ends with json
#URLS\.\w{0,5} find URLS follow by any character follow by any word with 0 to 5 letter
#^M.*|URLS.*  find worth that start with M and followed by any character OR the word URLS follow by any character
#^(?!^MATHEMATICS.*).*$ give everythin that is not ^MATHEMATICS.*
#REMOVE A LEVEL OF A LIST A <- unlist(JSON_FILES,recursive = FALSE)


#' @used for TOPICS() function 

# #the condition has length > 1 and only the first element will be used
# 
# v <- c("HOLA", "COMO", "ESTAS", "BIEN", "Y", "TU")
# 
# revisar <- function(x){
#   
#   z <- NA
#   #variable take 1 value
#   if(x %in%c("COMO", "BIEN")) z <- "H"
#   else z <- "NO"}
# 
# map_chr(v, revisar)
# 
# revisar <- function(x){
#   
#   z <- NA
#   #variable take more than 1 value
#   if(x %in%c("COMO", "BIEN")) z <- c("H", "D")
#   else z <- c("NO", "s")}
# 
# map(v, revisar)

#######################################################################################################################################

# TOPICS(json_files = JSON_FILES(), topic = "ACTUARIAL MATHEMATICS", subtopic = "RISK ADMINISTRATION", title_author = "Title")
# 
# TOPICS(json_files = JSON_FILES(), topic = "DEMOGRAPHY", title_author = "Author")
# TOPICS(json_files = JSON_FILES(), topic = "WRITING AN ESSAY", title_author = "Author")
# 
# TOPICS(json_files = JSON_FILES(), topic = "ECONOMY", subtopic = "MACROECONOMICS", title_author = "Author")

#######################################################################################################################################

# print(link_title(topic = "ACTUARIAL MATHEMATICS", subtopic = "RISK ADMINISTRATION"))
# print(link_title(topic = "DEMOGRAPHY"))
# print(link_title(topic = "ECONOMY", subtopic = "MACROECONOMICS"))

#######################################################################################################################################

# print(CREATE_LIBRARY(topic = "ACTUARIAL MATHEMATICS", subtopic = "RISK ADMINISTRATION"))
# print(CREATE_LIBRARY(topic = "DEMOGRAPHY"))
# print(CREATE_LIBRARY(topic = "ECONOMY", subtopic = "MACROECONOMICS"))

#######################################################################################################################################

# print(TOPIC_DATASET(topic = "ACTUARIAL MATHEMATICS"))
# print(TOPIC_DATASET(topic = "DEMOGRAPHY"))
# print(TOPIC_DATASET(topic = "ECONOMY"))

#######################################################################################################################################





#STEPS TO CREATE A FUNCTION


#A)

TOPICS <- function(json_files, topic, subtopic, title_author = c("Title", "Author")){
  
  json_files[[topic]][[subtopic]][[glue("{title_author}")]]
  
  
}


#B)


TOPICS <- function(topic, json_files, subtopic = NULL, title_author = c("Title", "Author")){
  
  subject <- NA
  
  if(length(json_files[[topic]]) > 2)  subject <- json_files[[topic]][[subtopic]][[glue("{title_author}")]]
  else if(length(json_files[[topic]]) <= 2) subject <- 3
  
  return(subject)
  
}

map("ACTUARIAL MATHEMATICS", TOPICS, json_files = JSON_FILES(), subtopic = "RISK ADMINISTRATION", title_author = "Title") %>% flatten_chr()
map("ECONOMY", TOPICS, json_files = JSON_FILES(), title_author = "Title") %>% flatten_chr()


#C)

TOPICS <- function(json_files, topic, subtopic = NULL, title_author = c("Title", "Author")){
  
  
  map(topic, function(topic){
    
    subject <- NA
    
    if(length(json_files[[topic]]) > 2)  subject <- json_files[[topic]][[subtopic]][[glue("{title_author}")]]
    
    else if(length(json_files[[topic]]) <= 2) subject <- 3
    
    return(subject)
    
  }) %>% flatten_chr() %>% 
    
    return()
  
}


TOPICS(JSON_FILES(), topic = "ACTUARIAL MATHEMATICS", subtopic = "RISK ADMINISTRATION", title_author = "Author")


TOPICS(json_files = JSON_FILES(), topic = "ECONOMY", title_author = "Author")


#D)

TOPICS <- function(json_files, topic, subtopic = NULL, title_author = c("Title", "Author")){
  
  options(warn = -1)
  
  map(topic, function(topic){
    
    subject <- NA
    
    if(length(json_files[[topic]]) <= 2) {
      
      map(topic, function(topic){
        
        subject <- NA
        
        if(names(json_files[[topic]]) %in% c("Title" , "Author")) subject <- json_files[[topic]][[glue("{title_author}")]]
        
        else subject <-  json_files[[topic]][[subtopic]][[glue("{title_author}")]]
        
        return(subject)}) %>% flatten_chr()
    }
    
    else if(length(json_files[[topic]]) > 2) subject <- json_files[[topic]][[subtopic]][[glue("{title_author}")]]
    
  }) %>% flatten_chr() %>% 
    
    return()
  
}

#
# #length(JSON_FILES[[topic]]) >2
#
# JSON_FILES[[topic]][[subtopic]][[title_author]]
#
# JSON_FILES[["ACTUARIAL MATHEMATICS"]][["RISK ADMINISTRATION"]][["Title"]]
#
# #length(JSON_FILES[[topic]]) <=2
#
# #a) SIN SUBTOPICS
#
# JSON_FILES[[topic]][[title_author]]
#
# JSON_FILES[["DEMOGRAPHY"]][["Title"]]
#
# #b) CON SUBTOPICS
#
# JSON_FILES[[topic]][[subtopic]][[title_author]]
#
# JSON_FILES[["ECONOMY"]][["MACROECONOMICS"]][["Title"]]











